// mod build;
mod executor;
mod logs;
mod settings;
mod simulation;

use anyhow::Result;
use extendr_api::prelude::*;
use pmcore::prelude::{data::read_pmetrics, pharmsol::exa::build, Analytical, ODE};
use simulation::SimulationRow;
use std::process::Command;
use std::sync::Once;
use tracing_subscriber::layer::SubscriberExt;

use crate::logs::RFormatLayer;

static PANIC_HOOK_INIT: Once = Once::new();

/// Initialize the global panic hook to prevent RStudio crashes
/// This captures panics from both main thread and rayon worker threads
fn init_panic_hook() {
    PANIC_HOOK_INIT.call_once(|| {
        // Set up the main panic hook
        let default_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |panic_info| {
            // Extract panic information
            let location = panic_info
                .location()
                .map(|l| format!("{}:{}:{}", l.file(), l.line(), l.column()))
                .unwrap_or_else(|| "unknown location".to_string());

            let message = if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
                s.clone()
            } else {
                "unknown panic payload".to_string()
            };

            // Log to R console using extendr's rprintln! macro
            rprintln!("\n╔═══════════════════════════════════════════════════════════════╗");
            rprintln!("║ RUST PANIC CAUGHT                                             ║");
            rprintln!("╠═══════════════════════════════════════════════════════════════╣");
            rprintln!("║ Location: {:<54} ║", location);
            rprintln!("║ Message:  {:<54} ║", message);
            rprintln!("╚═══════════════════════════════════════════════════════════════╝\n");

            // Also call the default hook for additional debugging if needed
            // (this typically prints to stderr)
            default_hook(panic_info);
        }));

        // Configure rayon to use our panic handler
        rayon::ThreadPoolBuilder::new()
            .panic_handler(|_| {
                // Rayon panic handler doesn't get PanicInfo, just the panic value
                // We need to handle it differently
                rprintln!("\n╔═══════════════════════════════════════════════════════════════╗");
                rprintln!("║ RUST PANIC IN RAYON THREAD                                    ║");
                rprintln!("╠═══════════════════════════════════════════════════════════════╣");
                rprintln!("║ A panic occurred in a parallel worker thread.                 ║");
                rprintln!("║ This may indicate an issue with parallel processing.          ║");
                rprintln!("╚═══════════════════════════════════════════════════════════════╝\n");
            })
            .build_global()
            .unwrap_or_else(|e| {
                rprintln!("Warning: Failed to initialize rayon thread pool: {}", e);
            });
    });
}

fn validate_paths(data_path: &str, model_path: &str) {
    if !std::path::Path::new(data_path).exists() {
        panic!("Data path does not exist: {}", data_path);
    }
    if !std::path::Path::new(model_path).exists() {
        panic!("Model path does not exist: {}", model_path);
    }
}

/// Simulates the first subject in the data set using the model at the given path.
///@export
#[extendr]
fn simulate_one(
    data_path: &str,
    model_path: &str,
    spp: &[f64],
    kind: &str,
) -> Result<Dataframe<SimulationRow>> {
    init_panic_hook();
    validate_paths(data_path, model_path);
    let data = read_pmetrics(data_path).expect("Failed to parse data");
    let subjects = data.subjects();
    let rows = match kind {
        "ode" => executor::simulate::<ODE>(
            model_path.into(),
            subjects.first().unwrap(),
            &spp.to_vec(),
            0,
        )?,
        "analytical" => executor::simulate::<Analytical>(
            model_path.into(),
            subjects.first().unwrap(),
            &spp.to_vec(),
            0,
        )?,
        _ => {
            return Err(anyhow::format_err!(
                "{} is not a supported model type",
                kind
            ));
        }
    };
    Ok(rows.into_dataframe().unwrap())
}

/// Simulates all subjects in the data set using the model at the given path.
///@export
#[extendr]
fn simulate_all(
    data_path: &str,
    model_path: &str,
    theta: RMatrix<f64>,
    kind: &str,
) -> Dataframe<SimulationRow> {
    use rayon::prelude::*;

    init_panic_hook();
    validate_paths(data_path, model_path);
    let theta = parse_theta(theta);
    let data = read_pmetrics(data_path).expect("Failed to parse data");
    let subjects = data.subjects();

    let rows: Vec<_> = match kind {
        "ode" => theta
            .par_iter()
            .enumerate()
            .flat_map(|(i, spp)| {
                subjects
                    .par_iter()
                    .flat_map(|subject| {
                        executor::simulate::<ODE>(model_path.into(), subject, spp, i).unwrap()
                    })
                    .collect::<Vec<_>>()
            })
            .collect(),
        "analytical" => theta
            .par_iter()
            .enumerate()
            .flat_map(|(i, spp)| {
                subjects
                    .par_iter()
                    .flat_map(|subject| {
                        executor::simulate::<Analytical>(model_path.into(), subject, spp, i)
                            .unwrap()
                    })
                    .collect::<Vec<_>>()
            })
            .collect(),
        _ => {
            panic!("{} is not a supported model type", kind);
        }
    };

    rows.into_dataframe().unwrap()
}

///@export
#[extendr]

pub fn fit(
    model_path: &str,
    data: &str,
    params: List,
    output_path: &str,
    kind: &str,
) -> Result<()> {
    init_panic_hook();
    RFormatLayer::reset_global_timer();
    setup_logs()?;
    println!("Initializing model fit...");
    validate_paths(data, model_path);
    match kind {
        "ode" => {
            match executor::fit::<ODE>(model_path.into(), data.into(), params, output_path.into()) {
                Ok(_) => {}
                Err(err) => {
                    println!("{}", err)
                }
            }
        }
        "analytical" => {
            match executor::fit::<Analytical>(
                model_path.into(),
                data.into(),
                params,
                output_path.into(),
            ) {
                Ok(_) => {}
                Err(err) => {
                    println!("{}", err)
                }
            }
        }
        err => return Err(anyhow::format_err!("{} is not a supported model type", err)),
    };
    Ok(())
}

fn parse_theta(matrix: RMatrix<f64>) -> Vec<Vec<f64>> {
    let nspp = matrix.nrows();
    let ndim = matrix.ncols();
    let real_vector = matrix.as_real_vector().unwrap();
    let mut theta = vec![vec![0.0; ndim]; nspp];
    for i in 0..nspp {
        for j in 0..ndim {
            theta[i][j] = real_vector[i + j * nspp];
        }
    }
    theta
}

/// Compiles the text representation of a model into a binary file.
///@export
#[extendr]
fn compile_model(model_path: &str, output_path: &str, params: Strings, kind: &str) -> Result<()> {
    init_panic_hook();
    let params: Vec<String> = params.iter().map(|x| x.to_string()).collect();
    let model_txt = std::fs::read_to_string(model_path).expect("Failed to read model file");
    match kind {
        "ode" => build::compile::<ODE>(
            model_txt,
            Some(output_path.into()),
            params.to_vec(),
            |_key, val| {
                print!("{}", val);
            },
        )?,
        "analytical" => build::compile::<Analytical>(
            model_txt,
            Some(output_path.into()),
            params.to_vec(),
            |_key, val| {
                print!("{}", val);
            },
        )?,
        err => return Err(anyhow::format_err!("{} is not a supported model type", err)),
    };

    Ok(())
}

/// Dummy function to cache compilation artifacts.
///@export
#[extendr]
fn dummy_compile() -> Result<String> {
    let build_path = build::dummy_compile(|_key, val| {
        print!("{}", val);
    })?;
    Ok(build_path)
}

///@export
#[extendr]
fn is_cargo_installed() -> bool {
    println!("inside is_cargo_installed");
    dbg!("dbg is_cargo_installed");
    Command::new("cargo").arg("--version").output().is_ok()
}

///@export
#[extendr]
fn model_parameters(model_path: &str, kind: &str) -> Result<Vec<String>> {
    match kind {
        "ode" => Ok(executor::model_parameters::<ODE>(model_path.into())),
        "analytical" => Ok(executor::model_parameters::<ODE>(model_path.into())),
        err => Err(anyhow::format_err!("{} is not a supported model type", err)),
    }
}

//@export
#[extendr]
fn template_path() -> String {
    build::template_path()
}

//@export
#[extendr]
fn clear_build() {
    build::clear_build();
}

/// Initialize the tracing subscriber with the custom R formatter
/// @export
#[extendr]
pub fn setup_logs() -> anyhow::Result<()> {
    use tracing::Level;
    use tracing_subscriber::filter::LevelFilter;

    // Create a subscriber with our custom layer using the global timer
    // Filter to show only INFO and above (INFO, WARN, ERROR)
    let subscriber = tracing_subscriber::registry()
        .with(RFormatLayer::new())
        .with(LevelFilter::from_level(Level::INFO));

    // Set as global default - this will fail if already set, which is fine
    // We just ignore the error
    let _ = tracing::subscriber::set_global_default(subscriber);

    Ok(())
}

/// Initialize the panic hook to prevent RStudio crashes
/// This should be called early in package initialization
/// @export
#[extendr]
pub fn init_panic_handler() {
    init_panic_hook();
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod Pmetrics;
    fn simulate_one;
    fn simulate_all;
    fn compile_model;
    fn dummy_compile;
    fn is_cargo_installed;
    fn fit;
    fn model_parameters;
    fn template_path;
    fn clear_build;
    fn setup_logs;
    fn init_panic_handler;
}

// To generate the exported function in R, run the following command:
// rextendr::document()
// Optional: reload Pmetrics
// devtools::load_all()
