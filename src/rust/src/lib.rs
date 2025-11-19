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
use tracing_subscriber::layer::SubscriberExt;

use crate::logs::RFormatLayer;

fn validate_paths(data_path: &str, model_path: &str) {
    if !std::path::Path::new(data_path).exists() {
        panic!("Data path does not exist: {}", data_path);
    }
    if !std::path::Path::new(model_path).exists() {
        panic!("Model path does not exist: {}", model_path);
    }
}

/// Simulates the first subject in the data set using the model at the given path.
/// @param data_path Path to the data file.
/// @param model_path Path to the compiled model file.
/// @param spp One support point as a numeric vector with probabiltity.
/// @param kind Kind of model, which can either be "ODE" or "Analytical".
/// @return Simulation results.
///@export
#[extendr]
fn simulate_one(
    data_path: &str,
    model_path: &str,
    spp: &[f64],
    kind: &str,
) -> Result<Dataframe<SimulationRow>> {
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
/// @param data_path Path to the data file.
/// @param model_path Path to the compiled model file.
/// @param theta Data frame of support points.
/// @param kind Kind of model, which can either be "ODE" or "Analytical".
/// @return Simulation results.
/// @export
#[extendr]
fn simulate_all(
    data_path: &str,
    model_path: &str,
    theta: RMatrix<f64>,
    kind: &str,
) -> Dataframe<SimulationRow> {
    use rayon::prelude::*;

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


/// Fits the model at the given path to the data at the given path using the provided parameters.
/// @param model_path Path to the compiled model file.
/// @param data Path to the data file.
/// @param params List of fitting parameters.
/// @param output_path Path to save the fitting results.
/// @param kind Kind of model, which can either be "ODE" or "Analytical".
/// @return Result of the fitting process.
/// @export
#[extendr]
pub fn fit(
    model_path: &str,
    data: &str,
    params: List,
    output_path: &str,
    kind: &str,
) -> Result<()> {
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
/// @param model_path Path to the model file.
/// @param output_path Path to save the compiled model.
/// @param params List of model parameters.
/// @param template_path Path to the template directory.
/// @param kind Kind of model, which can either be "ODE" or "Analytical".
/// @return Result of the compilation process.
/// @export
#[extendr]
fn compile_model(
    model_path: &str,
    output_path: &str,
    params: Strings,
    template_path: &str,
    kind: &str,
) -> Result<()> {
    let params: Vec<String> = params.iter().map(|x| x.to_string()).collect();
    let model_txt = std::fs::read_to_string(model_path).expect("Failed to read model file");
    let template_path = std::path::PathBuf::from(template_path);
    match kind {
        "ode" => build::compile::<ODE>(
            model_txt,
            Some(output_path.into()),
            params.to_vec(),
            template_path,
            |_key, val| {
                print!("{}", val);
            },
        )?,
        "analytical" => build::compile::<Analytical>(
            model_txt,
            Some(output_path.into()),
            params.to_vec(),
            template_path,
            |_key, val| {
                print!("{}", val);
            },
        )?,
        err => return Err(anyhow::format_err!("{} is not a supported model type", err)),
    };

    Ok(())
}

/// Dummy function to cache compilation artifacts.
/// @param template_path Path to the template directory.
/// @return Path to the build directory.
/// @export
#[extendr]
fn dummy_compile(template_path: &str) -> Result<String> {
    dbg!("inside dummy_compile");
    let template_path = std::path::PathBuf::from(template_path);
    let build_path = build::dummy_compile(template_path, |_key, val| {
        print!("{}", val);
    })?;
    Ok(build_path)
}
/// Checks if Cargo is installed on the system.
/// @return TRUE if Cargo is installed, FALSE otherwise.
/// @export
#[extendr]
fn is_cargo_installed() -> bool {
    println!("inside is_cargo_installed");
    dbg!("dbg is_cargo_installed");
    Command::new("cargo").arg("--version").output().is_ok()
}

/// Retrieves the model parameters from the compiled model at the given path.
/// @param model_path Path to the compiled model file.
/// @param kind Kind of model, which can either be "ODE" or "Analytical".
/// @return List of model parameters.
/// @export
#[extendr]
fn model_parameters(model_path: &str, kind: &str) -> Result<Vec<String>> {
    match kind {
        "ode" => Ok(executor::model_parameters::<ODE>(model_path.into())),
        "analytical" => Ok(executor::model_parameters::<ODE>(model_path.into())),
        err => Err(anyhow::format_err!("{} is not a supported model type", err)),
    }
}

/// Retrieves the temporary path used for building models.
/// @return Temporary build path.
/// @export
#[extendr]
fn temporary_path() -> String {
    build::temp_path().to_string_lossy().to_string()
}

/// Initialize the tracing subscriber with the custom R formatter
/// @keywords internal
/// @export
#[extendr]
fn setup_logs() -> anyhow::Result<()> {
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
    fn temporary_path;
    fn setup_logs;
}

// To generate the exported function in R, run the following command:
// rextendr::document()
// Optional: reload Pmetrics
// devtools::load_all()