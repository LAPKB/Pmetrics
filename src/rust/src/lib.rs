mod executor;
mod logs;
mod settings;
mod simulation;

use mimalloc::MiMalloc;

/// Use mimalloc as the global allocator for improved allocation performance
/// across Windows, macOS, and Linux.
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

use anyhow::Result;
use extendr_api::prelude::*;
use pmcore::prelude::data::{read_pmetrics, Data};
use simulation::SimulationRow;
use tracing_subscriber::layer::SubscriberExt;

use crate::logs::RFormatLayer;

fn validate_data_path(data_path: &str) -> Result<()> {
    if !std::path::Path::new(data_path).exists() {
        return Err(anyhow::anyhow!("Data path does not exist: {}", data_path));
    }
    Ok(())
}

fn read_data(data_path: &str) -> Result<Data> {
    read_pmetrics(data_path).map_err(|err| anyhow::format_err!("Failed to parse data: {}", err))
}

/// Simulates the first subject in the data set using the given model.
/// @param data_path Path to the data file.
/// @param model_source Model definition written in the pharmsol DSL.
/// @param spp One support point as a numeric vector.
/// @return Simulation results.
/// @export
#[extendr]
fn simulate_one(
    data_path: &str,
    model_source: &str,
    spp: &[f64],
    solver: Nullable<String>,
) -> Result<Dataframe<SimulationRow>> {
    validate_data_path(data_path)?;
    let data = read_data(data_path)?;
    let subjects = data.subjects();
    let first_subject = subjects
        .first()
        .ok_or_else(|| anyhow::anyhow!("Data set contains no subjects"))?;

    let solver = solver.into_option();
    let model = executor::compile_dsl(model_source, solver.as_deref())?;
    let rows = executor::simulate_model(&model, first_subject, spp, 0)?;

    rows.into_dataframe()
        .map_err(|e| anyhow::anyhow!("Failed to build data frame: {}", e))
}

/// Simulates all subjects in the data set using the given model.
/// @param data_path Path to the data file.
/// @param model_source Model definition written in the pharmsol DSL.
/// @param theta Data frame of support points.
/// @return Simulation results.
/// @export
#[extendr]
fn simulate_all(
    data_path: &str,
    model_source: &str,
    theta: RMatrix<f64>,
    solver: Nullable<String>,
) -> Result<Dataframe<SimulationRow>> {
    use rayon::prelude::*;

    validate_data_path(data_path)?;
    let theta = parse_theta(theta)?;
    let data = read_data(data_path)?;
    let subjects = data.subjects();
    let solver = solver.into_option();
    let model = executor::compile_dsl(model_source, solver.as_deref())?;

    let rows: Vec<_> = theta
        .par_iter()
        .enumerate()
        .map(|(i, spp)| {
            subjects
                .par_iter()
                .map(|subject| executor::simulate_model(&model, subject, spp, i))
                .collect::<Result<Vec<_>>>()
                .map(|v| v.into_iter().flatten().collect::<Vec<_>>())
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();

    rows.into_dataframe()
        .map_err(|e| anyhow::anyhow!("Failed to build data frame: {}", e))
}

/// Fits the given model to the data using the provided settings.
/// @param model_source Model definition written in the pharmsol DSL.
/// @param data Path to the data file.
/// @param params List of fitting parameters.
/// @param output_path Path to save the fitting results.
/// @return Result of the fitting process.
/// @export
#[extendr]
pub fn fit(
    model_source: &str,
    data: &str,
    params: List,
    output_path: &str,
    solver: Nullable<String>,
) -> Result<()> {
    RFormatLayer::reset_global_timer();
    setup_logs()?;
    println!("Initializing model fit...");
    validate_data_path(data)?;
    let data = read_data(data)?;
    let solver = solver.into_option();
    executor::fit(
        model_source,
        data,
        params,
        output_path.into(),
        solver.as_deref(),
    )?;
    Ok(())
}

fn parse_theta(matrix: RMatrix<f64>) -> Result<Vec<Vec<f64>>> {
    let nspp = matrix.nrows();
    let ndim = matrix.ncols();
    let real_vector = matrix
        .as_real_vector()
        .ok_or_else(|| anyhow::anyhow!("theta matrix must contain real values"))?;
    let mut theta = vec![vec![0.0; ndim]; nspp];
    for i in 0..nspp {
        for j in 0..ndim {
            theta[i][j] = real_vector[i + j * nspp];
        }
    }
    Ok(theta)
}

/// Retrieves the model parameters from the given model.
/// @param model_source Model definition written in the pharmsol DSL.
/// @return List of model parameters.
/// @export
#[extendr]
fn model_parameters(model_source: &str) -> Result<Vec<String>> {
    executor::model_parameters(model_source)
}

/// Initialize the tracing subscriber with the custom R formatter
/// @keywords internal
/// @export
#[extendr]
fn setup_logs() -> anyhow::Result<()> {
    use tracing::Level;
    use tracing_subscriber::filter::LevelFilter;

    // Create a subscriber with our custom layer using the global timer
    // Filter to show INFO and above (INFO, WARN, ERROR) so cycle logs are visible
    let subscriber = tracing_subscriber::registry()
        .with(RFormatLayer::new())
        .with(LevelFilter::from_level(Level::INFO));

    // Set as global default - this will fail if already set, which is fine
    // We just ignore the error
    let _ = tracing::subscriber::set_global_default(subscriber);

    Ok(())
}

extendr_module! {
    mod Pmetrics;
    fn simulate_one;
    fn simulate_all;
    fn fit;
    fn model_parameters;
    fn setup_logs;
}

// To generate the exported function in R, run the following command:
// rextendr::document()
// Optional: reload Pmetrics
// devtools::load_all()
