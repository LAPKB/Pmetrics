mod execute;
mod simulation;

use extendr_api::prelude::*;
use pmcore::prelude::data::read_pmetrics;
use simulation::SimulationRow;

/// Return string `"Hello rust!"` to R.
/// @export
#[extendr]
fn hello_world() -> &'static str {
    "Hello rust!"
}

/// Receives a path to a CSV file and returns true if the file exists.
/// @export
#[extendr]
fn file_exists(path: &str) -> bool {
    std::path::Path::new(path).exists()
}

///@export
#[extendr]
fn simulate(data_path: &str, model_path: &str, spp: &[f64]) -> Dataframe<SimulationRow> {
    if !std::path::Path::new(data_path).exists() {
        panic!("Data path does not exist: {}", data_path);
    }
    let data = read_pmetrics(data_path).expect("Failed to parse data");
    let subjects = data.get_subjects();

    if !std::path::Path::new(model_path).exists() {
        panic!("Model path does not exist: {}", model_path);
    }
    let rows = execute::execute(model_path.into(), subjects.first().unwrap(), &spp.to_vec());
    rows.into_dataframe().unwrap()
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod Pmetrics;
    fn hello_world;
    fn file_exists;
    fn simulate;
}
