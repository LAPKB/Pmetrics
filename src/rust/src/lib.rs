mod simulation;

use std::f32::consts::E;

use extendr_api::prelude::*;
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
    reprintln!("data_path: {}", data_path);
    reprintln!("model_path: {}", model_path);
    reprintln!("spp: {:?}", spp);
    let v = vec![
        SimulationRow::new("id1", 1, 1, 1.0, 1.0, 1, 1.0, 1),
        SimulationRow::new("id2", 1, 1, 1.0, 1.0, 1, 1.0, 1),
    ];
    v.into_dataframe().unwrap()
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
