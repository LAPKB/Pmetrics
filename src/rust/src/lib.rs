mod compiler;
mod executor;
mod simulation;

use std::process::Command;

use extendr_api::prelude::*;
use pmcore::prelude::data::read_pmetrics;
use simulation::SimulationRow;

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
fn simulate_one(data_path: &str, model_path: &str, spp: &[f64]) -> Dataframe<SimulationRow> {
    validate_paths(data_path, model_path);
    let data = read_pmetrics(data_path).expect("Failed to parse data");
    let subjects = data.get_subjects();
    let rows = executor::execute(model_path.into(), subjects.first().unwrap(), &spp.to_vec());
    rows.into_dataframe().unwrap()
}

/// Simulates all subjects in the data set using the model at the given path.
///@export
#[extendr]
fn simulate_all(data_path: &str, model_path: &str, spp: &[f64]) -> Dataframe<SimulationRow> {
    validate_paths(data_path, model_path);
    let data = read_pmetrics(data_path).expect("Failed to parse data");
    let subjects = data.get_subjects();
    let mut rows = Vec::new();
    for subject in subjects.iter() {
        rows.append(&mut executor::execute(
            model_path.into(),
            subject,
            &spp.to_vec(),
        ));
    }
    rows.into_dataframe().unwrap()
}

/// Compiles the text representation of a model into a binary file.
///@export
#[extendr]
fn compile_model(model_path: &str, output_path: &str, params: Strings) {
    let params: Vec<String> = params.iter().map(|x| x.to_string()).collect();
    compiler::compile(model_path.into(), Some(output_path.into()), params.to_vec());
}

/// Dummy function to cache compilation artifacts.
///@export
#[extendr]
fn dummy_compile() -> String {
    let build_path = compiler::dummy_compile().unwrap();
    build_path
}

///@export
#[extendr]
fn is_cargo_installed() -> bool {
    Command::new("cargo").arg("--version").output().is_ok()
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
}

// To generate the exported function in R, run the following command:
// rextendr::document()
// Optional: reload Pmetrics
// devtools::load_all()
