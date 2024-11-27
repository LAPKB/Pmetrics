mod build;
mod executor;
mod settings;
mod simulation;

use extendr_api::prelude::*;
use pmcore::prelude::data::read_pmetrics;
use simulation::SimulationRow;
use std::process::Command;

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
    let rows = executor::simulate(
        model_path.into(),
        subjects.first().unwrap(),
        &spp.to_vec(),
        0,
    );
    rows.into_dataframe().unwrap()
}

/// Simulates all subjects in the data set using the model at the given path.
///@export
#[extendr]
fn simulate_all(
    data_path: &str,
    model_path: &str,
    theta: RMatrix<f64>,
) -> Dataframe<SimulationRow> {
    validate_paths(data_path, model_path);
    let theta = parse_theta(theta);
    let data = read_pmetrics(data_path).expect("Failed to parse data");
    let subjects = data.get_subjects();
    let mut rows = Vec::new();
    for (i, spp) in theta.iter().enumerate() {
        for subject in subjects.iter() {
            rows.append(&mut executor::simulate(model_path.into(), subject, spp, i));
        }
    }
    rows.into_dataframe().unwrap()
}

///@export
#[extendr]
pub fn fit(model_path: &str, data: &str, params: List, output_path: &str) {
    validate_paths(data, model_path);
    executor::fit(model_path.into(), data.into(), params, output_path.into());
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
fn compile_model(model_path: &str, output_path: &str, params: Strings) {
    let params: Vec<String> = params.iter().map(|x| x.to_string()).collect();
    build::compile(model_path.into(), Some(output_path.into()), params.to_vec());
}

/// Dummy function to cache compilation artifacts.
///@export
#[extendr]
fn dummy_compile() -> String {
    let build_path = build::dummy_compile().unwrap();
    build_path
}

///@export
#[extendr]
fn is_cargo_installed() -> bool {
    Command::new("cargo").arg("--version").output().is_ok()
}

///@export
#[extendr]
fn model_parameters(model_path: &str) -> Vec<String> {
    executor::model_parameters(model_path.into())
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
}

// To generate the exported function in R, run the following command:
// rextendr::document()
// Optional: reload Pmetrics
// devtools::load_all()
