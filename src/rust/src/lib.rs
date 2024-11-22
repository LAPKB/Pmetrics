mod compile;
mod execute;
mod simulation;

use extendr_api::prelude::*;
use pmcore::prelude::data::read_pmetrics;
use simulation::SimulationRow;

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

///@export
#[extendr]
fn compile_model(model_path: &str, output_path: &str, params: Strings) {
    let params: Vec<String> = params.iter().map(|x| x.to_string()).collect();
    compile::compile(model_path.into(), Some(output_path.into()), params.to_vec());
}

//@export
#[extendr]
fn dummy_compile() {
    compile::dummy_compile().unwrap();
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod Pmetrics;
    fn simulate;
    fn compile_model;
    fn dummy_compile;
}
