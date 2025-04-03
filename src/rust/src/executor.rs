use crate::settings::settings;
use extendr_api::List;
 
use pmcore::prelude::{pharmsol::exa::load::load_ode, *};
 
use std::path::PathBuf;
 
use crate::simulation::SimulationRow;
 
pub(crate) fn model_parameters(model_path: PathBuf) -> Vec<String> {
    let (_lib, (_ode, meta)) = unsafe { load_ode(model_path) };
    meta.get_params().clone()
}
 
pub(crate) fn simulate(
    model_path: PathBuf,
    subject: &Subject,
    support_point: &Vec<f64>,
    spp_index: usize,
) -> Vec<SimulationRow> {
    let (_lib, (ode, meta)) = unsafe { load_ode(model_path) };
    assert!(meta.get_params().len() == support_point.len());
    SimulationRow::from_subject_predictions(
        ode.estimate_predictions(subject, support_point),
        subject.id(),
        spp_index,
    )
}
 
pub(crate) fn fit(
    model_path: PathBuf,
    data: PathBuf,
    params: List,
    output_path: PathBuf,
) -> std::result::Result<(), anyhow::Error> {
    let (_lib, (eq, meta)) = unsafe { load_ode(model_path) };
    let settings = settings(params, meta.get_params(), output_path.to_str().unwrap())?;
    let data = data::read_pmetrics(data.to_str().unwrap()).expect("Failed to read data");
    let mut algorithm = dispatch_algorithm(settings, eq, data).unwrap();
    let result = algorithm.fit().unwrap();
    result.write_outputs().unwrap();
    Ok(())
}
 
 