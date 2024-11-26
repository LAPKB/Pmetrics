use libloading::{Library, Symbol};
use logger::setup_log;
use pmcore::algorithms::routines::settings::*;
use pmcore::prelude::*;
use settings::Settings;
use std::path::PathBuf;
use toml::Table;

use crate::simulation::SimulationRow;
use indexmap::IndexMap;
unsafe fn load_ode(lib: &Library) -> (ODE, Meta) {
    let create_eqn: Symbol<unsafe extern "C" fn() -> *mut std::ffi::c_void> =
        unsafe { lib.get(b"create_eqn_ptr").expect("Failed to load symbol") };
    let eqn_ptr = unsafe { create_eqn() };

    let create_meta: Symbol<unsafe extern "C" fn() -> *mut std::ffi::c_void> =
        unsafe { lib.get(b"metadata_ptr").expect("Failed to load symbol") };
    let meta_ptr = unsafe { create_meta() };

    unsafe {
        (
            (&*(eqn_ptr as *mut equation::ODE)).clone(),
            (&*(meta_ptr as *mut equation::Meta)).clone(),
        )
    }
}

pub(crate) fn simulate(
    model_path: PathBuf,
    subject: &Subject,
    support_point: &Vec<f64>,
) -> Vec<SimulationRow> {
    let lib = unsafe { Library::new(model_path).expect("Failed to load library") };
    let (eq, meta) = unsafe { load_ode(&lib) };
    assert!(meta.get_params().len() == support_point.len());
    SimulationRow::from_subject_predictions(
        eq.estimate_predictions(subject, support_point),
        subject.id(),
    )
}

pub(crate) fn fit(model_path: PathBuf, data: PathBuf) {
    let lib = unsafe { Library::new(model_path).expect("Failed to load library") };
    let (eq, meta) = unsafe { load_ode(&lib) };
    dbg!(meta);
    let settings = settings(IndexMap::new());
    setup_log(&settings).unwrap();
    let data = data::read_pmetrics(data.to_str().unwrap()).expect("Failed to read data");
    let mut algorithm = dispatch_algorithm(settings, eq, data).unwrap();
    let result = algorithm.fit().unwrap();
    result.write_outputs().unwrap();
}

fn create_random_parameters(params: IndexMap<String, (f64, f64)>) -> Table {
    params
        .into_iter()
        .map(|(k, (v1, v2))| {
            (
                k,
                toml::Value::Array(vec![toml::Value::Float(v1), toml::Value::Float(v2)]),
            )
        })
        .collect()
}

fn settings(params: IndexMap<String, (f64, f64)>) -> Settings {
    let mut settings = Settings::default();
    // let mut params = IndexMap::new();
    // params.insert("CL0".to_string(), (0.2, 10.0));
    // params.insert("V0".to_string(), (1.0, 20.0));
    // params.insert("Vp0".to_string(), (1.0, 25.0));
    // params.insert("Q0".to_string(), (0.01, 55.0));
    settings.random = Random {
        parameters: create_random_parameters(params),
    };
    settings
}
