use crate::settings::settings;
use extendr_api::List;
use libloading::{Library, Symbol};
use logger::setup_log;
use pmcore::prelude::*;
use std::path::PathBuf;

use crate::simulation::SimulationRow;

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

pub(crate) fn fit(model_path: PathBuf, data: PathBuf, params: List, output_path: PathBuf) {
    let lib = unsafe { Library::new(model_path).expect("Failed to load library") };
    let (eq, meta) = unsafe { load_ode(&lib) };
    // dbg!(&meta);
    let settings = settings(params, meta.get_params(), output_path.to_str().unwrap());
    setup_log(&settings).unwrap();
    let data = data::read_pmetrics(data.to_str().unwrap()).expect("Failed to read data");
    let mut algorithm = dispatch_algorithm(settings, eq, data).unwrap();
    let result = algorithm.fit().unwrap();
    result.write_outputs().unwrap();
}
