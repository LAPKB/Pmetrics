use crate::settings::{settings, RunConfig};
use crate::simulation::SimulationRow;

use extendr_api::List;
use pharmsol::dsl::{
    compile_module_source_to_runtime, CompiledRuntimeModel, RuntimeCompilationTarget,
};
use pmcore::prelude::{simulator::Prediction, *};

use std::path::PathBuf;

/// Parse and JIT-compile a model written in the pharmsol DSL.
///
/// This replaces the old workflow of compiling a Rust source file into a shared
/// library with `cargo` and loading it at runtime. The model text is compiled
/// in-process, so no Rust toolchain is required on the user's machine.
pub(crate) fn compile_dsl(source: &str) -> Result<CompiledRuntimeModel> {
    compile_module_source_to_runtime(source, None, RuntimeCompilationTarget::Jit, |_, _| {})
        .map_err(|e| anyhow::anyhow!("Failed to compile model: {e}"))
}

/// The ordered list of parameter names declared by the model.
fn param_names(model: &CompiledRuntimeModel) -> Vec<String> {
    model
        .metadata()
        .parameters()
        .iter()
        .map(|p| p.name().to_string())
        .collect()
}

pub(crate) fn model_parameters(source: &str) -> Result<Vec<String>> {
    Ok(param_names(&compile_dsl(source)?))
}

/// Simulate a subject at a support point using an already-compiled model.
pub(crate) fn simulate_model(
    model: &CompiledRuntimeModel,
    subject: &Subject,
    support_point: &[f64],
    spp_index: usize,
) -> Result<Vec<SimulationRow>> {
    let nparams = model.metadata().parameters().len();
    if nparams != support_point.len() {
        return Err(anyhow::anyhow!(
            "Support point has {} values but model expects {} parameters",
            support_point.len(),
            nparams
        ));
    }

    let predictions: Vec<Prediction> = match model {
        CompiledRuntimeModel::Ode(eq) => eq
            .estimate_predictions_dense(subject, support_point)?
            .get_predictions(),
        CompiledRuntimeModel::Analytical(eq) => eq
            .estimate_predictions_dense(subject, support_point)?
            .get_predictions(),
        CompiledRuntimeModel::Sde(_) => {
            return Err(anyhow::anyhow!(
                "SDE models are not supported for simulation"
            ))
        }
    };

    Ok(SimulationRow::from_predictions(
        predictions,
        subject.id(),
        spp_index,
    ))
}

/// Fit a model (given as DSL source) to the data and write the output artifacts.
pub(crate) fn fit(
    source: &str,
    data: Data,
    params: List,
    output_path: PathBuf,
) -> std::result::Result<(), anyhow::Error> {
    let model = compile_dsl(source)?;
    let names = param_names(&model);
    let output_path_str = output_path
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("Output path contains invalid UTF-8: {:?}", output_path))?;
    let config = settings(params, &names, output_path_str)?;

    match model {
        CompiledRuntimeModel::Ode(eq) => run_fit(eq, data, config),
        CompiledRuntimeModel::Analytical(eq) => run_fit(eq, data, config),
        CompiledRuntimeModel::Sde(_) => {
            Err(anyhow::anyhow!("SDE models are not supported for fitting"))
        }
    }
}

fn run_fit<E>(eq: E, data: Data, config: RunConfig) -> Result<()>
where
    E: Equation + EquationMetadataSource + Send + 'static,
{
    let result = EstimationProblem::nonparametric(eq, data, config.prior, config.error_models)?
        .fit_with(config.algorithm)?;
    result.write_outputs(&config.output_path, config.idelta, config.tad)?;
    Ok(())
}
