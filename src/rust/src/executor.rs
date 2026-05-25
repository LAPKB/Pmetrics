use crate::settings::settings;
use extendr_api::List;

use pmcore::prelude::{simulator::SubjectPredictions, Predictions, *};

use crate::simulation::SimulationRow;

pub(crate) fn model_parameters<E: EquationMetadataSource>(equation: &E) -> Result<Vec<String>> {
    let metadata = equation
        .equation_metadata()
        .ok_or_else(|| anyhow::anyhow!("runtime model metadata is required"))?;

    Ok(metadata
        .parameters()
        .iter()
        .map(|parameter| parameter.name().to_string())
        .collect())
}

pub(crate) fn simulate<E>(
    equation: &E,
    subject: &Subject,
    parameters: &Parameters,
    spp_index: usize,
) -> Result<Vec<SimulationRow>>
where
    E: Equation + EquationMetadataSource,
{
    let metadata = equation
        .equation_metadata()
        .ok_or_else(|| anyhow::anyhow!("runtime model metadata is required"))?;

    if metadata.parameters().len() != parameters.as_slice().len() {
        return Err(anyhow::anyhow!(
            "Support point has {} values but model expects {} parameters",
            parameters.as_slice().len(),
            metadata.parameters().len()
        ));
    }

    let predictions: SubjectPredictions = equation
        .estimate_predictions(subject, parameters)?
        .get_predictions()
        .into();
    Ok(SimulationRow::from_subject_predictions(
        predictions,
        subject.id(),
        spp_index,
    ))
}

pub(crate) fn fit<E>(
    equation: E,
    data: Data,
    params: List,
    output_path: &str,
) -> std::result::Result<(), anyhow::Error>
where
    E: Equation + Clone + Send + 'static + EquationMetadataSource,
{
    let _result = settings(params, equation, data, output_path)?.fit()?;
    Ok(())
}
