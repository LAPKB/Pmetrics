use extendr_api::prelude::*;
use pmcore::prelude::simulator::Prediction;

//look at https://extendr.github.io/extendr/extendr_api/derive.IntoDataFrameRow.html

#[derive(Debug, IntoDataFrameRow)]
pub struct SimulationRow {
    id: String,
    time: f64,
    out: f64,
    outeq: usize,
    state: f64,
    state_index: usize,
    spp_index: usize,
}

impl SimulationRow {
    pub fn new(
        id: &str,
        time: f64,
        out: f64,
        outeq: usize,
        state: f64,
        state_index: usize,
        spp_index: usize,
    ) -> Self {
        Self {
            id: id.to_string(),
            time,
            out,
            outeq,
            state,
            state_index,
            spp_index,
        }
    }
}

impl SimulationRow {
    fn from_prediction(
        prediction: &Prediction,
        outputs: &[String],
        id: &str,
        spp_index: usize,
    ) -> anyhow::Result<Vec<Self>> {
        // Predictions carry the output label; report its 0-based position in the
        // model's declared outputs, as before.
        let label = prediction.outeq().as_str();
        let outeq = outputs
            .iter()
            .position(|output| output == label)
            .ok_or_else(|| {
                anyhow::anyhow!("prediction output '{}' is not declared by the model", label)
            })?;
        let mut rows = Vec::new();
        for (i, state) in prediction.state().iter().enumerate() {
            rows.push(Self::new(
                id,
                prediction.time(),
                prediction.prediction(),
                outeq,
                *state,
                i,
                spp_index,
            ));
        }
        Ok(rows)
    }

    pub fn from_predictions(
        predictions: Vec<Prediction>,
        outputs: &[String],
        id: &str,
        spp_index: usize,
    ) -> anyhow::Result<Vec<Self>> {
        let mut rows = Vec::new();
        for prediction in predictions.iter() {
            rows.extend(Self::from_prediction(prediction, outputs, id, spp_index)?);
        }
        Ok(rows)
    }
}
