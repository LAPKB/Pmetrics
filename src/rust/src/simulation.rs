use extendr_api::prelude::*;
use pmcore::prelude::{
    simulator::{Prediction, SubjectPredictions},
    Predictions,
};

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
    fn from_prediction(prediction: &Prediction, id: &str, spp_index: usize) -> Vec<Self> {
        let mut rows = Vec::new();
        for (i, state) in prediction.state().iter().enumerate() {
            rows.push(Self::new(
                id,
                prediction.time(),
                prediction.prediction(),
                prediction.outeq(),
                *state,
                i,
                spp_index,
            ));
        }
        rows
    }

    pub fn from_subject_predictions(
        subject_predictions: SubjectPredictions,
        id: &str,
        spp_index: usize,
    ) -> Vec<Self> {
        let mut rows = Vec::new();
        for prediction in subject_predictions.get_predictions().iter() {
            rows.extend(Self::from_prediction(prediction, id, spp_index));
        }
        rows
    }
}
