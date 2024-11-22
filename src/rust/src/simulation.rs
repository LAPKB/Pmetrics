use extendr_api::prelude::*;
use pmcore::prelude::{
    simulator::{Prediction, SubjectPredictions},
    Predictions,
};

//look at https://extendr.github.io/extendr/extendr_api/derive.IntoDataFrameRow.html

#[derive(Debug, IntoDataFrameRow)]
pub struct SimulationRow {
    id: String,
    spp_index: usize,
    time: f64,
    out: f64,
    outeq: usize,
    state: f64,
    state_index: usize,
}

impl SimulationRow {
    pub fn new(
        id: &str,
        spp_index: usize,
        time: f64,
        out: f64,
        outeq: usize,
        state: f64,
        state_index: usize,
    ) -> Self {
        Self {
            id: id.to_string(),
            spp_index,
            time,
            out,
            outeq,
            state,
            state_index,
        }
    }
}

impl SimulationRow {
    fn from_prediction(prediction: &Prediction, spp_index: usize, id: &str) -> Vec<Self> {
        let mut rows = Vec::new();
        for (i, state) in prediction.state().iter().enumerate() {
            rows.push(Self::new(
                id,
                spp_index,
                prediction.time(),
                prediction.prediction(),
                prediction.outeq(),
                *state,
                i,
            ));
        }
        rows
    }

    pub fn from_subject_predictions(
        subject_predictions: SubjectPredictions,
        spp_index: usize,
        id: &str,
    ) -> Vec<Self> {
        let mut rows = Vec::new();
        for prediction in subject_predictions.get_predictions().iter() {
            rows.extend(Self::from_prediction(prediction, spp_index, id));
        }
        rows
    }
}
