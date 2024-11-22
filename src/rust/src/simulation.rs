use extendr_api::prelude::*;

//look at https://extendr.github.io/extendr/extendr_api/derive.IntoDataFrameRow.html

#[derive(Debug, IntoDataFrameRow)]
pub struct SimulationRow {
    id: String,
    spp_index: usize,
    block: usize,
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
        block: usize,
        time: f64,
        out: f64,
        outeq: usize,
        state: f64,
        state_index: usize,
    ) -> Self {
        Self {
            id: id.to_string(),
            spp_index,
            block,
            time,
            out,
            outeq,
            state,
            state_index,
        }
    }
}
