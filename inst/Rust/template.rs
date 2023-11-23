#![allow(dead_code)]
#![allow(unused_variables)]
use std::collections::HashMap;

use eyre::Result;
use npcore::prelude::{
    datafile,
    datafile::{CovLine, Infusion, Scenario},
    predict::{Engine, Predict},
    settings, start, start_with_data,
};
use ode_solvers::*;

const ATOL: f64 = 1e-4;
const RTOL: f64 = 1e-4;

type State = SVector<f64, </neqs>>;
type Time = f64;

#[derive(Debug, Clone)]
struct Model {
    params: HashMap<String, f64>,
    _scenario: Scenario,
    infusions: Vec<Infusion>,
    cov: Option<HashMap<String, CovLine>>,
}
impl Model {
    pub fn get_param(&self, str: &str) -> f64 {
        *self.params.get(str).unwrap()
    }
}

impl ode_solvers::System<State> for Model<'_> {
    fn system(&mut self, t: Time, x: &mut State, dx: &mut State) {
        </parameter_alias>
        </cov>
        </seq>
        let mut rateiv = [0.0];//TODO: hardcoded
        for infusion in &self.infusions {
            if t >= infusion.time && t <= (infusion.dur + infusion.time) {
                rateiv[infusion.compartment] = infusion.amount / infusion.dur;
            }
        }
        </eqn>
    }
}

#[derive(Debug, Clone)]
struct Ode {}

impl Predict for Ode {
    type Model = Model;
    type State = State;
    fn initial_system(&self, params: &Vec<f64>, scenario: Scenario) -> Self::Model {
        //let params = HashMap::from([("ke".to_string(), params[0]), ("v".to_string(), params[1])]);
        </parameter_definition>
        Model {
            params,
            _scenario: scenario,
            infusions: vec![],
            cov: None,
        }
    }
    fn get_output(&self, x: &Self::State, system: &Self::Model, outeq: usize) -> f64 {
        // let v = system.get_param("v");
        // match outeq {
        //     1 => x[0] / v,
        //     _ => panic!("Invalid output equation"),
        // }
        </v_alias>
        </cov_out>
        </seq>
        </out_eqs>
    }
    fn initial_state(&self) -> State {
        State::default()
    }
    fn add_infusion(&self, mut system: Self::Model, infusion: Infusion) -> Model {
        system.infusions.push(infusion);
        system
    }
    fn add_covs(&self, mut system: Self::Model, cov: Option<HashMap<String, CovLine>>) -> Model {
        system.cov = cov;
        system
    }
    fn add_dose(&self, mut state: Self::State, dose: f64, compartment: usize) -> Self::State {
        state[compartment] += dose;
        state
    }
    fn state_step(
        &self,
        mut x: Self::State,
        system: Self::Model,
        time: f64,
        next_time: f64,
    ) -> State {
        if time == next_time {
            return x;
        }
        let mut stepper = Dopri5::new(system, time, next_time, 1e-3, x, RTOL, ATOL);
        let _res = stepper.integrate();
        let y = stepper.y_out();
        x = *y.last().unwrap();
        x
    }
}

fn main() -> Result<()> {
    start(
        Engine::new(Ode {}),
        "config.toml".to_string(),
    )?;
    Ok(())
}
