#![allow(dead_code)]
#![allow(unused_variables)]
use eyre::Result;
use np_core::prelude::{
    datafile::{CovLine, Infusion},
    *,
};
use ode_solvers::*;
use std::collections::HashMap;
#[derive(Debug, Clone)]
struct Model<'a> {
    </struct_params>
    _scenario: &'a Scenario,
    infusions: Vec<Infusion>,
    cov: Option<&'a HashMap<String, CovLine>>,
}

type State = SVector<f64, </neqs>>;
type Time = f64;

impl ode_solvers::System<State> for Model<'_> {
    fn system(&mut self, t: Time, x: &mut State, dx: &mut State) {
        </parameter_alias>
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
    fn predict(&self, params: Vec<f64>, scenario: &Scenario) -> Vec<f64> {
        let mut system = Model {
            </model_params>
            _scenario: scenario,
            infusions: vec![],
            cov: None
        };
        </lag>
        let mut yout = vec![];
        let mut x = State::new(</init>);
        let mut index: usize = 0;
        for block in &scenario.blocks {
            system.cov = Some(&block.covs);
            for event in &block.events {
                let mut event_time = event.time;
                if event.evid == 1 {
                    if event.dur.unwrap_or(0.0) > 0.0 {
                        //infusion
                        system.infusions.push(Infusion {
                            time: event.time + lag,
                            dur: event.dur.unwrap(),
                            amount: event.dose.unwrap(),
                            compartment: event.input.unwrap() - 1,
                        });
                    } else {
                        //dose
                        if lag > 0.0 {
                            event_time = event.time + lag;
                            let mut stepper =
                                Rk4::new(system.clone(), event.time, x, event_time, 0.1);
                            let _int = stepper.integrate();
                            let y = stepper.y_out();
                            x = *y.last().unwrap();
                        }
                        x[event.input.unwrap() - 1] += event.dose.unwrap();
                    }
                } else if event.evid == 0 {
                    //obs
                    </v_alias>
                    </out_eqs>
                    // yout.push(x[event.outeq.unwrap() - 1] / params[1]);
                }
                if let Some(next_time) = scenario.times.get(index + 1) {
                    let mut stepper = Rk4::new(system.clone(), event_time, x, *next_time, 0.1);
                    let _res = stepper.integrate();
                    let y = stepper.y_out();
                    x = *y.last().unwrap();
                    index += 1;
                }
            }
        }
        yout
    }
}

fn main() -> Result<()> {
    start(
        Engine::new(Ode {}),
        "config.toml".to_string(),
    )?;
    Ok(())
}
