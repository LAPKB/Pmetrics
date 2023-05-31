#![allow(dead_code)]
#![allow(unused_variables)]

use eyre::Result;
use np_core::prelude::{
    datafile::{Dose, Infusion},
    *,
};
use ode_solvers::*;

#[derive(Debug, Clone)]
struct Model<'a> {
    </struct_params>
    _scenario: &'a Scenario,
    infusions: Vec<Infusion>,
    dose: Option<Dose>,
}

type State = SVector<f64, </neqs>>;
type Time = f64;

impl ode_solvers::System<State> for Model<'_> {
    fn system(&mut self, t: Time, x: &mut State, dx: &mut State) {
        </parameter_alias>

        let lag = 0.0;

        let mut rateiv = [0.0];
        for infusion in &self.infusions {
            if t >= infusion.time && t <= (infusion.dur + infusion.time) {
                rateiv[infusion.compartment] = infusion.amount / infusion.dur;
            }
        }
        </lag>
        </diff_eq>
        if let Some(dose) = &self.dose {
            if t >= dose.time + lag {
                dx[dose.compartment] += dose.amount;
                self.dose = None;
            }
        }
        </seq>
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
            dose: None,
        };
        let lag = 0.0;
        let mut yout = vec![];
        let mut x = State::new(0.0);
        let mut index: usize = 0;
        for block in &scenario.blocks {
            //if no code is needed here, remove the blocks from the codebase
            //It seems that blocks is an abstractions we're going to end up not using
            for event in &block.events {
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
                        system.dose = Some(Dose {
                            time: event.time + lag,
                            amount: event.dose.unwrap(),
                            compartment: event.input.unwrap() - 1,
                        });
                    }
                } else if event.evid == 0 {
                    //obs
                    </v_alias>
                    </out_eqs>
                    // yout.push(x[event.outeq.unwrap() - 1] / params[1]);
                }
                if let Some(next_time) = scenario.times.get(index + 1) {
                    let mut stepper = Rk4::new(system.clone(), event.time, x, *next_time, 0.1);
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
        </c>,
    )?;
    Ok(())
}
