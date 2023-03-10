#![allow(dead_code)]
#![allow(unused_variables)]

use ode_solvers::*;
use np_core::prelude::*;
use eyre::Result;

struct Model<'a>{
    </struct_params>
    scenario: &'a Scenario
}

type State = Vector</neqs><f64>;
type Time = f64;

impl ode_solvers::System<State> for Model<'_> {
    fn system(&self, t: Time, x: &mut State, dx: &mut State) {
        </parameter_alias>
        let mut rateiv = [0.0, 0.0];
        for infusion in &self.scenario.infusions{
            if t >= infusion.time && t <= (infusion.dur + infusion.time) {
                rateiv[infusion.compartment] += infusion.amount / infusion.dur;
            }
        }
        </lag>
        </diff_eq>

        for dose in &self.scenario.doses{
            if (t-dose.time).abs() < 1.0e-4 {
                x[dose.compartment] += dose.dose;
            }
        }
        </seq>
    }
}
struct Sim{}
impl Simulate for Sim{
    fn simulate(&self, params: Vec<f64>, tspan:[f64;2], scenario: &Scenario) -> (Vec<f64>, Vec<Vec<f64>>) {
        let system = Model {</model_params> scenario};
        let y0 = State::new(</init>);
        let mut stepper = Rk4::new(system, tspan[0], y0, tspan[1],0.1);
        let _res = stepper.integrate();
        let x = stepper.x_out().to_vec();
        let y = stepper.y_out();
        let mut yout: Vec<Vec<f64>> = vec![];
        </v_alias>
        </out_eqs>
        (x, yout)    
    }
} 

fn main()-> Result<()>{
    start(
        Engine::new(Sim{}),
        vec![</ranges>],
        "config.toml".to_string(),
        </c>
    )?;
    Ok(())
}
