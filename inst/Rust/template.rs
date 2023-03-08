#![allow(dead_code)]
#![allow(unused_variables)]

use ode_solvers::*;
use np_core::prelude::*;
use eyre::Result;

struct Model<'a>{
    // ka: f64,
    // ke: f64,
    // _v: f64,
    // lag: f64,
    </struct_params>
    scenario: &'a Scenario
}

type State = Vector</neqs><f64>;
type Time = f64;

impl ode_solvers::System<State> for Model<'_> {
    fn system(&self, t: Time, x: &mut State, dx: &mut State) {
        // let ka = self.ka;
        // let ke = self.ke;
        </parameter_alias>
        // let t = t - self.lag;
        </lag>
        ///////////////////// USER DEFINED ///////////////
        </diff_eq>
        // dy[0] = -ka*y[0];
        // dy[1] = ka*y[0] - ke*y[1];
        //////////////// END USER DEFINED ////////////////
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
        // let system = Model {ka: params[0], ke: params[1], _v: params[2], lag: params[3], scenario};
        let system = Model {</model_params> scenario};
        // let y0 = State::new(0.0, 0.0);
        let y0 = State::new(</init>);
        let mut stepper = Rk4::new(system, tspan[0], y0, tspan[1],0.1);
        let _res = stepper.integrate();
        let x = stepper.x_out().to_vec();
        let y = stepper.y_out();
        let mut yout: Vec<Vec<f64>> = vec![];
        </v_alias>
        // let v = params[2];
        </out_eqs>
        // ///////////////////// ONE PER OUTPUT EQUATION ///////////////
        // let y0: Vec<f64> = y.iter().map(|y| {
        // ///////////////////// USER DEFINED ///////////////
        //     y[1]/v
        // //////////////// END USER DEFINED ////////////////
        // } ).collect();
        // yout.push(y0);
        // //////////////// END ONE PER OUTPUT EQUATION ////////////////
        (x, yout)    
    }
} 

fn main()-> Result<()>{
    // start(
    //     Engine::new(Sim{}),
    //     vec![(0.1,0.9),(0.001,0.1),(30.0,120.0),(0.0,4.0)],
    //     "examples/two_eq_lag.toml".to_string(),
    //     (0.1,0.25,-0.001,0.0)
    // )?;
    start(
        Engine::new(Sim{}),
        vec![</ranges>],
        "config.toml".to_string(),
        </c>
    )?;
    Ok(())
}
