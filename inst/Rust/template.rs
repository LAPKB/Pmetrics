#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
use pmcore::prelude::*;

// Constants for the absolute and relative tolerance for the dynamic steps used for solving the ODEs
const ATOL: f64 = 1e-4;
const RTOL: f64 = 1e-4;

// Define the state vector, which must be equal to the number of compartments in the model
// These are re-exported from the `nalgebra`-crate by `ode_solvers`, see https://github.com/srenevey/ode-solvers?tab=readme-ov-file#type-alias-definition
// In brief, for up to 6 compartments, use VectorN<f64>, N being the number of compartments.
// For more than 6 compartments, use `nalgebra::SVector<f64, N>`, where N is the number of compartments.
type State = SVector<f64, </neqs>>;
// Time uses f64 precision
type Time = f64;

// This is the main structure for the model
// It holds the parameters (defined in config.toml), the scenarios (i.e. datafile), the (possible) infusions and the covariates if any
#[derive(Debug, Clone)]
struct Model {
    params: HashMap<String, f64>,
    _scenario: Scenario,
    infusions: Vec<Infusion>,
    cov: Option<HashMap<String, CovLine>>,
}
// This is a helper function to get the parameter value by name
impl Model {
    pub fn get_param(&self, str: &str) -> f64 {
        *self.params.get(str).unwrap()
    }
}

impl ode_solvers::System<Time, State> for Model {
    /// The system function, defining the ordinary differential equations (ODEs) to be solved
    fn system(&self, t: Time, x: & State, dx: &mut State) {
        // Get the parameters, covariates and secondary equations from the model
        </self_parameter_alias>
        let cov = self.cov.as_ref().unwrap();
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

impl<'a> Predict<'a> for Ode {
    type Model = Model;
    type State = State;
    // This function is used initialize the system by setting parameter names and initial empty structs.
    fn initial_system(&self, parameters: &Vec<f64>, scenario: Scenario) -> (Self::Model,Scenario) {
        let mut params = HashMap::new();
        // params.insert("ke".to_string(), params[0].clone());
        // params.insert("v".to_string(), params[1].clone());
        </parameter_definition>
        let system = Model {
            params,
            _scenario: scenario.clone(),//TODO remove
            infusions: vec![],
            cov: None,
        };
        </parameter_alias>
        (system,
        // scenario.reorder_with_lag(vec![(0.0, 1)]))
        </lags>
        )
    }
    // This function is used to get the output from the model, defined by the output equations (outeq) supplied by the user
    fn get_output(&self, t: f64, x: &Self::State, system: &Self::Model, outeq: usize) -> f64 {
        // let v = system.get_param("v");
        // match outeq {
        //     1 => x[0] / v,
        //     _ => panic!("Invalid output equation"),
        // }
        </parameter_alias>
        let cov = system.cov.as_ref().unwrap();
        </cov>
        </seq>
        </out_eqs>
    }
    // This function is used to initialize the comparments
    //TODO: Handle non-zero initial conditions
    fn initial_state(&self) -> State {
        State::default()
    }
   // Add any possible infusions
   fn add_infusion(&self, system: &mut Self::Model, infusion: Infusion) {
        system.infusions.push(infusion);
    }
    // Add any possible covariates
    fn add_covs(&self, system: &mut Self::Model, cov: Option<HashMap<String, CovLine>>) {
        system.cov = cov;
    }
    // Add any possible doses
    fn add_dose(&self, state: &mut Self::State, dose: f64, compartment: usize) {
        state[compartment] += dose;
    }
    // Perform a "step" of the model, i.e. solve the ODEs from the current time to the next time
    // In the next step, we use this result as the initial state
    fn state_step(&self, x: &mut Self::State, system: &Self::Model, time: f64, next_time: f64) {
        if time > next_time {
            panic!("time error")
        } else if time == next_time {
            return;
        }
        let mut stepper = Dopri5::new(system.clone(), time, next_time, 1e-3, *x, RTOL, ATOL);
        let _res = stepper.integrate();
        let y = stepper.y_out();
        *x = *y.last().unwrap();
    }
}

fn main() -> Result<()> {
    start(
        Engine::new(Ode {}),
        "config.toml".to_string(),
    )?;
    Ok(())
}
