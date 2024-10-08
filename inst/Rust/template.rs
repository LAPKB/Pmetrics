#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
use pmcore::prelude::*;

fn main() -> Result<()> {
    let eq = equation::ODE::new(
        |x, p, t, dx, rateiv, cov| {
            fetch_params!(p, </params>);
            fetch_cov!(cov, t, </covs>);
            </constant>
            </seq>
            </eqn>
        },
        |p| {
            fetch_params!(p, </params>);
            lag! {</lag>}
        },
        |p| {
            fetch_params!(p, </params>);
            fa! {</fa>}
        },
        |p, t, cov, x| {
            fetch_params!(p, </params>);
            fetch_cov!(cov, t, </covs>);
            </constant>
            </init>
        },
        |x, p, t, cov, y| {
            fetch_params!(p, </params>);
            fetch_cov!(cov, t, </covs>);
            </constant>
            </seq>
            </out_eqs>
        },
        (</neqs>, </nouteqs>),
    );
    let settings = settings::read("config.toml").unwrap();
    let data = data::read_pmetrics("gendata.csv").unwrap();
    let _result = fit(eq, data, settings);
}
