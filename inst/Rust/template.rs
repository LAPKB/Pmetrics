#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
use pmcore::prelude::{simulator::Equation, *};

fn main() -> Result<()> {
    let eq = Equation::new_ode(
        |x, p, t, dx, rateiv, cov| {
            fetch_params!(p, </params>);
            fetch_cov!(cov, t, </covs>);
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
        |_p, _t, _cov, _x| {</init>},
        |x, p, t, cov, y| {
            fetch_params!(p, </params>);
            fetch_cov!(cov, t, </covs>);
            </seq>
            </out_eqs>
        },
        (</neqs>, </nouteqs>),
    );
    let _result = start(eq, "config.toml".to_string())?;
    Ok(())
}
