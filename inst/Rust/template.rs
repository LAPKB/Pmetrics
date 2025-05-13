equation::ODE::new(
        |x, p, t, dx, rateiv, cov| {
            fetch_params!(p, </params>);
            fetch_cov!(cov, t, </covs>);
            </constant>
            </sec>
            //</tem>
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
            </sec>
            </init>
        },
        |x, p, t, cov, y| {
            fetch_params!(p, </params>);
            fetch_cov!(cov, t, </covs>);
            </constant>
            </sec>
            </out_eqs>
        },
        (</neqs>, </nouteqs>),
    )