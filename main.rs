equation::ODE::new(
            |x, p, t, dx, rateiv, cov| {
    fetch_cov!(cov, t, wt, crcl, agecat);
    fetch_params!(p, ke0, kcp, kpc, v0);
    let ke = ke0 * wt.powf(-(0.25)) * crcl;
    dx[0] = (rateiv[0]) - ((ke + kcp) * x[0]) + kpc * x[1];
    dx[1] = (kcp * x[0]) - (kpc * x[1]);
},
            |_p| lag! {},
            |_p| fa! {},
            |_p, _t, _cov, _x| { },
            |x, p, t, cov, y| {
    fetch_cov!(cov, t, wt, crcl, agecat);
    fetch_params!(p, ke0, kcp, kpc, v0);
    let v = v0 * wt;
    y[0] = x[0] / v;
},
            (2, 1),
        )
