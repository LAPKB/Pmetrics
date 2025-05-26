use extendr_api::{Conversions, List};
use pmcore::prelude::*;
use std::collections::HashMap;

pub(crate) fn settings(
    settings: List,
    params: &Vec<String>,
    output_path: &str,
) -> Result<Settings> {
    let settings = settings.into_hashmap();
    dbg!(&settings);
    let ranges = settings.get("ranges").unwrap().as_list().unwrap();
    let ranges = robj_to_hashmap(ranges);
    let parameters = parse_parameters(ranges, params)?;
    let algorithm = settings
        .get("algorithm")
        .unwrap()
        .as_str()
        .unwrap()
        .to_string();
    let algorithm = match algorithm.as_str().to_lowercase().as_str() {
        "npag" => pmcore::prelude::Algorithm::NPAG,
        "npod" => pmcore::prelude::Algorithm::NPOD,
        _ => return Err(anyhow::anyhow!("Algorithm {} not supported", algorithm)),
    };
    let gamlam = settings
        .get("gamlam")
        .unwrap()
        .as_real()
        .ok_or_else(|| anyhow::anyhow!("No gamlam provided"))?;

    let poly = settings
        .get("error_coefficients")
        .unwrap()
        .as_real_vector()
        .ok_or_else(|| anyhow::anyhow!("No error coefficients provided"))?;
    let max_cycles = settings
        .get("max_cycles")
        .unwrap()
        .as_real()
        .unwrap_or(100.0) as usize;
    let ind_points = settings
        .get("ind_points")
        .unwrap()
        .as_real()
        .unwrap_or(2028.0) as usize;
    let seed = settings.get("seed").unwrap().as_real().unwrap_or(22.0) as usize;

    let prior = settings.get("prior").unwrap().as_str().unwrap().to_string();
    let prior = match prior.as_str() {
        "sobol" => pmcore::prelude::Prior::sobol(ind_points, seed),

        _ => return Err(anyhow::anyhow!("Prior {} not supported", prior)),
    };

    let mut settings = Settings::builder()
        .set_algorithm(algorithm)
        .set_parameters(parameters)
        .set_error_model(
            pmcore::prelude::ErrorModel::Additive,
            gamlam,
            (poly[0], poly[1], poly[2], poly[3]),
        )
        .build();

    settings.set_cycles(max_cycles);
    settings.set_prior(prior);
    settings.set_output_path(output_path.to_string());
    settings.set_write_logs(true);
    settings.write()?;
    settings.initialize_logs()?;
    Ok(settings)
}

fn robj_to_hashmap(list: List) -> HashMap<String, (f64, f64)> {
    let mut map: HashMap<String, (f64, f64)> = HashMap::new();
    list.iter().for_each(|x| {
        let name = x.0.to_owned();
        let ranges = x.1.as_real_slice().unwrap();
        map.insert(name, (ranges[0], ranges[1]));
    });
    map
}

fn parse_parameters(
    ranges: HashMap<String, (f64, f64)>,
    params: &Vec<String>,
) -> Result<Parameters> {
    let mut parameters = Parameters::new();
    for param in params.iter() {
        let (min, max) = match ranges.get(param) {
            Some(range) => range,
            None => {
                // eprintln!("Parameter {} not found in ranges {:?}", param, &ranges);
                return Err(anyhow::anyhow!(
                    "Parameter {} not found in ranges {:?}",
                    param,
                    &ranges
                ));
            }
        };
        parameters = parameters.add(param.to_string(), *min, *max, false);
    }

    Ok(parameters)
}
