use anyhow::bail;
use extendr_api::{Conversions, List};
use pmcore::prelude::*;
use std::collections::HashMap;

pub(crate) fn settings(
    settings: List,
    params: &Vec<String>,
    output_path: &str,
) -> Result<Settings> {
    let settings = settings.into_hashmap();
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
        "postprob" => pmcore::prelude::Algorithm::POSTPROB,
        _ => return Err(anyhow::anyhow!("Algorithm {} not supported", algorithm)),
    };
    let blq = settings.get("blq").unwrap().as_real_vector().unwrap();
    let blq: Vec<Option<f64>> = blq
        .iter()
        .map(|&x| if x.is_nan() { None } else { Some(x) })
        .collect();

    let error_models_raw = settings.get("error_models").unwrap().as_list().unwrap();

    let mut ems = ErrorModels::new();

    for (i, (_, em)) in error_models_raw.iter().enumerate() {
        let em = em.as_list().unwrap().into_hashmap();
        let gamlam = em.get("initial").unwrap().as_real().unwrap();
        let type_vec = em.get("type").unwrap().as_string_vector().unwrap();
        let err_type = type_vec.first().unwrap();
        let coeff = em.get("coeff").unwrap().as_real_vector().unwrap();
        match err_type.as_str() {
            "additive" => {
                ems = ems.add(
                    i,
                    ErrorModel::additive(
                        ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]),
                        gamlam,
                        blq[i],
                    ),
                )?;
            }
            "proportional" => {
                ems = ems.add(
                    i,
                    ErrorModel::proportional(
                        ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]),
                        gamlam,
                        blq[i],
                    ),
                )?;
            }
            err => {
                bail!("Invalid Error type: {}", err);
            }
        }
    }

    let max_cycles = settings
        .get("max_cycles")
        .unwrap()
        .as_real()
        .unwrap_or(100.0) as usize;
    let ind_points = settings.get("points").unwrap().as_real().unwrap_or(2028.0) as usize;
    let seed = settings.get("seed").unwrap().as_real().unwrap_or(22.0) as usize;

    let prior = settings.get("prior").unwrap().as_str().unwrap().to_string();
    let prior = match prior.as_str() {
        "sobol" => pmcore::prelude::Prior::sobol(ind_points, seed),
        "prior.csv" => pmcore::prelude::Prior::File("prior.csv".to_string()),

        _ => return Err(anyhow::anyhow!("Prior {} not supported", prior)),
    };

    let idelta = settings.get("idelta").unwrap().as_real().unwrap_or(0.01);
    let tad = settings.get("tad").unwrap().as_real().unwrap_or(0.0) as f64;

    let mut settings = Settings::builder()
        .set_algorithm(algorithm)
        .set_parameters(parameters)
        .set_error_models(ems)
        .build();
    settings.set_idelta(idelta);
    settings.set_tad(tad);
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
        parameters = parameters.add(param.to_string(), *min, *max);
    }

    Ok(parameters)
}
