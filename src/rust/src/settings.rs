use extendr_api::List;
use pmcore::prelude::*;
use std::collections::HashMap;

pub(crate) fn settings(ranges: List, params: &Vec<String>, output_path: &str) -> Result<Settings> {
    let ranges = robj_to_hashmap(ranges);
    let parameters = parse_parameters(ranges, params)?;
    let mut settings = Settings::builder()
        .set_algorithm(pmcore::prelude::Algorithm::NPAG)
        .set_parameters(parameters)
        .set_error_model(
            pmcore::prelude::ErrorModel::Additive,
            0.5,
            (0.0, 0.5, 0.0, 0.0),
        )
        .build();
    settings.set_cycles(1000);
    settings.set_prior(Prior::sobol(2028, 22));
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
