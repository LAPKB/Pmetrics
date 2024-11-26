use extendr_api::List;
use pmcore::prelude::settings::*;
use std::collections::HashMap;
use toml::Table;

pub(crate) fn settings(ranges: List, params: &Vec<String>, output_path: &str) -> Settings {
    let ranges = robj_to_hashmap(ranges);
    let mut settings = Settings::default();

    settings.random = Random {
        parameters: create_random_parameters(ranges, params),
    };

    settings.output = Output {
        write: true,
        path: output_path.to_string(),
    };
    settings
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

fn create_random_parameters(ranges: HashMap<String, (f64, f64)>, params: &Vec<String>) -> Table {
    let mut random_parameters = Table::new();
    for param in params.iter() {
        let (min, max) = match ranges.get(param) {
            Some(range) => range,
            None => {
                eprintln!("Parameter {} not found in ranges {:?}", param, &ranges);
                panic!()
            }
        };
        random_parameters.insert(
            param.to_string(),
            toml::Value::Array(vec![toml::Value::Float(*min), toml::Value::Float(*max)]),
        );
    }
    random_parameters
}
