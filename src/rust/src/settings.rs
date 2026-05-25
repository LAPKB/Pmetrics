use anyhow::{anyhow, bail, Result as AnyResult};
use extendr_api::{Conversions, List, Robj};
use pmcore::api::NonparametricEstimationProblemBuilder;
use pmcore::prelude::*;
use std::collections::HashMap;

fn get_field<'a>(map: &'a HashMap<&str, Robj>, key: &str) -> AnyResult<&'a Robj> {
    map.get(key)
        .ok_or_else(|| anyhow!("Missing required setting '{}'", key))
}

fn get_list(map: &HashMap<&str, Robj>, key: &str) -> AnyResult<List> {
    get_field(map, key)?
        .as_list()
        .ok_or_else(|| anyhow!("Setting '{}' is not a list", key))
}

fn get_str(map: &HashMap<&str, Robj>, key: &str) -> AnyResult<String> {
    get_field(map, key)?
        .as_str()
        .ok_or_else(|| anyhow!("Setting '{}' is not a string", key))
        .map(|value| value.to_string())
}

fn get_real_or(map: &HashMap<&str, Robj>, key: &str, default: f64) -> AnyResult<f64> {
    match map.get(key) {
        Some(value) => {
            if let Some(real) = value.as_real() {
                Ok(real)
            } else if let Some(integer) = value.as_integer() {
                Ok(integer as f64)
            } else {
                Err(anyhow!("Setting '{}' is not numeric", key))
            }
        }
        None => Ok(default),
    }
}

fn get_bool_or(map: &HashMap<&str, Robj>, key: &str, default: bool) -> AnyResult<bool> {
    match map.get(key) {
        Some(value) => value
            .as_logical()
            .ok_or_else(|| anyhow!("Setting '{}' is not logical", key))
            .map(|value| value.to_bool()),
        None => Ok(default),
    }
}

pub(crate) enum FitBuilder<E: Equation> {
    Npag(NonparametricEstimationProblemBuilder<E>),
    Npod(NonparametricEstimationProblemBuilder<E>),
    PostProb(NonparametricEstimationProblemBuilder<E>),
}

impl<E: Equation + EquationMetadataSource> FitBuilder<E> {
    fn parameter(self, parameter: Parameter) -> AnyResult<Self> {
        match self {
            Self::Npag(builder) => Ok(Self::Npag(builder.parameter(parameter)?)),
            Self::Npod(builder) => Ok(Self::Npod(builder.parameter(parameter)?)),
            Self::PostProb(builder) => Ok(Self::PostProb(builder.parameter(parameter)?)),
        }
    }

    fn error(self, name: &str, model: AssayErrorModel) -> AnyResult<Self> {
        match self {
            Self::Npag(builder) => Ok(Self::Npag(builder.error(name, model)?)),
            Self::Npod(builder) => Ok(Self::Npod(builder.error(name, model)?)),
            Self::PostProb(builder) => Ok(Self::PostProb(builder.error(name, model)?)),
        }
    }

    fn output_dir(self, path: impl Into<String>) -> Self {
        let path = path.into();
        match self {
            Self::Npag(builder) => Self::Npag(builder.output_dir(path.clone())),
            Self::Npod(builder) => Self::Npod(builder.output_dir(path.clone())),
            Self::PostProb(builder) => Self::PostProb(builder.output_dir(path)),
        }
    }

    fn cycles(self, cycles: usize) -> Self {
        match self {
            Self::Npag(builder) => Self::Npag(builder.cycles(cycles)),
            Self::Npod(builder) => Self::Npod(builder.cycles(cycles)),
            Self::PostProb(builder) => Self::PostProb(builder.cycles(cycles)),
        }
    }

    fn cache(self, enabled: bool) -> Self {
        match self {
            Self::Npag(builder) => Self::Npag(builder.cache(enabled)),
            Self::Npod(builder) => Self::Npod(builder.cache(enabled)),
            Self::PostProb(builder) => Self::PostProb(builder.cache(enabled)),
        }
    }

    fn progress(self, enabled: bool) -> Self {
        match self {
            Self::Npag(builder) => Self::Npag(builder.progress(enabled)),
            Self::Npod(builder) => Self::Npod(builder.progress(enabled)),
            Self::PostProb(builder) => Self::PostProb(builder.progress(enabled)),
        }
    }

    fn idelta(self, value: f64) -> Self {
        match self {
            Self::Npag(builder) => Self::Npag(builder.idelta(value)),
            Self::Npod(builder) => Self::Npod(builder.idelta(value)),
            Self::PostProb(builder) => Self::PostProb(builder.idelta(value)),
        }
    }

    fn tad(self, value: f64) -> Self {
        match self {
            Self::Npag(builder) => Self::Npag(builder.tad(value)),
            Self::Npod(builder) => Self::Npod(builder.tad(value)),
            Self::PostProb(builder) => Self::PostProb(builder.tad(value)),
        }
    }

    fn prior(self, prior: Prior) -> Self {
        match self {
            Self::Npag(builder) => Self::Npag(builder.prior(prior.clone())),
            Self::Npod(builder) => Self::Npod(builder.prior(prior.clone())),
            Self::PostProb(builder) => Self::PostProb(builder.prior(prior)),
        }
    }
}

impl<E> FitBuilder<E>
where
    E: Equation + Clone + Send + 'static + EquationMetadataSource,
{
    pub(crate) fn fit(self) -> AnyResult<FitResult<E>> {
        match self {
            Self::Npag(builder) => builder.fit(),
            Self::Npod(builder) => builder.fit(),
            Self::PostProb(builder) => builder.fit(),
        }
    }
}

pub(crate) fn settings<E>(
    settings: List,
    equation: E,
    data: Data,
    output_path: &str,
) -> AnyResult<FitBuilder<E>>
where
    E: Equation + Clone + Send + 'static + EquationMetadataSource,
{
    let settings: HashMap<&str, Robj> = HashMap::try_from(&settings)
        .map_err(|e| anyhow!("Failed to convert settings list to map: {}", e))?;

    let metadata = equation
        .equation_metadata()
        .ok_or_else(|| anyhow!("runtime model metadata is required"))?;
    let parameter_names = metadata
        .parameters()
        .iter()
        .map(|parameter| parameter.name().to_string())
        .collect::<Vec<_>>();
    let output_labels = metadata
        .outputs()
        .iter()
        .map(|output| output.name().to_string())
        .collect::<Vec<_>>();

    let ranges = get_list(&settings, "ranges")?;
    let ranges = robj_to_hashmap(ranges)?;
    let parameters = parse_parameters(ranges, &parameter_names)?;

    let algorithm = get_str(&settings, "algorithm")?;
    let mut builder = match algorithm.to_lowercase().as_str() {
        "npag" => FitBuilder::Npag(EstimationProblem::builder(equation, data).method(Npag::new())),
        "npod" => FitBuilder::Npod(EstimationProblem::builder(equation, data).method(Npod::new())),
        "postprob" => {
            FitBuilder::PostProb(EstimationProblem::builder(equation, data).method(PostProb::new()))
        }
        _ => return Err(anyhow!("Algorithm {} not supported", algorithm)),
    };

    for parameter in parameters {
        builder = builder.parameter(parameter)?;
    }

    let error_models_raw = get_list(&settings, "error_models")?;

    for (i, (_, em)) in error_models_raw.iter().enumerate() {
        let outeq = i + 1;
        let em_list = em
            .as_list()
            .ok_or_else(|| anyhow!("error_models[{}] is not a list", outeq))?;
        let em: HashMap<&str, Robj> = HashMap::try_from(&em_list)
            .map_err(|e| anyhow!("Failed to parse error_models[{}]: {}", outeq, e))?;

        let gamlam = get_field(&em, "initial")?
            .as_real()
            .ok_or_else(|| anyhow!("error_models[{}].initial is not a real number", outeq))?;
        let type_vec = get_field(&em, "type")?
            .as_string_vector()
            .ok_or_else(|| anyhow!("error_models[{}].type is not a character vector", outeq))?;
        let err_type = type_vec
            .first()
            .ok_or_else(|| anyhow!("error_models[{}].type is empty", outeq))?;
        let fixed = get_field(&em, "fixed")?
            .as_logical()
            .ok_or_else(|| anyhow!("error_models[{}].fixed is not logical", outeq))?;
        let coeff = get_field(&em, "coeff")?
            .as_real_vector()
            .ok_or_else(|| anyhow!("error_models[{}].coeff is not a numeric vector", outeq))?;
        if coeff.len() < 4 {
            bail!(
                "error_models[{}].coeff must have at least 4 values, got {}",
                outeq,
                coeff.len()
            );
        }

        let assay_model = match err_type.as_str() {
            "additive" => {
                if fixed.to_bool() {
                    AssayErrorModel::additive_fixed(
                        ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]),
                        gamlam,
                    )
                } else {
                    AssayErrorModel::additive(
                        ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]),
                        gamlam,
                    )
                }
            }
            "proportional" => {
                if fixed.to_bool() {
                    AssayErrorModel::proportional_fixed(
                        ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]),
                        gamlam,
                    )
                } else {
                    AssayErrorModel::proportional(
                        ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]),
                        gamlam,
                    )
                }
            }
            err => bail!("Invalid Error type: {}", err),
        };

        let output_name = output_labels.get(i).ok_or_else(|| {
            anyhow!(
                "error model {} has no matching output equation in the compiled DSL metadata",
                outeq
            )
        })?;
        builder = builder.error(output_name, assay_model)?;
    }

    let max_cycles = get_real_or(&settings, "max_cycles", 100.0)? as usize;
    let ind_points = get_real_or(&settings, "points", 2028.0)? as usize;
    let seed = get_real_or(&settings, "seed", 22.0)? as usize;

    let prior = get_str(&settings, "prior")?;
    let prior = match prior.as_str() {
        "sobol" => Prior::sobol(ind_points, seed),
        "prior.csv" => Prior::File("prior.csv".to_string()),
        _ => return Err(anyhow!("Prior {} not supported", prior)),
    };

    let cache = get_bool_or(&settings, "cache", true)?;
    let progress = get_bool_or(&settings, "progress", true)?;
    let idelta = get_real_or(&settings, "idelta", 0.01)?;
    let tad = get_real_or(&settings, "tad", 0.0)?;

    Ok(builder
        .output_dir(output_path.to_string())
        .cycles(max_cycles)
        .cache(cache)
        .progress(progress)
        .idelta(idelta)
        .tad(tad)
        .prior(prior))
}

fn robj_to_hashmap(list: List) -> AnyResult<HashMap<String, (f64, f64)>> {
    let mut map: HashMap<String, (f64, f64)> = HashMap::new();
    for (name, value) in list.iter() {
        let ranges = value
            .as_real_slice()
            .ok_or_else(|| anyhow!("Range for parameter '{}' is not numeric", name))?;
        if ranges.len() < 2 {
            bail!(
                "Range for parameter '{}' must have at least 2 values, got {}",
                name,
                ranges.len()
            );
        }
        map.insert(name.to_owned(), (ranges[0], ranges[1]));
    }
    Ok(map)
}

fn parse_parameters(
    ranges: HashMap<String, (f64, f64)>,
    params: &Vec<String>,
) -> AnyResult<Vec<Parameter>> {
    let mut parameters = Vec::new();
    for param in params.iter() {
        let (min, max) = match ranges.get(param) {
            Some(range) => range,
            None => {
                return Err(anyhow::anyhow!(
                    "Parameter {} not found in ranges {:?}",
                    param,
                    &ranges
                ));
            }
        };
        parameters.push(Parameter::bounded(param.to_string(), *min, *max));
    }

    Ok(parameters)
}

#[cfg(test)]
mod tests {
    use super::*;
    use extendr_api::prelude::*;

    fn equation() -> equation::ODE {
        equation::ODE::new(
            |x, p, _t, dx, b, _rateiv, _cov| {
                fetch_params!(p, ke);
                dx[0] = -ke * x[0] + b[0];
            },
            |_p, _t, _cov| lag! {},
            |_p, _t, _cov| fa! {},
            |_p, _t, _cov, _x| {},
            |x, p, _t, _cov, y| {
                fetch_params!(p, v);
                y[0] = x[0] / v;
            },
        )
        .with_nstates(1)
        .with_ndrugs(1)
        .with_nout(1)
        .with_metadata(
            equation::metadata::new("settings_cache")
                .parameters(["ke", "v"])
                .states(["central"])
                .outputs(["0"])
                .route(equation::Route::bolus("0").to_state("central")),
        )
        .expect("settings cache test metadata should validate")
    }

    fn data() -> Data {
        Data::new(vec![Subject::builder("1")
            .bolus(0.0, 100.0, 0)
            .observation(1.0, 10.0, 0)
            .observation(2.0, 7.0, 0)
            .build()])
    }

    fn settings_list(cache: Option<bool>, progress: Option<bool>) -> List {
        let error_model = List::from_pairs(vec![
            ("initial", r!(2.0)),
            ("type", r!(["additive"])),
            ("fixed", r!(false)),
            ("coeff", r!([0.0, 0.10, 0.0, 0.0])),
        ]);

        let mut pairs = vec![
            (
                "ranges",
                list!(ke = r!([0.05, 1.0]), v = r!([5.0, 50.0])).into(),
            ),
            ("algorithm", r!("NPAG")),
            ("error_models", List::from_values(vec![error_model]).into()),
            ("max_cycles", r!(2.0)),
            ("prior", r!("sobol")),
            ("points", r!(8.0)),
            ("seed", r!(7.0)),
        ];

        if let Some(enabled) = cache {
            pairs.push(("cache", r!(enabled)));
        }

        if let Some(enabled) = progress {
            pairs.push(("progress", r!(enabled)));
        }

        List::from_pairs(pairs)
    }

    fn compile_runtime_flags(
        cache: Option<bool>,
        progress: Option<bool>,
    ) -> AnyResult<(bool, bool, bool)> {
        let output_path = std::env::temp_dir()
            .join("pm-rs-settings-cache")
            .to_string_lossy()
            .into_owned();
        let builder = settings(
            settings_list(cache, progress),
            equation(),
            data(),
            &output_path,
        )?;
        let problem = match builder {
            FitBuilder::Npag(builder) => builder.build()?,
            FitBuilder::Npod(builder) => builder.build()?,
            FitBuilder::PostProb(builder) => builder.build()?,
        };
        let compiled = problem.compile()?;
        Ok((
            compiled.runtime_options().cache,
            compiled.caches.prediction_cache_enabled,
            compiled.runtime_options().progress,
        ))
    }

    #[test]
    fn settings_respects_explicit_cache_false() {
        test! {
            let (runtime_cache, prediction_cache_enabled) =
                {
                    let (runtime_cache, prediction_cache_enabled, _runtime_progress) =
                        compile_runtime_flags(Some(false), None)
                            .expect("cache = FALSE should compile");
                    (runtime_cache, prediction_cache_enabled)
                };
            assert!(!runtime_cache);
            assert!(!prediction_cache_enabled);
        }
    }

    #[test]
    fn settings_accepts_integer_max_cycles() {
        test! {
            let error_model = List::from_pairs(vec![
                ("initial", r!(2.0)),
                ("type", r!(["additive"])),
                ("fixed", r!(false)),
                ("coeff", r!([0.0, 0.10, 0.0, 0.0])),
            ]);

            let settings_list = List::from_pairs(vec![
                (
                    "ranges",
                    list!(ke = r!([0.05, 1.0]), v = r!([5.0, 50.0])).into(),
                ),
                ("algorithm", r!("NPAG")),
                ("error_models", List::from_values(vec![error_model]).into()),
                ("max_cycles", r!(2_i32)),
                ("prior", r!("sobol")),
                ("points", r!(8.0)),
                ("seed", r!(7.0)),
            ]);

            let builder = settings(
                settings_list,
                equation(),
                data(),
                std::env::temp_dir()
                    .join("pm-rs-settings-integer-cycles")
                    .to_string_lossy()
                    .as_ref(),
            )
            .expect("integer max_cycles should be accepted");

            match builder {
                FitBuilder::Npag(builder) => {
                    let compiled = builder.build().expect("builder should build").compile().expect("problem should compile");
                    assert_eq!(compiled.runtime_options().cycles, 2);
                }
                FitBuilder::Npod(_) | FitBuilder::PostProb(_) => panic!("expected NPAG builder"),
            }
        }
    }

    #[test]
    fn settings_defaults_cache_to_true() {
        test! {
            let (runtime_cache, prediction_cache_enabled) =
                {
                    let (runtime_cache, prediction_cache_enabled, _runtime_progress) =
                        compile_runtime_flags(None, None)
                            .expect("default cache settings should compile");
                    (runtime_cache, prediction_cache_enabled)
                };
            assert!(runtime_cache);
            assert!(prediction_cache_enabled);
        }
    }

    #[test]
    fn settings_respects_explicit_progress_false() {
        test! {
            let (_runtime_cache, _prediction_cache_enabled, runtime_progress) =
                compile_runtime_flags(None, Some(false))
                    .expect("progress = FALSE should compile");
            assert!(!runtime_progress);
        }
    }

    #[test]
    fn settings_defaults_progress_to_true() {
        test! {
            let (_runtime_cache, _prediction_cache_enabled, runtime_progress) =
                compile_runtime_flags(None, None)
                    .expect("default progress settings should compile");
            assert!(runtime_progress);
        }
    }
}
