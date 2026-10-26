# bestDose Model Validation Checks

This document summarizes the checks performed by `checkModel()` and its underlying validation functions.

## 1. Model Structure

### Required model blocks

The validator checks that the model contains all required blocks:

- [ ] `primary`
- [ ] `covariates`
- [ ] `secondary`
- [ ] `initial_conditions`
- [ ] `fa`
- [ ] `lag`
- [ ] `equation`
- [ ] `out`
- [ ] `error`

Missing blocks are reported as errors.

---

## 2. Model Description

### Description elements

The validator checks that:

- [ ] No unexpected elements are present in `model$description`.
- [ ] Required elements are present:
  - [ ] `drug`
  - [ ] `route`
  - [ ] `name`
  - [ ] `version`
  - [ ] `compartment`
  - [ ] `description`
  - [ ] `reference`
  - [ ] `covariates`

- [ ] `name` is not empty.
- [ ] `drug` is not empty.
- [ ] `version` is not empty.
- [ ] Optional `dose_unit`, when provided, is not empty.
- [ ] Optional `target`, when provided, is not empty.
- [ ] Optional `target_unit`, when provided, is not empty.
- [ ] A model description is provided or a warning is generated.
- [ ] A reference is provided or a warning is generated.

### Compartments

The validator checks that:

- [ ] `compartment` is a list.
- [ ] `compartment$number` exists.
- [ ] `compartment$number` is a single integer between 1 and 10. It counts the **PK** compartments
      only: a joint PK/PD model's PD states are extra differential equations past them.
- [ ] At least one named compartment is defined.
- [ ] A `central` compartment is always present.
- [ ] No more than 6 named compartments are defined.
- [ ] The number of named compartments matches `compartment$number`.
- [ ] Each named compartment is represented by a unique integer from 1 to 6.
- [ ] No compartment index exceeds `compartment$number`.
- [ ] Named compartment indices are unique.
- [ ] `compartment$number` does not exceed the number of state equations.
- [ ] A warning is generated when `compartment$number` is lower than the number of state equations.

### Compartment outputs

When `compartment$outputs` is defined, the validator checks that:

- [ ] Outputs are provided as a data frame or non-empty list.
- [ ] Each output contains a `name`.
- [ ] Each output contains an `equation`.
- [ ] Output names are non-empty character values.
- [ ] Output equations are integers from 1 to 6.
- [ ] Output names are unique.
- [ ] A warning when there are more **PK** outputs than compartments (PD effect outputs are not counted).
- [ ] Output equation numbers are unique.

---

## 3. Routes

The route validation checks that:

- [ ] `description$route` is a data frame.
- [ ] At least one route is defined.
- [ ] The route data frame contains exactly two columns.
- [ ] The columns are named `route` and `compartment`.
- [ ] A warning is generated when multiple routes use the same compartment number.

---

## 4. Covariates in the Description

The validator checks that:

- [ ] Covariate information contains:
  - [ ] `number`
  - [ ] `names`
  - [ ] `label`
  - [ ] `units`
  - [ ] `types`
  - [ ] `value`
  - [ ] `description`

- [ ] The number of names matches `number`.
- [ ] The number of labels matches `number`.
- [ ] The number of units matches `number`.
- [ ] The number of types matches `number`.
- [ ] The number of values matches `number`.
- [ ] The number of descriptions matches `number`.

### Covariate names

The validator checks that:

- [ ] Covariate names contain only letters, numbers and `_`.
- [ ] `wt` and `tbw` or their equivelent in other languages are not used for weight; `weight` must be used.
- [ ] `gfr`, `egfr`, `ccr`, `clcr`, `dfg` and `edfg` are not used for renal function; `crcl` must be used.
- [ ] `creatinine` and `scr` are not used; `creat` must be used.
- [ ] `ht` and `size` are not used for height; `height` must be used.
- [ ] `imc` and `body_mass_index` are not used; `bmi` must be used.
- [ ] `sc` and `body_surface_area` are not used; `bsa` must be used.
- [ ] `sexe` and `gender` are not used; `sex` must be used.
- [ ] Reserved keywords are lowercase:
  - [ ] `weight`
  - [ ] `crcl`
  - [ ] `creat`
  - [ ] `height`
  - [ ] `bmi`
  - [ ] `bsa`
  - [ ] `sex`
  - [ ] `age`

### Numeric covariates

For numeric covariates, the validator checks that values are defined using either:

- [ ] `mean`, `sd`, `default`

or:

- [ ] `min`, `max`, `default`

Additionally:

- [ ] `min` is lower than `max`.
- [ ] `default` is numeric.
- [ ] `default` is within `min`/`max` when a range is provided.

### Categorical covariates

For categorical covariates, the validator checks that:

- [ ] Values are provided as a data frame.
- [ ] The data frame contains `cat_value`, `num_value`, and `default`.
- [ ] `cat_value` is character.
- [ ] `num_value` is numeric.
- [ ] Exactly one category is marked as the default.
- [ ] A warning is generated when categorical values look numeric.
- [ ] `sex` contains lowercase `male` and `female`.

### Covariate descriptions

- [ ] Each covariate has a character description.
- [ ] A warning is generated when a covariate description is empty.

---

## 5. Primary Parameters

The validator checks that:

- [ ] At least two primary parameters are defined.
- [ ] Each parameter contains:
  - [ ] `type`
  - [ ] `min`
  - [ ] `max`

- [ ] `type` is either `ab` or `msd`.
- [ ] `type` is not empty.
- [ ] `min` is numeric.
- [ ] `max` is numeric.
- [ ] For `ab` parameters, `min < max`.

---

## 6. Model Covariates

The validator checks that:

- [ ] The covariates block is a list.
- [ ] Each model covariate contains an `interp` element.
- [ ] `interp` is either `linear` or `none`.
- [ ] Numeric covariates can use `linear` interpolation.
- [ ] Categorical covariates cannot use `linear` interpolation.
- [ ] Categorical covariates must use `none` interpolation.

---

## 7. Secondary Parameters

The validator checks that:

- [ ] The secondary block is a list.
- [ ] Each secondary parameter is a character equation.
- [ ] Each secondary equation contains `=`.
- [ ] Each secondary parameter uses at least one primary parameter, directly or through another
      secondary (`kin = e0 * kout` uses no covariate, and that is fine). A constant (`v2 = 10`) is a
      fixed parameter and is exempt.
- [ ] A secondary parameter does not share its name with a primary one.
- [ ] A warning is generated for covariates not used anywhere the model evaluates them (secondary,
      differential, output, `fa`/`lag` or initial-condition expressions).

---

## 8. Initial Conditions

Pmetrics `#ini`: a state's value at time 0 (e.g. a PD turnover baseline); a state with no entry
starts at 0. The validator checks that:

- [ ] The `initial_conditions` block can be absent.
- [ ] Each entry is written as `X[N] = ...` (or `<-`), with `N` one of the model's states.
- [ ] Each entry is keyed `xN` for the `X[N]` it sets: the engine places the value by the key.
- [ ] The expression only reads parameters, covariates and secondary parameters.

---

## 9. Bioavailability (`fa`)

When the `fa` block is present, the validator checks that:

- [ ] `fa` is a list.
- [ ] Each FA definition is a character string.
- [ ] Each equation follows the expected form:
  - [ ] `FA[X] = ...`
  - [ ] `FA[X] <- ...`

---

## 10. Lag Time (`lag`)

When the `lag` block is present, the validator checks that:

- [ ] `lag` is a list.
- [ ] Each lag definition is a character string.
- [ ] Each equation follows the expected form:
  - [ ] `LAG[X] = ...`
  - [ ] `LAG[X] <- ...`

---

## 11. Model Equations

The validator checks that:

- [ ] The equation block exists.
- [ ] The equation block is a list.
- [ ] At least one equation is defined.
- [ ] Equation names follow `dxN`, where `N` is 1–10.
- [ ] Each equation is a character string.
- [ ] Each equation follows the expected form:
  - [ ] `dx[N] = ...`
  - [ ] `dx[N] <- ...`

- [ ] At least one equation contains a dose/input function:
  - [ ] `r[N]`
  - [ ] `b[N]`
  - [ ] `rateiv[N]`

---

## 12. Model Outputs

The validator checks that:

- [ ] The output block exists.
- [ ] The output block is a list.
- [ ] At least one output is defined.
- [ ] No more than 6 outputs are defined.
- [ ] Exactly one output equation per *declared* compartment output — `model_dsl::build_dsl_source`
      zips the two lists together, so that is the real constraint. The number of outputs is **not**
      tied to the compartment count: a two-compartment model with a single measured concentration
      is ordinary PK, and the states come from the differential equations, not from this list.
- [ ] Defined output equations correspond to the equations referenced in the compartment output definitions.
- [ ] Output names follow `yN`, where `N` is 1–6.
- [ ] Each output is a character equation.
- [ ] Each output follows the expected form:
  - [ ] `Y[N] = ...`
  - [ ] `Y[N] <- ...`

- [ ] Each concentration output contains an `X[N]` reference.
- [ ] `X[N]` is divided by another parameter (amount to concentration).
- [ ] A PD effect output is exempt from the two rules above (an effect is any expression of the
      states and parameters), but keeps the `yN` name and the `Y[N] = ...` form.
- [ ] A metabolite output (`compartment$outputs[i]$metabolite`, parent-metabolite model) is a
      concentration and keeps the two rules above. It is never output 1 (`central`), never also a
      PD effect, and is not named `concentration`, after a covariate, or like another output up to
      case (its levels are matched by name). An error when it has no error row of its own, as for the parent. Its Y is
      the mass of metabolite per litre (in the drug's concentration unit), never a parent-equivalent mass; its label, units and targets live in the drug file's
      `metabolites`, matched by name.

---

## 13. Error Model

The validator checks that:

- [ ] The error block exists.
- [ ] The error block is a list.
- [ ] Required elements are present:
  - [ ] `type`
  - [ ] `initial_value`
  - [ ] `coefficient`

- [ ] `type` is either `additional` or `proportional`.
- [ ] `initial_value` is numeric.
- [ ] `initial_value` is greater than zero — it is the assay's gamma/lambda, and a zero leaves the
      error model with no scale, so every support point gets the same degenerate likelihood.
- [ ] `coefficient` is a data frame.
- [ ] Coefficient columns `c0`, `c1`, `c2`, and `c3` exist.
- [ ] At most one coefficient row per output. `posterior::build_error_models` indexes the rows by
      output and falls back to the last, so a second, differently-assayed output may carry its own;
      rows past the last output are never read and are refused.
- [ ] Coefficient values are numeric.
- [ ] A row may carry its own `type` and `initial_value` for its output (Pmetrics gives each output
      its own error model); they answer to the same rules as the block's.
- [ ] A warning when every coefficient of a row is zero (no variance at all).

---

## 13a. PD Model

Only for a joint PK/PD model: which primary parameters are PD (`description$pd_parameters`) and
which outputs are effects (`compartment$outputs[i]$pd`, with `label` and `unit`). The validator
checks that:

- [ ] Every PD parameter is a primary parameter.
- [ ] The `central` output is never an effect.
- [ ] Every effect has a label; a warning when it has no unit.
- [ ] An effect's name says what it measures: a measured effect is recorded on the patient as a row
      named after its output and matched by that name alone. So the name must not be
      `concentration` or a covariate's name (error), must not differ from another output's name only
      in case (error), and a generic `effect` is a warning.
- [ ] A warning when an effect has no error row of its own: it would reuse the last row, which is the
      concentration assay's error, on another scale.
- [ ] A warning for PD parameters with no effect output, and for an effect output with no PD
      parameter (nothing to individualize from measured effects).

---

## 14. Parameter Consistency

The parameter validation performs cross-block checks.

### Support points

The validator checks that:

- [ ] Support points contain a `prob` element.
- [ ] Every primary parameter has corresponding support points.
- [ ] The support point probabilities sum to 1 (within 0.01). The weights are renormalised before
      the fit, so a short sum breaks nothing outright — it is the signature of a grid that was
      imported incomplete, and is reported as a warning.
- [ ] Every support-point parameter is defined as a primary parameter.
- [ ] No unsupported parameter is present in the support-point block.

### Secondary parameter dependencies

For every secondary parameter:

- [ ] At least one primary parameter is used.
- [ ] At least one covariate is used.

The validator also checks that:

- [ ] Covariates used in secondary equations are defined in the model covariate block.
- [ ] Model covariates are used in at least one secondary equation.
- [ ] Covariates defined in the description are defined in the model covariate block.
- [ ] Model covariates are defined in the description.
- [ ] Covariates used in secondary equations are defined in the description.
- [ ] Warnings are generated for description covariates that are not used.

### Primary/secondary parameter usage

The validator constructs the expected parameter set from:

- [ ] Secondary parameters.
- [ ] Primary parameters not replaced/used by secondary equations.

It then checks that expected parameters are used somewhere in:

- [ ] State equations.
- [ ] Output equations.
- [ ] FA equations.
- [ ] LAG equations.

Unused expected parameters are reported as errors.

### FA and LAG dependencies

The validator extracts variables used in FA and LAG equations and checks that they are defined among:

- [ ] Primary parameters.
- [ ] Secondary parameters.
- [ ] Covariates.
- [ ] Allowed time variables (`t`, `time`, `TIME`).

Unknown variables in FA or LAG equations are reported as errors.

---

## 15. Covariate Consistency Across Blocks

The validator cross-checks covariate names between:

- [ ] `description$covariates`
- [ ] `model$covariates`
- [ ] Secondary equations

It checks for:

- [ ] Covariates present in the description but missing from the model.
- [ ] Covariates present in the model but missing from the description.
- [ ] Covariates used in secondary equations but missing from the model covariate block.
- [ ] Covariates defined in the model but not used in secondary equations.
- [ ] Covariates used in secondary equations but missing from the description.
- [ ] Covariates defined in the description but not used in secondary equations.

---

## 16. Route/Input Function Consistency

The validator cross-checks routes against dose input functions in the equations.

It checks that:

- [ ] Route information is a data frame.
- [ ] The route data frame contains a `compartment` column.
- [ ] At least one dose input function exists when routes are defined.
- [ ] Every input number referenced by an equation is defined in `description$route`.
- [ ] Every defined route is referenced by at least one dose input function, otherwise a warning is generated.
- [ ] `r[N]` and `rateiv[N]` are only used for `IV` or `SC` routes.
- [ ] Other routes use compatible input functions.

Recognized input functions are:

- [ ] `r[N]`
- [ ] `b[N]`
- [ ] `rateiv[N]`

---

## 17. Overall Validation

`checkModel()` combines all individual checks:

- [ ] Model structure
- [ ] Model description
- [ ] Routes
- [ ] Description covariates
- [ ] Primary parameters
- [ ] Model covariates
- [ ] Secondary parameters
- [ ] Initial conditions
- [ ] Bioavailability
- [ ] Lag time
- [ ] Equations
- [ ] Outputs
- [ ] Error model
- [ ] PD model
- [ ] Parameter consistency
- [ ] Cross-block covariate consistency

The final validation result contains:

- [ ] Overall `valid` status.
- [ ] Overall `has_warning` status.
- [ ] Errors and warnings grouped by validation block.
- [ ] Covariate information.
- [ ] Primary parameter names.
- [ ] Secondary parameter names.
- [ ] Secondary equations.
- [ ] Equation names.
- [ ] Output equation names.

### Validation interpretation

- **Error** — the model is considered invalid and should be corrected before use.
- **Warning** — the model can remain valid, but the model definition should be reviewed.
- **Valid** — all required validation checks passed without errors.
