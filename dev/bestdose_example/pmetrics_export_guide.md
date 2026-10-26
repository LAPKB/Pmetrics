# Exporting a Pmetrics run to BestDose (`$export()`)

`run$export()` writes a BestDose model file (`<model>.bdm`, JSON) from a finished NPAG/NPOD run. This page lists what must come from the run, what may be added, and what must never be in the file. BestDose-side fields (clinical wrapper, targets, versioning) are completed in BestDose.

```r
run <- PM_load(1)
run$export(path = "bestdose", drug = "vancomycin", model = "vanco_adult", ask = FALSE)
```

## 1. Must come from the run

The export stops with an error, or the file is rejected by BestDose, if one of these is missing or invalid.

| Item | Source in the run | Requirement |
|---|---|---|
| Final population support points | `run$final$popPoints` | Present; one column per `#pri` parameter plus `prob`. |
| Primary parameters | `#pri` | At least 2, each a range (`min < max`). Every one must be used in the model. |
| Differential equations | `#eqn` | At least one `dx[i]`. Library (analytical) models are converted to ODEs automatically. |
| Dose inputs | `#eqn` | At least one `R[i]` (infusion) or `B[i]` (bolus). |
| Outputs | `#out` | 1 to 6 `Y[i]`. The first one is a concentration: `X[n]` divided by a volume-like term. |
| Error model | `#err` | One per output; `initial` value > 0. |
| Model definition | `PM_model` | Written with `pri`, `eqn`, `out` and `err` blocks. |
| Drug and model names | `drug`, `model` arguments | Non-empty strings (asked in the console when `ask = TRUE`). The drug must already exist in BestDose. |

## 2. Taken from the run when present (nothing to do)

| Item | What is exported |
|---|---|
| `#sec`, `#ini`, `#fa`, `#lag` | Copied as written (optional blocks). |
| `#cov` | Covariates that an equation uses, with their interpolation (`linear` or `none`). Unused covariates are left out (message). |
| Covariate ranges | Observed min, max and median (default) in the data file. |
| Error values | Gamma/lambda of the last cycle (`cycle$gamlam`) when estimated, else the `#err` initial value. |
| Routes | Inferred from the doses (`EVID = 1`, `DUR`, `INPUT`) and the model inputs. Infusions and boluses into the central compartment are labelled `IV`, other boluses `PO`. |
| Compartments | `central`, `peripheral`, `depot` inferred from the first output. Other states and outputs are flagged as PD. |
| Fit summary | Number of subjects, support points, cycles and -2LL, written in the `description` text. |

## 3. Optional

Not needed to import the file or to run MIPD estimation. Each has a neutral default, can be set through `...` or `outputs`, or edited in BestDose.

| Field | Default | How to set |
|---|---|---|
| `description` | Generated from the fit | `description = "..."` |
| `reference`, `reference_url` | "Exported from a Pmetrics run", empty | `reference = "Author, Journal, 2024"` |
| Covariate `label`, `units`, `description` | Built in for known names, else name and empty unit | Console questions (`ask = TRUE`), or BestDose editor |
| Covariate display range and default | From the data | BestDose editor |
| PD output `name`, `label`, `unit` | `pd<N>`, "Output N", empty | `outputs = list("2" = list(name = "platelets", label = "Platelets", unit = "10^9/L"))` |
| Output names (non-PD) | `central`, `peripheral` | `outputs` |
| `pd_parameters` | Inferred: parameters used only by the PD part | `pd_parameters = c(...)` |
| `default_model` | `false` | `default_model = TRUE` |
| Route label (`IM`, `SC`) | `IV` or `PO` from the data | BestDose editor |

## 4. Never in the file

| Item | Why |
|---|---|
| Fixed parameters (`name, value!` in `#pri`) | Not allowed as primary. Write the constant in `#sec` (`v2 = 10`) before the run. |
| Models without `pri`, `eqn`, `out` and `err` blocks | Not exportable; BestDose models are ODE based. |
| Covariate aliases | Renamed to the reserved name: `weight`, `crcl`, `creat`, `height`, `bmi`, `bsa`, `sex`, `age` (English, French, Spanish, Norwegian aliases are recognised). |
| Categorical covariates with interpolation | Only `sex` is treated as categorical (`male`/`female`), with no interpolation. |
| Parameter in both `#pri` and `#sec` | A secondary cannot share a name with a primary. |
| Per-subject data, individual posteriors, observations | The file holds the population prior only. |
| `forked_from`, `update_snoozed` | Set by BestDose. Do not add them via `...`. |
| `native` | Set by BestDose. `export()` writes `false`; do not override it. |
| Any other top-level key besides `description`, `model`, `support_point` | Dropped by BestDose on the next save. |
| Drug-level information (max dose, targets, `fu`, metabolite labels) | Lives in the BestDose drug file, not in the model file. |

## 5. After the export

`export()` checks the file against the BestDose model checklist and reports errors and warnings in the console. A file with errors is still written but must be corrected before use; warnings (empty units, empty reference, probabilities not summing to 1) can be left or fixed in BestDose.
