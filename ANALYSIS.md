# Pmetrics DSL Compilation & Model Execution - Comprehensive Analysis

## Architecture Overview

Pmetrics 3.0+ is a hybrid R/Rust application where R serves as the user-facing API layer and Rust (via PMcore) handles the heavy computational work. The system follows a clear pipeline:

```
R Model Definition → DSL Compilation → Rust JIT → PMcore Estimation → Fit Payload → PM_result → Report
```

---

## 1. DSL Compilation

### 1.1 R-Side Model Definition (`R/PM_model.R`)

The `PM_model` R6 class is the central abstraction. Users define models through named blocks:

| Block | Type | Purpose |
|-------|------|---------|
| `pri` | list | Primary (random) parameters with `ab()` or `msd()` creators |
| `cov` | list | Covariates with `interp()` creator |
| `sec` | function | Secondary/global equations |
| `eqn` | function | Model equations (ODE or analytical template) |
| `lag` | function | Lag time / delayed absorption |
| `fa` | function | Bioavailability (fraction absorbed) |
| `ini` | function | Initial compartment conditions |
| `out` | function | Output equations (predictions) |
| `err` | list | Error models (`additive()`, `proportional()`) |
| `solver` | string | ODE solver selection (BDF, TRBDF2, ESDIRK34, TSIT45) |

```r
mod_list <- list(
  pri = list(CL = ab(10, 200), V0 = ab(0, 100), ka = ab(0, 3)),
  cov = list(wt = interp()),
  sec = function() { V <- V0 * (wt / 70) },
  eqn = function() {
    dx[1] <- -ka * x[1]
    dx[2] <- rateiv[1] + ka * x[1] - (ke + k23) * x[2] + k32 * x[3]
  },
  out = function() { y[1] <- x[1] / V },
  err = list(proportional(2, c(0.1, 0.15, 0, 0)))
)
mod <- PM_model$new(mod_list)
```

### 1.2 Model Validation (`PM_model$initialize`)

The `initialize` method performs extensive validation:

1. **Reserved name conflicts** - Checks against `t, x, dx, p, b, bolus, r, rateiv, cov, y`
2. **Required blocks** - `pri`, `eqn`, `out`, `err` are mandatory
3. **Model type detection** - Analytical (library template) vs ODE (user-defined)
4. **Index literal enforcement** - ODE models require literal compartment indices (e.g., `x[1]` not `x[i]`) so the Rust `ode!` macro can infer dimensions at compile time
5. **Parameter completeness** - For analytical templates, verifies all required parameters are defined across blocks
6. **Error model count** - One error model per output equation

### 1.3 DSL Parsing Helpers (`R/model_dsl.R`)

Small but critical utilities for AST analysis:

- `statement_exprs()` - Extracts statements from function bodies
- `reserved_name_conflicts()` - Detects naming collisions with DSL reserved words
- `get_max_index()` / `collect_index_refs()` - Finds max compartment/input indices from bracket expressions
- `get_assignments()` / `get_assignment_indices()` - Counts `dx[i]` and `y[i]` assignments
- `has_nonliteral_index()` - Validates ODE models use static indices

### 1.4 Rust-Side Runtime Compilation (`src/rust/src/lib.rs`)

The `compile_runtime_model` function bridges R to Rust:

```rust
// Called via extendr from R
pub fn compile_runtime_model(
    model_source: &str,
    kind: &str,       // "ode" or "analytical"
    solver: Option<String>
) -> AnyResult<CompiledRuntimeModel>
```

Key flow:
1. Validates model source is non-empty and kind is supported
2. Calls `dsl::compile_module_source_to_runtime()` with `RuntimeCompilationTarget::Jit`
3. Validates the compiled model's actual kind matches the declared kind
4. Applies ODE solver selection if provided
5. Returns a `CompiledRuntimeModel` (enum: `Ode`, `Analytical`, `Sde`)

The actual DSL compilation happens in **PMcore** at `PMcore/compile/`:

| File | Responsibility |
|------|----------------|
| `mod.rs` | Entry point, compilation pipeline orchestration |
| `caches.rs` | Prediction caching infrastructure |
| `compiled_problem.rs` | Builds the computable problem from model + data |
| `design_context.rs` | Context for model design-time analysis |
| `observation_index.rs` | Maps observations to output equations |
| `validation.rs` | Pre-flight validation of model-data compatibility |

### 1.5 Model Library (`R/mod_lib.R`)

Pre-defined analytical PK templates (e.g., `one_comp_iv`, `two_comp_iv`). The `mod_list` is a tibble of model definitions. Key functions:

- `get_found_model()` - Detects which template (if any) is referenced in `eqn`
- `model_lib()` - Launches the PmetricsModelLib Shiny app for browsing templates
- `generate_model_code_text()` - Generates ready-to-use `PM_model$new()` code from a template

---

## 2. Model Execution

### 2.1 The Fit Pipeline (`R/PM_model.R::fit`)

The `$fit()` method is the main entry point, replacing the legacy `NPrun()`:

```
R fit() → PM_data → PM_model → compile → Rust extendr → PMcore fit → FitPayload → PM_result
```

Key steps:

1. **Data preparation** - Creates/validates `PM_data` object from file or data frame
2. **Path setup** - Creates output directory with run number
3. **Parameter assembly** - Builds fit parameters (ranges, error models, prior, points)
4. **Rust invocation** - Calls into Rust via extendr bindings
5. **Result construction** - Builds `PM_result` from the JSON payload
6. **Snapshot saving** - Saves `PMout.Rdata` for reproducibility

### 2.2 Rust Execution Layer (`src/rust/src/executor.rs`)

```rust
pub(crate) fn fit<E>(
    equation: E,           // Compiled runtime model
    data: Data,            // Parsed Pmetrics data
    params: List,          // R list of fit parameters
    output_path: &str      // Output directory
) -> Result<FitPayload>
```

The executor:
1. Extracts live session ID, idelta, tad from params
2. Builds a `NonparametricEstimationProblemBuilder` via `settings()`
3. If a live session is active, runs with progress callbacks
4. Otherwise runs in-memory
5. Materializes a `FitPayload` from the `FitResult`

### 2.3 Settings Builder (`src/rust/src/settings.rs`)

The `settings()` function translates R parameters into PMcore's `NonparametricEstimationProblemBuilder`:

```rust
pub(crate) enum FitBuilder<E> {
    Npag(NonparametricEstimationProblemBuilder<E>),
    Npod(NonparametricEstimationProblemBuilder<E>),
    PostProb(NonparametricEstimationProblemBuilder<E>),
}
```

Each builder method (`parameter`, `error`, `cycles`, `cache`, `progress`, `prior`, etc.) is dispatched across all three algorithm variants.

### 2.4 PMcore Estimation Engine (`PMcore/src/estimation/`)

| Path | Purpose |
|------|---------|
| `nonparametric/` | NPAG - Nonparametric Adaptive Grid algorithm |
| `parametric/` | NPOD - Nonparametric Optimization by Duality |

The nonparametric workspace (`NonparametricWorkspace<E>`) manages:
- Cycle-by-cycle adaptive grid updates
- Theta matrix (support point values × probabilities)
- Posterior calculations
- Prediction generation

### 2.5 Fit Payload Serialization (`src/rust/src/fit_payload.rs`)

The `FitPayload` struct bridges Rust → R:

```rust
pub struct FitPayload {
    parameter_names: Vec<String>,
    covariate_names: Vec<String>,
    iterations: Vec<Map<String, Value>>,  // Cycle-by-cycle metrics
    theta: Vec<Map<String, Value>>,        // Support point values
    posterior: Vec<Map<String, Value>>,     // Individual posteriors
    predictions: Vec<PredictionRow>,        // Population/individual predictions
    covariates: Vec<Map<String, Value>>,    // Time-varying covariates
}
```

Each field is serialized to JSON, sent to R, then deserialized in `R/PM_fit_payload.R`:

```r
decode_fit_payload <- function(payload_json, fit_params) {
    parsed <- jsonlite::fromJSON(payload_json)
    # ... tibble construction with type coercion
    structure(list(
        parameter_names, covariate_names, iterations, theta,
        posterior, predictions, covariates, config
    ), class = "PM_fit_payload")
}
```

### 2.6 PM_result Construction (`R/PM_fit_payload.R`)

```r
build_pm_result_from_fit_payload <- function(payload_json, data, model, fit_params, path) {
    fit_payload <- decode_fit_payload(payload_json, fit_params)
    
    out <- list(
        pop   = PM_pop$new(fit_payload, path),
        post  = PM_post$new(fit_payload, path),
        final = PM_final$new(fit_payload, path),
        cycle = PM_cycle$new(fit_payload, path),
        op    = PM_op$new(fit_payload, path),
        cov   = PM_cov$new(fit_payload, path),
        data  = data,
        model = model,
        success = TRUE
    )
    
    save_pm_result_snapshot(out, path)
    PM_result$new(out, path = path, quiet = TRUE)
}
```

Each result component (`PM_pop`, `PM_post`, `PM_final`, `PM_cycle`, `PM_op`, `PM_cov`) is an R6 object that wraps the corresponding tibble from the payload.

---

## 3. Live Execution System

### 3.1 Architecture

The live execution system enables real-time progress reporting during model fitting via a TCP-based session protocol.

```
R (PM_live.R) ←JSON→ TCP ←JSON→ Rust (live.rs) ←events→ PMcore (FitProgress)
                                    ↑
                              Shiny App (PmetricsReports)
```

### 3.2 R-Side Live Session (`R/PM_live.R`)

Key functions:

| Function | Purpose |
|----------|---------|
| `start_live_report_session()` | Launches the Shiny app and connects to Rust |
| `send_live_report_result()` | Serializes and sends the final result via gzip+base64 |
| `send_live_report_failure()` | Sends failure messages to the live session |
| `encode_live_report_result()` | Gzip-compresses + base64-encodes the serialized result |

The live session registry is a `OnceLock<Mutex<HashMap<String, Arc<LiveSession>>>>` shared across all R threads.

### 3.3 Rust Live Session (`src/rust/src/live.rs`)

```rust
pub struct LiveSession {
    id: String,              // Unique session identifier
    host: String,             // "127.0.0.1"
    port: u16,                // Dynamic TCP port
    state: Mutex<LiveSessionState>,
}

enum LiveSessionMessage {
    SessionStarted { session_id },
    Progress { event: FitProgress },
    FitFailed { message },
    FinalReportReady { result_payload, report_generated_at },
    ReportFailed { message },
    SessionClosed,
}
```

Key features:
- **Non-blocking TCP listener** - Accepts client connections without blocking the fit
- **Message backlog** - Up to 1000 messages for reconnecting clients
- **Fit control source** - Clients can send `FitControl` commands (pause, resume, stop)
- **Graceful shutdown** - Publishes `SessionClosed` on cleanup

### 3.4 Fit Progress Integration

In `executor.rs`, when a live session is active:

```rust
let live_result = builder.fit_with_progress_and_control_in_memory(
    |event| {
        session.publish_progress(event)  // Forward PMcore events to client
    },
    || session.next_control(),           // Check for client commands
);
```

This creates a real-time bridge: PMcore → Rust → TCP → Shiny App → User's browser.

### 3.5 Live Session R6 Class

```r
LiveSession <- R6::R6Class("LiveSession", public = list(
    initialize = function(host, port, timeout_ms = 5000L),
    connect = function(),
    close = function(),
    wait_connected = function(timeout_ms),
    publish_result = function(result_payload, generated_at),
    publish_failure = function(message)
))
```

---

## 4. Reporting System

### 4.1 PM_report (`R/PM_report.R`)

The `PM_report()` function is the entry point for generating reports from a `PM_result`:

```r
PM_report <- function(x, template, path, show = TRUE, quiet = TRUE)
```

**Report modes:**

| Mode | Behavior |
|------|----------|
| `"app"` | Launches PmetricsReports Shiny app (default) |
| `"plotly"` | Generates legacy Plotly HTML report |
| `"ggplot"` | Generates legacy ggplot HTML report |
| `"ggplot_rust"` | Generates report with rust-backed rendering |
| `"none"` | No-op, returns 0 |

**Fallback chain:**
1. Try to load `PmetricsReports` package (sibling package)
2. If that fails and mode is `"app"`, fall back to `"ggplot"` HTML generation
3. HTML generation uses Rmd templates from `inst/report/templates/`

### 4.2 PmetricsReports Package

Located at `Pmetricsreports/` (sibling to Pmetrics). This is a standalone R package containing:

- **Shiny app** - Interactive visualization of PM_result objects
- **`run_app()` function** - Entry point, accepts `res`, `launch.browser`, `background`, `live_session`
- **Live session integration** - Connects to the Rust TCP session for real-time updates

The app resolves via:
```r
pmetrics_path → ../Pmetricsreports/ → pkgload::load_all() → run_app
```

### 4.3 Legacy HTML Reports

When the app is unavailable, Pmetrics falls back to R Markdown templates:

- `inst/report/templates/plotly.Rmd` - Plotly-based interactive reports
- `inst/report/templates/ggplot.Rmd` - ggplot2-based static reports

These are rendered using `rmarkdown::render()` to produce standalone HTML files.

---

## 5. Data Flow Summary

```
┌─────────────────────────────────────────────────────────────────────┐
│                        USER INTERACTION                              │
│  R: mod <- PM_model$new(list(pri=..., eqn=..., out=..., err=...))   │
│  R: data <- PM_data$new("data.csv")                                 │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      DSL VALIDATION & COMPILATION                    │
│  PM_model$initialize():                                              │
│    ├─ Check reserved names                                           │
│    ├─ Detect model type (ODE vs Analytical)                         │
│    ├─ Validate index literals for ODEs                              │
│    ├─ Verify required parameters for templates                      │
│    └─ PM_model$compile() → stores arg_list                          │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         FIT EXECUTION                                 │
│  PM_model$fit(data, cycles, prior, points, ...):                    │
│    ├─ Validate data/model compatibility                             │
│    ├─ Build fit parameters (ranges, error models, prior)            │
│    ├─ extendr → Rust compile_runtime_model()                        │
│    │     └─ dsl::compile_module_source_to_runtime()                  │
│    │          └─ PMcore compile/ (validation, caches, etc.)         │
│    ├─ Rust executor::fit() → PMcore estimation                      │
│    │     ├─ Build NonparametricEstimationProblemBuilder              │
│    │     ├─ Run NPAG/NPOD/POSTPROB algorithm                        │
│    │     └─ FitPayload::from_fit_result()                           │
│    └─ R: decode_fit_payload() → PM_result                           │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                        RESULT CONSTRUCTION                            │
│  build_pm_result_from_fit_payload():                                 │
│    ├─ PM_pop (population predictions)                                │
│    ├─ PM_post (individual posteriors)                                │
│    ├─ PM_final (theta, support points)                               │
│    ├─ PM_cycle (cycle-by-cycle metrics)                              │
│    ├─ PM_op (individual parameters)                                  │
│    ├─ PM_cov (covariate data)                                        │
│    └─ save_pm_result_snapshot() → PMout.Rdata                        │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                        REPORTING                                      │
│  PM_report(result, template = "app"):                                │
│    ├─ Try PmetricsReports::run_app() (Shiny)                        │
│    │     └─ Live session TCP bridge for real-time updates            │
│    └─ Fallback: rmarkdown::render() → HTML                           │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 6. Key Design Patterns & Observations

### 6.1 Hybrid R/Rust Architecture

- **extendr-api** is used for R ↔ Rust FFI (modern replacement for rpy2-style bindings)
- Rust handles: DSL compilation, ODE solving, estimation algorithms, prediction caching
- R handles: User API, data validation, result object construction, reporting
- Communication: JSON serialization for fit payloads, TCP for live sessions

### 6.2 Compile-Time Dimension Inference

ODE models require **literal indices** (e.g., `dx[1]`, `x[2]`) rather than dynamic ones (e.g., `dx[i]`). This allows the Rust `ode!` macro to:
- Infer the number of states, drugs, and outputs at compile time
- Generate type-safe, zero-cost abstractions
- Avoid runtime bounds checking

### 6.3 Three Algorithm Variants

| Algorithm | Description | Use Case |
|-----------|-------------|----------|
| NPAG | Nonparametric Adaptive Grid | Standard population analysis |
| NPOD | Nonparametric Optimization by Duality | Faster convergence for simpler models |
| POSTPROB | Posterior calculation | Bayesian posteriors with `cycles = 0` |

All three share the same builder pattern and only differ in their estimation core.

### 6.4 Prediction Caching

The `caches.rs` module implements runtime prediction caching during fitting. This avoids recomputing predictions for identical parameter sets across cycles, providing significant speedups (especially in later cycles where parameter values repeat).

### 6.5 Live Session Protocol

The live session uses a simple JSON-over-TCP protocol:
- **Server** (Rust): Publishes `FitProgress` events and accepts `FitControl` commands
- **Client** (Shiny): Subscribes to events, displays progress, sends controls
- **Backlog**: 1000-message buffer allows late-connecting clients to catch up

### 6.6 Result Snapshot System

Every successful fit saves a `PMout.Rdata` snapshot containing the full result list. This enables:
- Reproducibility (exact state at end of run)
- Fast reloading via `PM_load()`
- Debugging and inspection

### 6.7 Deprecated Paths

Several legacy patterns still exist:
- Model text files (`.txt` model definitions) - being replaced by R list definitions
- `PM_fit` class - deprecated in favor of `PM_model$fit()`
- `PM_fit$run()` - deprecated, use `PM_model$fit()`
- Legacy HTML reports - being replaced by PmetricsReports app

---

## 7. File Reference Map

### R Files

| File | Role |
|------|------|
| `R/PM_model.R` | PM_model R6 class, model definition, validation, fit() |
| `R/PM_data.R` | PM_data R6 class, data loading and validation |
| `R/PM_fit.R` | PM_fit R6 class (deprecated wrapper) |
| `R/PM_fit_payload.R` | Fit payload decoding, PM_result construction |
| `R/PM_cycle.R` | Cycle information R6 object |
| `R/PM_live.R` | Live session R6 class, TCP communication |
| `R/PM_report.R` | Report generation entry point |
| `R/model_dsl.R` | DSL AST analysis utilities |
| `R/mod_lib.R` | Model library templates and browsing |
| `R/PM_parse.R` | Parser for Rust NPAG output files |
| `R/PM_result.R` | PM_result R6 class (result container) |
| `R/PM_pop.R` | Population predictions R6 object |
| `R/PM_post.R` | Posterior estimates R6 object |
| `R/PM_final.R` | Final theta/support points R6 object |
| `R/PM_op.R` | Individual parameters R6 object |
| `R/PM_cov.R` | Covariate data R6 object |

### Rust Files (Pmetrics crate)

| File | Role |
|------|------|
| `src/rust/src/lib.rs` | extendr entry points, compile_runtime_model, simulate |
| `src/rust/src/executor.rs` | fit() orchestration, live session integration |
| `src/rust/src/settings.rs` | R parameter → PMcore builder translation |
| `src/rust/src/fit_payload.rs` | FitResult → JSON serialization |
| `src/rust/src/live.rs` | Live session TCP server, message protocol |
| `src/rust/src/simulation.rs` | Simulation row construction |
| `src/rust/src/logs.rs` | Tracing/log configuration |

### Rust Files (PMcore crate)

| Path | Role |
|------|------|
| `src/compile/` | DSL compilation, validation, caching |
| `src/model/` | Equation types, parameter space, covariate models |
| `src/estimation/nonparametric/` | NPAG algorithm implementation |
| `src/estimation/parametric/` | NPOD algorithm implementation |
| `src/api/` | Public API: estimation problem builder, progress, fit control |
| `src/results/` | FitResult, predictions, summary, diagnostics |
| `src/routines/initialization/` | Support point initialization (Sobol, etc.) |

---

## 8. Potential Improvements

1. **Remove deprecated paths** - Clean up `PM_fit`, model text files, legacy HTML templates
2. **Better error messages** - DSL compilation errors could include line numbers and context
3. **Parallel compilation** - Multiple models could be compiled in parallel
4. **Cached compilation** - DSL compilation results could be cached by source hash
5. **Live session persistence** - Live sessions could survive R session restarts
6. **Typed fit payload** - Instead of JSON → tibble, use a binary format (e.g., Arrow) for performance
7. **Model versioning** - Track model DSL schema versions for backward compatibility
