# Compare runs

` r lifecycle::badge("stable")`

Compare convergence, -2\*log likelihood, AIC/BIC, bias, imprecision, and
regression statistics of population and posterior predictions.
Additionally, compare distributions of support points between models
(see details)

## Usage

``` r
PM_compare(..., icen = "median", outeq = 1, plot = FALSE)
```

## Arguments

- ...:

  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
  objects to compare. See details.

- icen:

  Can be either "median" for the predictions based on medians of
  `pred.type` parameter value distributions, or "mean". Default is
  "median".

- outeq:

  Number of the output equation to compare; default is 1.

- plot:

  Boolean indicating whether to generate and open the comparison report;
  default is FALSE

## Value

A highlighted table comparing the selected models with the following
columns. In each metric column, the best (lowest) value is highlighted
in red. In the final best column, the red highlighting applies to the
model with the most "best" metrics.

- **run** The run number of the data

- **nvar** Number of random parameters in the model

- **converged** Boolean value if convergence occurred.

- **-2\*ll** Final cycle -2\*Log-likelihood

- One of the following, depending on the option set in
  [setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md):

  - **aic** Final cycle Akaike Information Criterion OR

  - **bic** Final cycle Bayesian (Schwartz) Information Criterion

- **popBias** Bias, calculated by the method set in
  [setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md),
  of the predictions based on `icen` population parameters

- **popImp** Imprecision, calculated by the method set in
  [setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md),
  of the predictions based on `icen` population parameters

- **postBias** Bias, calculated by the method set in
  [setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md),
  of the predictions based on `icen` posterior parameters

- **postImp** Imprecision, calculated by the method set in
  [setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md),
  of the predictions based on `icen` posterior parameters

- **popInt** Intercept of observed vs. population predicted values
  regression

- **postInt** Intercept of observed vs. posterior predicted values
  regression

- **popSl** Slope of observed vs. population predicted values regression

- **postSl** Slope of observed vs. posterior predicted values regression

- **popR2** R-squared of observed vs. population predicted values
  regression

- **postR2** R-squared of observed vs. posterior predicted values
  regression

- **pval** P-value for each model compared to the first. See details.

- **best** Number of times each model was the best (lowest) in the above
  bias/imprecision, likelihood, and regression metrics.

## Details

Objects can be specified separated by commas, e.g.
`PM_compare(run1, run2, run3)`. P-values are based on comparison using
the nearest neighbors approach if all models are non-parametrics. Models
may only be compared on parameters that are included in the first model.
The P-value is the comparison between each model and the first model in
the list. Missing P-values are when a model has no parameter names in
common with the first model, and for the first model compared to itself.
Significant P-values indicate that the null hypothesis should be
rejected, i.e. the joint distributions between the two compared models
for that parameter are significantly different.

## See also

[PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md)

## Author

Michael Neely
