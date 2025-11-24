# Final Cycle Population Values

**\[stable\]**

Contains final cycle information from run.

## Details

The PM_final object is both a data field within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
and itself an R6 object comprising data fields and associated methods
suitable for analysis and plotting of final cycle parameters.

Because PM_final objects are automatically added to the
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
at the end of a successful run, it is generally not necessary for users
to generate PM_final objects themselves.

The main results are contained in the `$data` field, and it is this
field which is passed to the `$plot` and `$summary` methods. You can use
this `$data` field for custom manipulations, e.g.
`probs <- run1$final$data$popPoints %>% select(prob)`. This will select
the probabilities of the support points. If you are unfamiliar with the
`%>%` pipe function, please type `help("%>%", "magrittr")` into the R
console and look online for instructions/tutorials in tidyverse, a
powerful approach to data manipulation upon which Pmetrics is built.

To provide a more traditional experience in R, the `$data` field is also
separated by list items into the other data fields within the R6 object,
e.g. `popMean` or `nsub`. This allows you to access them in an S3 way,
e.g. `run1$final$popMean` if `run1` is a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object.

## Author

Michael Neely, Julian Otalvaro

## Public fields

- `data`:

  A list with the following elements, which can also be extracted by
  name.

  - **popPoints** (NPAG only) Data frame of the final cycle joint
    population density of grid points with column names equal to the
    name of each random parameter plus *prob* for the associated
    probability of that point

  - **popMean** The final cycle mean for each random parameter
    distribution

  - **popSD** The final cycle standard deviation for each random
    parameter distribution

  - **popCV** The final cycle coefficient of variation (SD/Mean) for
    each random parameter distribution

  - **popVar** The final cycle variance for each random parameter
    distribution

  - **popCov** The final cycle random parameter covariance matrix

  - **popCor** The final cycle random parameter correlation matrix

  - **popMed** The final cycle median values for each random parameter,
    i.e. those that have unknown mean and unknown variance, both of
    which are fitted during the run

  - **popRanFix** The final cycle median values for each parameter that
    is random but fixed to be the same for all subjects, i.e. unknown
    mean, zero variance, with only mean fitted in the run

  - **postPoints** (NPAG only) Data frame of posterior population points
    for each of the first 100 subject, with columns id, point,
    parameters and probability. The first column is the subject, the
    second column has the population point number, followed by the
    values for the parameters in that point and the probability.

  - **postMean** A *nsub* x *npar* data frame containing the means of
    the posterior distributions for each parameter.

  - **postSD** A *nsub* x *npar* data frame containing the SDs of the
    posterior distributions for each parameter.

  - **postVar** A *nsub* x *npar* data frame containing the variances of
    the posterior distributions for each parameter.

  - **postCov** NPAG only: An list of length *nsub*, each element with
    an *npar* x *npar* data frame that contains the posterior parameter
    value covariances for that subject.

  - **postCor** NPAG only: An list of length *nsub*, each element with
    an *npar* x *npar* data frame that contains the posterior parameter
    value correlations for that subject.

  - **postMed** A *nsub* x *npar* data frame containing the medians of
    the posterior distributions for each parameter.

  - **shrinkage** A data frame with the shrinkage for each parameter.

  - **gridpts** (NPAG only) Initial number of support points

  - **nsub** Number of subjects

  - **ab** Tibble/data frame of boundaries for random parameter values
    with columns: name, lower, upper.

## Active bindings

- `popPoints`:

  (NPAG only) Data frame of the final cycle joint population density of
  grid points with column names equal to the name of each random
  parameter plus *prob* for the associated probability of that point

- `popMean`:

  The final cycle mean for each random parameter distribution

- `popSD`:

  The final cycle standard deviation for each random parameter
  distribution

- `popCV`:

  The final cycle coefficient of variation (SD/Mean) for each random
  parameter distribution

- `popVar`:

  The final cycle variance for each random parameter distribution

- `popCov`:

  The final cycle random parameter covariance matrix

- `popCor`:

  The final cycle random parameter correlation matrix

- `popMed`:

  The final cycle median values for each random parameter, i.e. those
  that have unknown mean and unknown variance, both of which are fitted
  during the run

- `popRanFix`:

  The final cycle median values for each parameter that is random but
  fixed to be the same for all subjects, i.e. unknown mean, zero
  variance, with only mean fitted in the run

- `postPoints`:

  (NPAG only) Data frame of posterior population points for each of the
  first 100 subject, with columns id, point, parameters and probability.
  The first column is the subject, the second column has the population
  point number, followed by the values for the parameters in that point
  and the probability.

- `postMean`:

  A *nsub* x *npar* data frame containing the means of the posterior
  distributions for each parameter.

- `postSD`:

  A *nsub* x *npar* data frame containing the SDs of the posterior
  distributions for each parameter.

- `postVar`:

  A *nsub* x *npar* data frame containing the variances of the posterior
  distributions for each parameter.

- `postCov`:

  NPAG only: An list of length *nsub*, each element with an *npar* x
  *npar* data frame that contains the posterior parameter value
  covariances for that subject.

- `postCor`:

  NPAG only: An list of length *nsub*, each element with an *npar* x
  *npar* data frame that contains the posterior parameter value
  correlations for that subject.

- `postMed`:

  A *nsub* x *npar* data frame containing the medians of the posterior
  distributions for each parameter.\*

- `shrinkage`:

  A data frame with the shrinkage for each parameter. The total
  population variance for a parameter is comprised of variance(EBE) plus
  average variance(EBD), where each subject's EBE is the Empirical Bayes
  Estimate or mean posterior value for the parameter. EBD is the
  Empirical Bayes Distribution, or the full Bayesian posterior parameter
  value distribution for each subject.

  The typical definition of \\\eta\\ shrinkage is \\\[1 -
  \frac{SD(\eta)}{\omega}\]\\ or \\\[1 - \frac{var(\eta)}{\omega^2}\]\\,
  where \\\eta\\ is the EBE and \\\omega^2\\ is the population variance
  of \\\eta\\.

  In parametric modeling approaches \\\eta\\ is the interindividual
  variability around the typical (mean) value of the parameter in the
  population, usually referred to as \\\theta\\. In nonparametric
  approaches, there is no assumption of normality, so \\\eta\\ simply
  becomes each subject's mean parameter value estimate.

  Here is how Pmetrics derives and then calculates shrinkage for a given
  parameter. \$\$popVar = var(EBE) + mean(var(EBD))\$\$ \$\$1 =
  \frac{var(EBE)}{popVar} + \frac{mean(var(EBD)}{popVar}\$\$ \$\$1 -
  \frac{var(EBE)}{popVar} = \frac{mean(var(EBD))}{popVar}\$\$
  \$\$shrinkage = \frac{mean(var(EBD))}{popVar}\$\$ Shrinkage is
  therefore a fraction between 0 and 1. If Bayesian posterior
  distributions are wide for a given parameter and \\mean(var(EBD))\\ is
  high due to sparse or uninformative sampling, then most of the
  population variance is due to this variance and shrinkage is high,
  i.e., individual posterior estimates (EBE) shrink towards the
  population mean. Be aware, however, that a Bayesian posterior
  parameter value distribution for a given subject who is sparsely
  sampled may also be a single support point with no variance. Therefore
  EBD under nonparametric assumptions is not always large with
  uninformative sampling. This means that shrinkage is not as readily
  interpretable in nonparametric population modeling.

  An alternative is to consider the number of support points relative to
  the number of subjects. Highly informed, distinct subjects will result
  in the maximum possible number of support points, *N*, which is the
  same as the number of subjects. In contrast, badly undersampled
  subjects can result in only one support point. There is no formal
  criterion for this statistic, but it can be used in combination with
  shrinkage to assess the information content of the data.

- `gridpts`:

  (NPAG only) Initial number of support points

- `nsub`:

  Number of subjects

- `ab`:

  Matrix of boundaries for random parameter values

## Methods

### Public methods

- [`PM_final$new()`](#method-PM_final-new)

- [`PM_final$plot()`](#method-PM_final-plot)

- [`PM_final$summary()`](#method-PM_final-summary)

- [`PM_final$clone()`](#method-PM_final-clone)

------------------------------------------------------------------------

### Method `new()`

Create new object populated with final cycle information

#### Usage

    PM_final$new(PMdata = NULL, path = ".", ...)

#### Arguments

- `PMdata`:

  include Saved, parsed output of prior run, used when source files are
  not available. .

- `path`:

  include Path to the folder containing the raw results of the run.
  Default is the current working directory. .

- `...`:

  Not currently used.

#### Details

Creation of new `PM_final` object is automatic and not generally
necessary for the user to do.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot method

#### Usage

    PM_final$plot(...)

#### Arguments

- `...`:

  Arguments passed to
  [plot.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_final.md)

#### Details

See
[plot.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_final.md).

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summary method

#### Usage

    PM_final$summary(...)

#### Arguments

- `...`:

  Arguments passed to
  [summary.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_final.md)

#### Details

See
[summary.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_final.md).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_final$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
