

# Pmetrics <a href="https://lapkb.github.io/Pmetrics/"><img src="man/figures/Pmetrics_logo.png" align="right" height="100" width = "100" alt="Pmetrics" /></a>

Thank you for your interest in `Pmetrics`, created by the [Laboratory of
Applied Pharmacokinetics and Bioinformatics](http://www.lapk.org).
`Pmetrics` is a library package for R to perform non-parametric and
parametric pharmacokinetic-pharmacodynamic population and individual
modeling and simulation. It is primarily designed for pharmacometric
researchers.

`Pmetrics` is based on over 40 years of research by our laboratory and is
the most mature non-parametric method available. Nevertheless, `Pmetrics`
is dynamically evolving. Best of all, it is SHAREWARE with only a
suggested donation.


## Installation

### R (required)

You can download and install [R](https://r-project.org) from the
[Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/).

### Rust (required)

The simulation engine for `Pmetrics` is written in `Rust`, which is required to fit models.

Follow the instructions on the [Rust
website](https://www.rust-lang.org/tools/install) to install Rust on
your system.

### Pmetrics

`Pmetrics` is distributed on [r-universe](https://lapkb.r-universe.dev/Pmetrics), and built for Windows, MacOS, and Linux. To install, you must specify `r-universe` as the repository using

```r
install.packages("Pmetrics", repos = "https://lapkb.r-universe.dev")
```

For updating the package and for easier future installations, you can run the following command
to add the `r-universe` repository to your `R.profile`

```r
write('options(repos = c(CRAN = "https://cloud.r-project.org", LAPKB = "https://lapkb.r-universe.dev"))', "~/.Rprofile", append = TRUE)
```

With this setup, you can install or update Pmetrics using the
standard command `install.packages("Pmetrics")` without specifying
the repository each time.


### IDE (optional)

In addition, we *strongly* recommend a third-party integrated
development environment (IDE) to use R and Pmetrics rather than the
rudimentary IDE that comes with R. We use all the IDEs below when
developing and working with Pmetrics.

- [RStudio](https://posit.co/download/rstudio-desktop/) is the oldest
  and most stable IDE, developed by [Posit](https://posit.co). Most
  users who only program in R use Rstudio. It has a simple interface
  with many R-specific buttons and menus. It is relatively easy to learn
  and use, but does not support other programming languages.

- [Positron](https://positron.posit.co) is a newer alternative also from
  Posit. Posit are now mostly developing Positron rather than Rstudio.
  Positron primarily supports users who work in R and Python. However,
  it is built upon [VS Code](https://code.visualstudio.com), which gives
  Positron access to thousands of extensions, including multiple AI
  tools to assist with coding and data analysis, and Positron can
  support additional programming languages. It is far more powerful than
  Rstudio, but has a steeper learning curve and currently fewer
  R-specific buttons and menus. VS Code users will find Positron
  familiar.

- [VS Code](https://code.visualstudio.com) itself can be used for R, but
  it is best suited for experienced coders who also use other
  programming languages.


## Related projects

The LAPKB has developed two packages in `Rust` to power `Pmetrics`.

[`pharmsol`](https://github.com/LAPKB/pharmsol) is a Rust library for defining and solving pharmacokinetic models.

[`PMcore`](https://github.com/LAPKB/PMcore) depends on `pharmsol` to fit models to data, and implements the different algorithms which are made available in `Pmetrics`.

Pmetrics and the code that powers it is provided as open-source code.

## Contributing

We welcome contributions in the form of feature requests, raising issues, or writing code.

## Documentation

We encourage you to visit our
[website](https://lapkb.github.io/Pmetrics/) to learn about Pmetrics.
