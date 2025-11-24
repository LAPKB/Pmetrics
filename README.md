

# Pmetrics <a href="https://lapkb.github.io/Pmetrics/"><img src="Images/Pmetrics_logo.png" align="right" height="139" alt="Pmetrics website" /></a>

:warning: This is a development branch of Pmetrics using a new backend
written in Rust. The repository for the current stable release is
[LAPKB/Pmetrics](https://github.com/LAPKB/Pmetrics).

Thank you for your interest in Pmetrics, created by the [Laboratory of
Applied Pharmacokinetics and Bioinformatics](http://www.lapk.org).
Pmetrics is a library package for R to perform non-parametric and
parametric pharmacokinetic-pharmacodynamic population and individual
modeling and simulation. It is primarily designed for pharmacometric
researchers.

Pmetrics is based on over 40 years of research by our laboratory and is
the most mature non-parametric method available. Nevertheless, Pmetrics
is dynamically evolving. Best of all, it is SHAREWARE with only a
suggested donation.

## Source code

The open source code for the Pmetrics R package is hosted here on
Github. LAPKB has developed two other packages,
[PMcore](https://github.com/LAPKB/PMcore) and
[pharmsol](https://github.com/LAPKB/pharmsol), which implement the core
algorithms used by Pmetrics. These packages are also open source and
available on Github.

## Installation

To install and use Pmetrics, you will need **R**, **Rust**, and the
**Pmetrics** package appropriate for your system.

<a href="https://r-project.org"><img src="Images/Rlogo.png" align="left" height="40" /></a><br><br>
You can download and install **R** from the [Comprehensive R Archive
Network (CRAN)](https://cran.r-project.org/).

<a href="https://rust-lang.org"><img src="Images/rust-logo-64x64.png" align="left" height="40" alt="Rust website" /></a><br><br>
**Rust** is the language used by pharmsol for the backend of Pmetrics
and all the heavy “number crunching”. Rust is modern, fast, reliable,
widely used, and extremely well supported. It works on all major
operating systems.

Follow the instructions on the [Rust
website](https://www.rust-lang.org/tools/install) to install Rust on
your system.

<img src="Images/Pmetrics_logo.png" align="left" height="40" /><br><br>
The compiled versions of **Pmetrics** for MacOS, Windows and Linux are
hosted on [r-universe](https://lapkb.r-universe.dev/Pmetrics). Follow
these easy steps to install the correct version for your system.

- One time only, add the LAPKB r-universe repository to your list of
  repositories by running this command in R.

``` r
options(repos = c(LAPKB = 'https://lapkb.r-universe.dev', getOption('repos')))
```

- After you have run the above command once, forever after you can
  install or updated previously installed Pmetrics by running this
  command in R: `install.packages("Pmetrics")`. This command is the same
  as for any other R package hosted on CRAN. By having the LAPKB
  repository in your list of repositories, R will automatically find and
  install/update the correct version of Pmetrics for your system.

## Documentation

We encourage you to visit the [Pmetrics
website](https://lapkb.github.io/Pmetrics/) for documentation,
tutorials, and examples.
