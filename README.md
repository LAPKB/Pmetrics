

# Pmetrics <a href="https://lapkb.github.io/Pmetrics/"><img src="man/figures/Pmetrics_logo.png" align="right" height="100" width = "100" alt="Pmetrics" /></a>

> [!WARNING]
>
> This is a development branch of Pmetrics using a new backend written
> in Rust. The repository for the current stable release is
> [LAPKB/Pmetrics](https://github.com/LAPKB/Pmetrics).

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

<a href="https://r-project.org"><img src="man/figures/Rlogo.png" align="left" height="40" width="40" alt="R" /></a><br><br>
You can download and install **R** from the [Comprehensive R Archive
Network (CRAN)](https://cran.r-project.org/). In addition, we *strongly*
recommend a third-party integrated development environment (IDE) to use
R and Pmetrics rather than the rudimentary IDE that comes with R. We use
all the IDEs below when developing and working with Pmetrics.

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

<a href="https://rust-lang.org"><img src="man/figures/rust-logo-64x64.png" align="left" height="40" width = "40" alt="Rust" /></a><br><br>
**Rust** is the language used by pharmsol for the backend of Pmetrics
and all the heavy “number crunching”. Rust is modern, fast, reliable,
widely used, and extremely well supported. It works on all major
operating systems.

Follow the instructions on the [Rust
website](https://www.rust-lang.org/tools/install) to install Rust on
your system.

<img src="man/figures/Pmetrics_logo.png" align="left" height="40" width = "40" alt="Pmetrics" /><br><br>
The compiled versions of **Pmetrics** for MacOS, Windows and Linux are
hosted on [r-universe](https://lapkb.r-universe.dev/Pmetrics). Follow
these easy steps to install the correct version for your system.

1.  Add the LAPKB r-universe repository to your list of repositories by
    adding this code to your `.Rprofile` script, which ensures that R
    will always include the LAPKB repository when installing or updating
    packages, even after you restart or update R.

    ``` r
    options(repos = c(
        CRAN = "https://cloud.r-project.org",
        LAPKB = "https://lapkb.r-universe.dev"
    ))
    ```

    If you don’t have an `.Rprofile` script, you can create one in your
    home directory. This only needs to be done once.

    <details>

    <summary>

    Mac

    </summary>

    You can create or edit the `.Rprofile` file using TextEdit or any
    text editor. Make sure to save it as a plain text file named
    `.Rprofile` in your home directory, i.e.,
    `/Users/yourusername/.Rprofile`. Do not add a `.txt` extension. To
    open/edit your `.Rprofile` later, type `open -e ~/.Rprofile` in the
    Terminal.

    </details>

    <details>

    <summary>

    Windows

    </summary>

    You can create or edit the `.Rprofile` file using Notepad or any
    text editor. Make sure to save it as a plain text file named
    `.Rprofile` in your home directory, i.e.,
    `C:\Users\yourusername\.Rprofile`. Do not add a `.txt` extension. To
    open/edit your `.Rprofile` later, type
    `notepad %USERPROFILE%\.Rprofile` in the Command Prompt.

    </details>

    <details>

    <summary>

    Linux

    </summary>

    You can create or edit the `.Rprofile` file using any text editor,
    such as `nano` or `vim`. Make sure to save it as a plain text file
    named `.Rprofile` in your home directory, i.e.,
    `/home/yourusername/.Rprofile`. To open/edit your `.Rprofile` later,
    type `nano ~/.Rprofile` or `vim ~/.Rprofile` in the terminal.

    </details>

2.  After you have run the above command once, you can install or update
    previously installed Pmetrics by running this command in R:

    ``` r
    install.packages("Pmetrics")
    ```

    This command is the same as for any other R package hosted on CRAN.
    By having the LAPKB repository in your list of repositories, R will
    automatically find and install/update the correct version of
    Pmetrics for your system.

    <details>

    <summary>

    What if I couldn’t add the LAPKB repository to my .Rprofile?

    </summary>

    You can still install Pmetrics by specifying the repository directly
    in the `install.packages` function like this:

    ``` r
    install.packages("Pmetrics", repos = "https://lapkb.r-universe.dev")
    ```

    However, you will need to include the `repos` argument each time you
    want to install or update Pmetrics.

    </details>

## Documentation

We encourage you to visit our
[website](https://lapkb.github.io/Pmetrics/) to learn about Pmetrics.
