# Introduction

Thank you for your interest in Pmetrics! This guide provides
instructions and examples to assist users of the Pmetrics R package, by
the Laboratory of Applied Pharmacokinetics and Bioinformatics at the
Saban Research Institute, Children’s Hospital Los Angeles, and the
Department of Pediatrics, Keck School of Medicine, University of
Southern California. Please see our website at
[*http://www.lapk.org*](http://www.lapk.org) for more information.

## 1 Rust

**As of v. 3.0**, Pmetrics uses Rust for the core of the package and no
longer uses Fortran. This is for several reasons, including the fact
that Rust is a modern programming language that is more efficient and
easier to maintain than Fortran. Rust is memory safe, which means that
it prevents common programming errors that can lead to memory leaks and
crashes. This makes it a more reliable choice for high-performance
computing tasks. Additionally, Rust has a growing ecosystem of libraries
and tools that make it easier to work with data and perform complex
computations and a more flexible choice for a wide range of
applications.

## 2 R6 architecture

**As of v. 2.0**, Pmetrics uses an “R6” architecture less dependent on
reading and writing files, preserving hard drive space and speeding
execution, since file I/O is slow. The goal is to simplfy work within a
session by eliminating the need to repeatedly copy and paste files from
one folder to the next whenever a new fit is executed. Storing critical
objects in memory means they can be used without accessing files.

However, data and model files are still used for longer term storage and
preserving work from one session to another. The format of those files,
both data and model, will be familiar to long term Pmetrics users.

Data files are generally .csv and the format is detailed in
[data](https://lapkb.github.io/Pmetrics_rust/articles/data.md)\]. A
model can be read from a text file or can be defined directly in R. The
easiest way to accomplish this is with our model builder app. Whether
choosing to define models in R, with the builder app, or in a .txt file,
details can be found in
[models](https://lapkb.github.io/Pmetrics_rust/articles/models.md).

**Tips for using this guide.**

- Items that are hyperlinked can be selected to cross reference within
  this manual or link to external sites.

- `Items` correspond to inline examples of R code, which are not
  evaluated in this document, but serve as templates for what may be
  typed into your R console or script. They may not necessarily be
  executable if typed verbatim.

## 3 Disclaimer

You, the user, assume all responsibility for acting on the results
obtained from Pmetrics. The Laboratory of Applied Pharmacokinetics and
Bioinformatics (LAPKB), members and consultants to LAPKB, and Children’s
Hospital Los Angeles and the University of Southern California and their
employees assume no liability whatsoever. Your use of the package
constitutes your agreement to this provision.

## 4 Installation

Pmetrics and all required components will run under Mac (Unix), Windows,
and Linux. Instructions on obtaining and installing the package are
[here](https://lapkb.github.io/Pmetrics_rust/articles/pmetrics.md).

## 5 What This Guide Is Not

We assume that the user has familiarity with population modeling and R,
and thus this manual is not a tutorial for basic concepts and techniques
in either domain. We have tried to make the R code simple, regular and
well documented. A very good free online resource for learning the
basics of R can be found at [Stat
Methods](http://www.statmethods.net/index.md).

We recognize that initial use of a new software package can be complex,
so please feel free to contact us at any time, preferably through the
[Pmetrics Disussions](https://github.com/LAPKB/Pmetrics/discussions).
You can report [issues](https://github.com/LAPKB/Pmetrics/issues) online
as well.

This manual is also not intended to be a theoretical treatise on the
algorithms Pmetrics uses. For that, the user is directed to our
[website](http://www.lapk.org/publications.php).

## 6 Getting Help and Updates

Within R, you can use `help("command")` or more simply just `?command`
in the R console to see detailed help files for any Pmetrics command.
Many commands have examples included in this documentation and you can
execute the examples with `example(command)`.

You can also type `PMmanual()` to launch this website from within
Pmetrics.

Pmetrics will check for updates automatically every time you load it
with [`library(Pmetrics)`](https://lapkb.github.io/Pmetrics_rust/) and
you are connected to the internet. If an update is available, it will
provide a brief message to inform you. You can then reinstall the
package from github.
