# Create a Pmetrics Project (RStudio Project Template)

This function is called by RStudio when creating a new Pmetrics project
via the "New Project..." wizard. It creates the project directory
structure and an RStudio project file.

## Usage

``` r
create_pmetrics_project(path, ...)
```

## Arguments

- path:

  The path to the newly created project directory (provided by RStudio).

- ...:

  Additional arguments passed from the project template dialog.

  project_name

  :   The name for the project (used in the Analysis.R script).

## Value

Invisibly returns `NULL`. Called for its side effects of creating the
project directory structure.
