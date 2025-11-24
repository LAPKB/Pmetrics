# Create a new Pmetrics folder tree

**\[stable\]**

Sets up a directory tree for a new Pmetrics project

## Usage

``` r
PM_tree(project = "NewProject", path = getwd())

PMtree(...)
```

## Arguments

- project:

  A character string of a new project name, e.g. "DrugX"

- path:

  The full path to the root folder for the new project. Default is the
  current working directory.

- ...:

  Arguments passed to PM_tree.

## Value

A new folder named `project` with the following subfolders:

- Rscript :

  The folder for the Rscript containing all run instructions. Within
  this folder will be a skeleton R script for the project.

- Runs :

  The folder for all Pmetrics runs. Put run files, i.e. a data file and
  a model file in this directory prior to each run.

- Sim :

  The folder for all simulations related to the project.

- src :

  The folder for source data files in their original format, to preserve
  integrity and for audit purposes.

## Details

This function will create a new project folder tree with appropriate
subfolders and a skeleton R script.

## See also

[PM_manual](https://lapkb.github.io/Pmetrics_rust/reference/PM_manual.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
PM_tree("DrugX")
} # }
```
