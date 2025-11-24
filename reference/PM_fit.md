# Object to define and run a model and data in Pmetrics

**\[stable\]**

`PM_fit` objects comprise a `PM_data` and `PM_model` object ready for
analysis

## Details

Data and model objects can be previously created as
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md) or
[PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
objects, or created on the fly when making a new PM_fit object. PM_fit
objects contain methods to cross-check data and model objects for
compatibility, as well as to run the analysis.

## Public fields

- `data`:

  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object

- `model`:

  [PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
  object

## Methods

### Public methods

- [`PM_fit$new()`](#method-PM_fit-new)

- [`PM_fit$run()`](#method-PM_fit-run)

- [`PM_fit$save()`](#method-PM_fit-save)

- [`PM_fit$load()`](#method-PM_fit-load)

- [`PM_fit$check()`](#method-PM_fit-check)

- [`PM_fit$clone()`](#method-PM_fit-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object

#### Usage

    PM_fit$new(data = data, model = model)

#### Arguments

- `data`:

  Either the name of a
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object in memory or the quoted name of a Pmetrics data file in the
  current working directory, which will crate a
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object on the fly. However, if created on the fly, this object will
  not be available to other methods or other instances of PM_fit.

- `model`:

  Similarly, this is either the name of a
  [PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
  object in memory or the quoted name of a Pmetrics text model file in
  the current working directory. Again, if created on the fly, the
  object will not be available to other methods or other instances of
  PM_fit.

------------------------------------------------------------------------

### Method `run()`

Run a fit of model to the data (deprecated)

#### Usage

    PM_fit$run()

#### Details

The `$run` method for `PM_fit` objects has been deprecated in favor of
the `$fit` method in
[`PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md).

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save the current PM_fit object to a .rds file.

#### Usage

    PM_fit$save(file_name = "PMfit.rds")

#### Arguments

- `file_name`:

  Name of the file to be created. The default is "PMfit.rds".

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

`PM_fit` objects contain a `save` method which invokes
[saveRDS](https://rdrr.io/r/base/readRDS.html) to write the object to
the hard drive as an .rds file. This is the corresponding load function.

#### Usage

    PM_fit$load(file_name)

#### Arguments

- `file_name`:

  Name of the file to be read, the default is "PMfit.rds".

#### Returns

A `PM_fit` object.

------------------------------------------------------------------------

### Method `check()`

Checks for errors in data and model objects and agreement between them.

#### Usage

    PM_fit$check()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_fit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
