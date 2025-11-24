# Primary parameter values

**\[experimental\]** Define primary model parameter object. This is used
internally by the `PM_model` class.

## Public fields

- `min`:

  Minimum value of the range.

- `max`:

  Maximum value of the range.

- `mean`:

  Mean value of the range, calculated as (min + max) / 2.

- `sd`:

  Standard deviation of the range, calculated as (max - min) / 6.

## Methods

### Public methods

- [`PM_pri$new()`](#method-PM_pri-new)

- [`PM_pri$print()`](#method-PM_pri-print)

- [`PM_pri$clone()`](#method-PM_pri-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new range object.

#### Usage

    PM_pri$new(min, max)

#### Arguments

- `min`:

  Minimum value of the range.

- `max`:

  Maximum value of the range.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the range.

#### Usage

    PM_pri$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_pri$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
