# Modify a list with another list, allowing NULL values

**\[stable\]** Version of
[`utils::modifyList()`](https://rdrr.io/r/utils/modifyList.html) that
works with lists which have unnamed elements.

## Usage

``` r
modifyList2(x, val, keep.null = FALSE)
```

## Arguments

- x:

  A list to be modified.

- val:

  A list of values to modify `x`.

- keep.null:

  A logical value indicating whether to keep NULL values in `val`.
  Default is FALSE.

## Value

A modified list, as in
[`utils::modifyList()`](https://rdrr.io/r/utils/modifyList.html).
