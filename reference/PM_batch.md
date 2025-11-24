# Execute a Batch of PM_fit objects

Runs a set of PM_fit objects in order

## Usage

``` r
PM_batch(batch, batchname = "batch", ...)
```

## Arguments

- batch:

  list of PM_fit objects

- batchname:

  name of the folder in which the batch will be executed

- ...:

  other parameters passed to PM_fit\$run(...)

## Details

This function receives a batch (list) of PM_fit objects and executes
them in order
