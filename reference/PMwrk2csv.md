# Convert Old .wrk Files to .csv Matrix File

**\[stable\]**

Convert old-style, USC\*PACK single drug working copy files into a
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
object and write a .csv file to the current working directory.

## Usage

``` r
PMwrk2csv(prefix, ext = NULL, nsub)
```

## Arguments

- prefix:

  The alphabetic prefix of the working copy files to be converted, as a
  character vector.

- ext:

  The extension of the working copy files files, if it exists. Does not
  have to be specified.

- nsub:

  The number of subjects, or working copy files to read.

## Value

A new file will be created with the name equal to `prefix` and an
extension of "csv".

## Details

This function will determine if the working copy files are old and
convert them. New, multi-drug working copy files will be ignored. IDs
will be suffixed with .1 to .9 for \<10 subjects, .01 to .99 for \<100
subjects and .001 to .999 for \<1000 subjects, as needed to ensure
unique ID numbers.

## Author

Michael Neely
