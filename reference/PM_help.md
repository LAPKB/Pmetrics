# Get Help and Report Issues

**\[stable\]**

This function displays system information useful for debugging and
provides a link to the GitHub repository for reporting bugs or
requesting help.

## Usage

``` r
PM_help(copy = TRUE)
```

## Arguments

- copy:

  Logical. If `TRUE`, copies the system information to clipboard.
  Default is `TRUE`.

## Value

Invisibly returns a list containing the system information.

## Details

When you encounter bugs or need help, this function collects relevant
system information that will help maintainers diagnose the issue. The
information includes OS version, R version, RStudio version (if
applicable), package version, Rust and Cargo versions (if available).

## Examples

``` r
if (FALSE) { # \dontrun{
# Display help information
PM_help()

# Copy information to clipboard
PM_help(copy = TRUE)
} # }
```
