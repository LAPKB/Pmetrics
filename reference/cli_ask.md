# Ask with warning

Get user input in warning situation

## Usage

``` r
cli_ask(text, prompt = ">> ", ...)
```

## Arguments

- text:

  The warning text.

- prompt:

  The prompt preceding user input. Default is "\>\>".

- ...:

  Additional parameters which could be passed to
  [cli::cli_text](https://cli.r-lib.org/reference/cli_text.html).

## Value

The value of the user response

## Details

Combines the
[cli::cli_text](https://cli.r-lib.org/reference/cli_text.html) function
with [readline](https://rdrr.io/r/base/readline.html).
