# Save a flextable object to a file

**\[stable\]** Saves flextable objects to a file based on the `file`
attribute in the object, set when the flextable generator function is
called. Allowable file types are 'docx', 'pptx', 'html', 'png', and
'svg'.

## Usage

``` r
save_flextable(x)
```

## Arguments

- x:

  A
  [flextable::flextable](https://davidgohel.github.io/flextable/reference/flextable.html)
  object.

## Value

A message indicating the file was saved.
