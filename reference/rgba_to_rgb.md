# Convert RGBA to RGB

**\[stable\]** Converts a CSS `rgba()` string to an RGB color string.

## Usage

``` r
rgba_to_rgb(rgba_str, alpha = NULL)
```

## Arguments

- rgba_str:

  A string in the format `rgba(r, g, b, a)` where r, g, and b are
  integers between 0 and 255, and a is a float between 0 and 1.

- alpha:

  Optional numeric value to replace the alpha channel in the rgba
  string.
