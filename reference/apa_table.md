# Generic APA-style table formatter

S3 generic for converting analysis objects into publication-ready
APA-style tables. The default behavior is dispatched to class-specific
methods (e.g., `apa_table.mcq_analysis`). Output formats include data
frame, markdown, HTML, and LaTeX for direct inclusion in manuscripts.

## Usage

``` r
apa_table(x, format = c("data.frame", "markdown", "html", "latex"), ...)
```

## Arguments

- x:

  An object of an appropriate class (e.g., `mcq_analysis`).

- format:

  One of `"data.frame"`, `"markdown"`, `"html"`, or `"latex"`.

- ...:

  Additional arguments passed to methods.

## Value

A formatted table object whose type depends on `format`.
