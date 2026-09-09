# APA-style table for an mcq_analysis object

Formats item-level results from an `mcq_analysis` object as a
publication-ready APA-style table, with optional Interpretation columns
based on conventional CTT cutoffs (Ebel & Frisbie, 1991).

## Usage

``` r
# S3 method for class 'mcq_analysis'
apa_table(
  x,
  format = c("data.frame", "markdown", "html", "latex"),
  digits = 2,
  include_interpretation = TRUE,
  ...
)
```

## Arguments

- x:

  An object of class `mcq_analysis`.

- format:

  Output format. One of `"data.frame"` (default), `"markdown"`,
  `"html"`, or `"latex"`.

- digits:

  Number of decimal places to display. Default 2.

- include_interpretation:

  Logical. If `TRUE` (default), includes columns interpreting difficulty
  and discrimination using conventional cutoffs.

- ...:

  Additional arguments passed to
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) for non
  data-frame formats.

## Value

A data frame (when `format = "data.frame"`) or a character string
formatted in the requested style.

## References

Ebel, R. L., & Frisbie, D. A. (1991). *Essentials of educational
measurement* (5th ed.). Prentice Hall.

## Examples

``` r
data(mcq_example)
result <- mcq_analysis(mcq_example$responses, mcq_example$key)
apa_table(result, format = "data.frame")
#>      Item Key Difficulty Point-biserial Discrimination D Distractor Efficiency
#> 1  item01   D       0.85           0.47             0.43                     2
#> 2  item02   D       0.86           0.44             0.37                     1
#> 3  item03   D       0.84           0.34             0.30                     2
#> 4  item04   A       0.84           0.52             0.54                     2
#> 5  item05   D       0.72           0.42             0.56                     3
#> 6  item06   B       0.70           0.47             0.59                     3
#> 7  item07   D       0.69           0.42             0.59                     3
#> 8  item08   A       0.66           0.48             0.61                     3
#> 9  item09   D       0.58           0.47             0.69                     3
#> 10 item10   C       0.56           0.49             0.72                     3
#> 11 item11   B       0.57           0.51             0.65                     3
#> 12 item12   A       0.55           0.47             0.67                     3
#> 13 item13   C       0.50           0.52             0.70                     3
#> 14 item14   B       0.50           0.49             0.74                     3
#> 15 item15   D       0.42           0.53             0.76                     3
#> 16 item16   A       0.40           0.43             0.63                     3
#> 17 item17   A       0.47           0.48             0.69                     3
#> 18 item18   D       0.42           0.46             0.70                     3
#> 19 item19   A       0.49           0.57             0.76                     3
#> 20 item20   D       0.44           0.52             0.74                     3
#> 21 item21   B       0.38           0.42             0.65                     3
#> 22 item22   B       0.32           0.45             0.61                     3
#> 23 item23   D       0.41           0.53             0.70                     3
#> 24 item24   D       0.40           0.52             0.74                     3
#> 25 item25   A       0.26           0.39             0.52                     3
#> 26 item26   C       0.38           0.29             0.39                     3
#> 27 item27   D       0.30           0.25             0.30                     3
#> 28 item28   A       0.30           0.19             0.26                     3
#> 29 item29   C       0.46          -0.40            -0.41                     1
#> 30 item30   D       0.50          -0.29            -0.30                     1
#>    Difficulty Level Discrimination
#> 1          Moderate      Excellent
#> 2          Moderate      Excellent
#> 3          Moderate           Good
#> 4          Moderate      Excellent
#> 5          Moderate      Excellent
#> 6          Moderate      Excellent
#> 7          Moderate      Excellent
#> 8          Moderate      Excellent
#> 9          Moderate      Excellent
#> 10         Moderate      Excellent
#> 11         Moderate      Excellent
#> 12         Moderate      Excellent
#> 13         Moderate      Excellent
#> 14         Moderate      Excellent
#> 15         Moderate      Excellent
#> 16         Moderate      Excellent
#> 17         Moderate      Excellent
#> 18         Moderate      Excellent
#> 19         Moderate      Excellent
#> 20         Moderate      Excellent
#> 21         Moderate      Excellent
#> 22         Moderate      Excellent
#> 23         Moderate      Excellent
#> 24         Moderate      Excellent
#> 25             Hard           Good
#> 26         Moderate       Marginal
#> 27         Moderate       Marginal
#> 28         Moderate           Poor
#> 29         Moderate           Poor
#> 30         Moderate           Poor
```
