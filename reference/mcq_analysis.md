# Comprehensive multiple-choice item analysis

Runs the full classical test theory item analysis on a multiple-choice
response matrix and returns a tidy `mcq_analysis` object containing
per-item difficulty, discrimination (both point-biserial and the
upper-lower 27 percent index), distractor efficiency, and the full
per-option distractor analysis. The returned object has dedicated
[`print()`](https://rdrr.io/r/base/print.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html), and
[`apa_table()`](https://rafhq1403.github.io/mcqAnalysis/reference/apa_table.md)
methods.

## Usage

``` r
mcq_analysis(responses, key, options = NULL, min_proportion = 0.05)
```

## Arguments

- responses:

  A matrix or data frame of student responses, with students in rows and
  items in columns.

- key:

  A vector of correct answers with length equal to the number of items.

- options:

  Optional character vector listing all possible response options. If
  `NULL` (default), inferred from the data.

- min_proportion:

  Minimum proportion of examinees selecting a distractor for it to be
  considered functioning when computing distractor efficiency. Default
  0.05.

## Value

An object of class `mcq_analysis` (a list) with components:

- `items`:

  Data frame with one row per item summarizing difficulty,
  point-biserial, discrimination index, and distractor efficiency.

- `distractors`:

  Data frame with full per-option distractor analysis (one row per
  item-option combination).

- `total_scores`:

  Numeric vector of total test scores, one per student.

- `n_students`:

  Number of students.

- `n_items`:

  Number of items.

- `key`:

  Answer key.

## Examples

``` r
data(mcq_example)
result <- mcq_analysis(mcq_example$responses, mcq_example$key)
result
#> Multiple-Choice Item Analysis
#> ------------------------------
#> Students: 200 
#> Items:    30 
#> Mean total score: 15.765  (SD = 6.342 )
#> 
#> Item-level statistics:
#>    item key difficulty point_biserial discrimination_index
#>  item01   D      0.850          0.472                0.426
#>  item02   D      0.860          0.440                0.370
#>  item03   D      0.845          0.336                0.296
#>  item04   A      0.840          0.515                0.537
#>  item05   D      0.720          0.423                0.556
#>  item06   B      0.695          0.465                0.593
#>  item07   D      0.690          0.424                0.593
#>  item08   A      0.660          0.484                0.611
#>  item09   D      0.580          0.471                0.685
#>  item10   C      0.565          0.486                0.722
#>  item11   B      0.570          0.508                0.648
#>  item12   A      0.550          0.467                0.667
#>  item13   C      0.495          0.519                0.704
#>  item14   B      0.505          0.486                0.741
#>  item15   D      0.425          0.530                0.759
#>  item16   A      0.395          0.431                0.630
#>  item17   A      0.465          0.477                0.685
#>  item18   D      0.420          0.458                0.704
#>  item19   A      0.490          0.573                0.759
#>  item20   D      0.440          0.522                0.741
#>  item21   B      0.375          0.424                0.648
#>  item22   B      0.325          0.454                0.611
#>  item23   D      0.405          0.530                0.704
#>  item24   D      0.395          0.525                0.741
#>  item25   A      0.260          0.391                0.519
#>  item26   C      0.380          0.290                0.389
#>  item27   D      0.305          0.245                0.296
#>  item28   A      0.300          0.195                0.259
#>  item29   C      0.460         -0.396               -0.407
#>  item30   D      0.500         -0.292               -0.296
#>  distractor_efficiency
#>                      2
#>                      1
#>                      2
#>                      2
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      3
#>                      1
#>                      1
```
