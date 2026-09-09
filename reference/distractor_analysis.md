# Distractor analysis

For each item, summarizes the selection frequency, proportion, and
point-biserial correlation with the total test score for every response
option (the key and all distractors). Distractor analysis is a core
classical test theory diagnostic for evaluating multiple-choice items:
the key should be the most-selected option and should have a positive
point-biserial correlation with total score, while each distractor
should be selected by at least some examinees and should have a negative
point-biserial correlation with total score (Haladyna, 2004).

## Usage

``` r
distractor_analysis(responses, key, options = NULL)
```

## Arguments

- responses:

  A matrix or data frame of student responses, with students in rows and
  items in columns.

- key:

  A vector of correct answers with length equal to the number of items.

- options:

  Optional character vector listing all possible response options (e.g.,
  `c("A", "B", "C", "D")`). If `NULL` (default), the set of options is
  inferred from the unique values present in `responses`.

## Value

A data frame in long format with one row per item-option combination,
containing:

- `item`: item identifier

- `option`: response option

- `is_key`: logical, `TRUE` if this option is the correct answer

- `frequency`: number of students selecting this option

- `proportion`: proportion of students selecting this option

- `point_biserial`: correlation between selecting this option and the
  total test score (using all items)

## References

Haladyna, T. M. (2004). *Developing and validating multiple-choice test
items* (3rd ed.). Lawrence Erlbaum Associates.

## Examples

``` r
set.seed(1)
responses <- matrix(
  sample(c("A", "B", "C", "D"), 200, replace = TRUE),
  nrow = 50, ncol = 4,
  dimnames = list(NULL, paste0("Q", 1:4))
)
key <- c("A", "B", "C", "A")
distractor_analysis(responses, key)
#>     item option is_key frequency proportion point_biserial
#> Q1    Q1      A   TRUE        16       0.32     0.50864921
#> Q11   Q1      B  FALSE        16       0.32    -0.22216862
#> Q12   Q1      C  FALSE        11       0.22     0.06803155
#> Q13   Q1      D  FALSE         7       0.14    -0.46635225
#> Q2    Q2      A  FALSE        11       0.22    -0.26115337
#> Q21   Q2      B   TRUE        15       0.30     0.37692181
#> Q22   Q2      C  FALSE        11       0.22     0.01316740
#> Q23   Q2      D  FALSE        13       0.26    -0.15958626
#> Q3    Q3      A  FALSE        11       0.22    -0.15142506
#> Q31   Q3      B  FALSE        12       0.24    -0.04895789
#> Q32   Q3      C   TRUE        14       0.28     0.54464602
#> Q33   Q3      D  FALSE        13       0.26    -0.36684115
#> Q4    Q4      A   TRUE        13       0.26     0.51399212
#> Q41   Q4      B  FALSE        11       0.22    -0.04169676
#> Q42   Q4      C  FALSE        15       0.30    -0.16862292
#> Q43   Q4      D  FALSE        11       0.22    -0.31601752
```
