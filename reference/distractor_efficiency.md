# Distractor efficiency

Computes Haladyna's distractor efficiency for each item: the number of
functioning distractors per item. A distractor is considered to be
functioning if it meets two criteria: (a) it is selected by at least a
threshold proportion of examinees (default 5 percent), and (b) it has a
negative point-biserial correlation with the total test score (Haladyna
& Downing, 1993). The key (correct answer) is excluded from the count.

## Usage

``` r
distractor_efficiency(responses, key, options = NULL, min_proportion = 0.05)
```

## Arguments

- responses:

  A matrix or data frame of student responses, with students in rows and
  items in columns.

- key:

  A vector of correct answers with length equal to the number of items.

- options:

  Optional character vector listing all possible response options. If
  `NULL` (default), the set of options is inferred from the unique
  values present in `responses`.

- min_proportion:

  Minimum proportion of examinees selecting a distractor for it to be
  considered functioning. Default is 0.05.

## Value

A named numeric vector of distractor efficiency values, one per item,
representing the count of functioning distractors.

## Details

Distractor efficiency provides a simple integer summary of item quality.
A four-option multiple-choice item with three functioning distractors
(distractor efficiency = 3) is performing optimally. Items with fewer
functioning distractors waste examinee time and reduce the item's
contribution to score variance, and they are candidates for revision.

## References

Haladyna, T. M., & Downing, S. M. (1993). How many options is enough for
a multiple-choice test item? *Educational and Psychological
Measurement*, 53(4), 999-1010.

## Examples

``` r
set.seed(1)
responses <- matrix(
  sample(c("A", "B", "C", "D"), 400, replace = TRUE),
  nrow = 100, ncol = 4,
  dimnames = list(NULL, paste0("Q", 1:4))
)
key <- c("A", "B", "C", "A")
distractor_efficiency(responses, key)
#> Q1 Q2 Q3 Q4 
#>  2  3  3  3 
```
