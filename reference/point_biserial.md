# Point-biserial correlation

Computes the point-biserial correlation between each item and the total
test score (excluding the item itself, i.e., corrected for item
overlap). This is the standard classical test theory discrimination
index based on the correlation between item performance and overall test
performance.

## Usage

``` r
point_biserial(responses, key, corrected = TRUE)
```

## Arguments

- responses:

  A matrix or data frame of student responses, with students in rows and
  items in columns.

- key:

  A vector of correct answers with length equal to the number of items.

- corrected:

  Logical. If `TRUE` (default), the total score is computed excluding
  the item being correlated, yielding the corrected item-total
  correlation. If `FALSE`, the total score includes the item.

## Value

A named numeric vector of point-biserial correlations, one per item.

## Details

Items with point-biserial correlations of 0.30 or above are generally
considered to discriminate well between high- and low-ability students.
Values between 0.20 and 0.29 are marginal; values below 0.20 indicate
poor discrimination, and negative values suggest a problem with the item
(Ebel & Frisbie, 1991).

## References

Ebel, R. L., & Frisbie, D. A. (1991). *Essentials of educational
measurement* (5th ed.). Prentice Hall.

## Examples

``` r
set.seed(1)
responses <- matrix(
  sample(c("A", "B", "C", "D"), 100, replace = TRUE),
  nrow = 20, ncol = 5,
  dimnames = list(NULL, paste0("Q", 1:5))
)
key <- c("A", "B", "C", "A", "B")
point_biserial(responses, key)
#>          Q1          Q2          Q3          Q4          Q5 
#> -0.21352448 -0.21352448 -0.22091804  0.02252458  0.29149154 
```
