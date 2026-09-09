# Item discrimination

Computes a discrimination index for each item using one of two classical
methods: the point-biserial correlation between item and total test
score, or the upper-lower 27 percent discrimination index proposed by
Kelley (1939).

## Usage

``` r
item_discrimination(
  responses,
  key,
  method = c("point_biserial", "discrimination_index"),
  group_pct = 0.27
)
```

## Arguments

- responses:

  A matrix or data frame of student responses, with students in rows and
  items in columns.

- key:

  A vector of correct answers with length equal to the number of items.

- method:

  One of `"point_biserial"` (default) or `"discrimination_index"`.

- group_pct:

  For `method = "discrimination_index"`, the proportion of students
  assigned to each extreme group. Default is 0.27 following Kelley
  (1939).

## Value

A named numeric vector of discrimination values, one per item.

## Details

The point-biserial method is the most widely used CTT discrimination
index. The discrimination index `D` compares the proportion of the
upper-scoring group (top 27 percent by total score) who answered the
item correctly to the proportion of the lower-scoring group (bottom 27
percent) who answered it correctly. Kelley (1939) demonstrated that the
27 percent cutoff maximizes the difference between extreme groups under
a normal distribution of ability.

Interpretive guidelines for `D` (Ebel & Frisbie, 1991):

- D \>= 0.40: very good item

- 0.30 \<= D \< 0.40: good item, possibly subject to improvement

- 0.20 \<= D \< 0.30: marginal item, needs improvement

- D \< 0.20: poor item, revise or discard

## References

Ebel, R. L., & Frisbie, D. A. (1991). *Essentials of educational
measurement* (5th ed.). Prentice Hall.

Kelley, T. L. (1939). The selection of upper and lower groups for the
validation of test items. *Journal of Educational Psychology*, 30(1),
17-24.

## Examples

``` r
set.seed(1)
responses <- matrix(
  sample(c("A", "B", "C", "D"), 200, replace = TRUE),
  nrow = 40, ncol = 5,
  dimnames = list(NULL, paste0("Q", 1:5))
)
key <- c("A", "B", "C", "A", "B")
item_discrimination(responses, key)
#>          Q1          Q2          Q3          Q4          Q5 
#> -0.24699867 -0.24699867 -0.23966555 -0.18093672 -0.02105466 
item_discrimination(responses, key, method = "discrimination_index")
#>  Q1  Q2  Q3  Q4  Q5 
#> 0.4 0.4 0.3 0.4 0.3 
```
