# Simulated multiple-choice test data

A simulated dataset for demonstrating the mcqAnalysis package. The test
contains 30 four-option multiple-choice items administered to 200
students. The data are generated under a two-parameter logistic
framework with a deliberately mixed mix of item quality:

- Items 1-8 are easy items with strong discrimination.

- Items 9-24 are medium-difficulty items, most discriminating well.

- Items 25-28 are harder items with progressively weaker discrimination.

- Items 29-30 are deliberately badly-written items with negative
  discrimination (high-ability students get them wrong more often).

- Item 30 additionally has a "trap" distractor disproportionately chosen
  by high-ability students, useful for demonstrating distractor
  analysis.

## Usage

``` r
mcq_example
```

## Format

A list with two components:

- responses:

  A 200 x 30 character matrix of student responses (values in
  `{"A", "B", "C", "D"}`).

- key:

  A named character vector of length 30 giving the correct answer for
  each item.

## Examples

``` r
data(mcq_example)
str(mcq_example, max.level = 1)
#> List of 2
#>  $ responses: chr [1:200, 1:30] "D" "D" "D" "D" ...
#>   ..- attr(*, "dimnames")=List of 2
#>  $ key      : Named chr [1:30] "D" "D" "D" "A" ...
#>   ..- attr(*, "names")= chr [1:30] "item01" "item02" "item03" "item04" ...
mcq_example$key
#> item01 item02 item03 item04 item05 item06 item07 item08 item09 item10 item11 
#>    "D"    "D"    "D"    "A"    "D"    "B"    "D"    "A"    "D"    "C"    "B" 
#> item12 item13 item14 item15 item16 item17 item18 item19 item20 item21 item22 
#>    "A"    "C"    "B"    "D"    "A"    "A"    "D"    "A"    "D"    "B"    "B" 
#> item23 item24 item25 item26 item27 item28 item29 item30 
#>    "D"    "D"    "A"    "C"    "D"    "A"    "C"    "D" 
head(mcq_example$responses)
#>            item01 item02 item03 item04 item05 item06 item07 item08 item09
#> student001 "D"    "D"    "D"    "A"    "D"    "B"    "D"    "A"    "D"   
#> student002 "D"    "D"    "D"    "A"    "A"    "B"    "A"    "B"    "D"   
#> student003 "D"    "D"    "C"    "A"    "D"    "B"    "D"    "A"    "B"   
#> student004 "D"    "D"    "D"    "A"    "D"    "B"    "D"    "A"    "D"   
#> student005 "D"    "D"    "D"    "A"    "D"    "B"    "D"    "A"    "B"   
#> student006 "D"    "D"    "D"    "A"    "D"    "B"    "A"    "A"    "B"   
#>            item10 item11 item12 item13 item14 item15 item16 item17 item18
#> student001 "C"    "D"    "B"    "C"    "B"    "D"    "A"    "A"    "D"   
#> student002 "C"    "C"    "D"    "B"    "C"    "C"    "D"    "D"    "B"   
#> student003 "C"    "B"    "B"    "C"    "C"    "D"    "C"    "A"    "A"   
#> student004 "C"    "A"    "A"    "D"    "B"    "D"    "A"    "A"    "A"   
#> student005 "C"    "B"    "A"    "A"    "D"    "D"    "A"    "B"    "D"   
#> student006 "B"    "B"    "A"    "A"    "C"    "C"    "D"    "A"    "D"   
#>            item19 item20 item21 item22 item23 item24 item25 item26 item27
#> student001 "A"    "D"    "C"    "B"    "D"    "D"    "A"    "A"    "B"   
#> student002 "D"    "A"    "C"    "D"    "B"    "A"    "B"    "B"    "D"   
#> student003 "B"    "D"    "B"    "C"    "D"    "D"    "A"    "D"    "C"   
#> student004 "A"    "B"    "D"    "B"    "A"    "B"    "C"    "C"    "D"   
#> student005 "A"    "D"    "A"    "C"    "D"    "D"    "D"    "B"    "D"   
#> student006 "A"    "A"    "C"    "B"    "D"    "A"    "C"    "C"    "D"   
#>            item28 item29 item30
#> student001 "A"    "B"    "A"   
#> student002 "D"    "D"    "A"   
#> student003 "C"    "C"    "D"   
#> student004 "C"    "A"    "D"   
#> student005 "D"    "D"    "A"   
#> student006 "A"    "D"    "C"   
```
