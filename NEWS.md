# mcqAnalysis 0.1.0

* Initial CRAN release.
* Implements classical test theory item analysis for multiple-choice
  tests, including item difficulty (p-value), item discrimination
  (point-biserial and upper-lower 27 percent index), distractor
  analysis (per-option frequency, proportion, and point-biserial),
  and Haladyna's distractor efficiency.
* Wrapper function `mcq_analysis()` returns an S3 object with
  `print`, `plot` (difficulty-discrimination scatter), and
  `apa_table` methods.
* Includes a simulated example dataset `mcq_example` of 200 students
  on a 30-item four-option test.
* Verified by 78 unit tests using **testthat**.
