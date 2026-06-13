# mcqAnalysis

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/mcqAnalysis)](https://CRAN.R-project.org/package=mcqAnalysis)
[![R-CMD-check](https://github.com/Rafhq1403/mcqAnalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Rafhq1403/mcqAnalysis/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/Rafhq1403/mcqAnalysis/blob/master/LICENSE.md)
[![DOI](https://img.shields.io/badge/DOI-10.1016%2Fj.softx.2026.102797-blue.svg)](https://doi.org/10.1016/j.softx.2026.102797)
<!-- badges: end -->

A unified R toolkit for **classical test theory (CTT) item analysis**
of multiple-choice tests. Compute item difficulty, item discrimination
(point-biserial and upper-lower 27 percent), per-distractor analysis,
and Haladyna's distractor efficiency in a single call. A wrapper
function returns a tidy `mcq_analysis` object with `print`, `plot`
(difficulty-discrimination scatter), and APA-style table methods for
direct manuscript inclusion.

## Installation

Install the released version from CRAN:

```r
install.packages("mcqAnalysis")
```

Or the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("Rafhq1403/mcqAnalysis")
```

## Quick example

```r
library(mcqAnalysis)
data(mcq_example)

# Complete item analysis in one call
result <- mcq_analysis(mcq_example$responses, mcq_example$key)
result
#> Multiple-Choice Item Analysis
#> ------------------------------
#> Students: 200
#> Items:    30
#> Mean total score: 15.765 (SD = 6.342)
#>
#> Item-level statistics:
#>   item key difficulty point_biserial discrimination_index distractor_efficiency
#> item01   D      0.850          0.472                0.426                     2
#> ...

# Item quality map
plot(result)

# Publication-ready APA table
apa_table(result, format = "markdown")
```

## What's included

| Function                    | Purpose                                            |
|-----------------------------|----------------------------------------------------|
| `item_difficulty()`         | p-value per item                                   |
| `item_discrimination()`     | Point-biserial or upper-lower discrimination index |
| `point_biserial()`          | Corrected item-total point-biserial correlation    |
| `distractor_analysis()`     | Per-option frequency, proportion, point-biserial   |
| `distractor_efficiency()`   | Haladyna's count of functioning distractors        |
| `mcq_analysis()`            | One-call wrapper returning an S3 object            |
| `plot.mcq_analysis()`       | Difficulty-discrimination scatter                  |
| `apa_table.mcq_analysis()`  | APA-style table (data.frame, markdown, HTML, LaTeX) |
| `mcq_example` (dataset)     | 200 students × 30 four-option items                |

## Methods reference

The package implements widely-used classical test theory indices.
Conventional cutoffs follow Ebel & Frisbie (1991) for discrimination
and Haladyna & Downing (1993) for distractor efficiency. See the
[getting-started vignette](https://CRAN.R-project.org/package=mcqAnalysis)
for a complete worked example and references.

## Related packages

`mcqAnalysis` is designed to be a CTT-focused companion to its sister
package [`contentValidity`](https://CRAN.R-project.org/package=contentValidity),
which computes content-validity indices (I-CVI, S-CVI, modified kappa,
Aiken's V, Lawshe's CVR). Together they cover pre-administration
content validation (via expert review) and post-administration
empirical item analysis (via examinee response data) in a consistent
API. For inter-rater agreement statistics beyond content validity, see
[`irrCAC`](https://CRAN.R-project.org/package=irrCAC); for broader
psychometric utilities, see
[`psych`](https://CRAN.R-project.org/package=psych).

## How to cite

If you use `mcqAnalysis` in your work, please cite the SoftwareX paper:

> Alqahtani, R. (2026). McqAnalysis: Classical test theory item analysis for multiple-choice tests in R. *SoftwareX*, *35*, 102797.
> <https://doi.org/10.1016/j.softx.2026.102797>

BibTeX:

```bibtex
@article{alqahtani2026mcqanalysis,
  title   = {McqAnalysis: Classical test theory item analysis for multiple-choice tests in {R}},
  author  = {Alqahtani, Rashed},
  journal = {SoftwareX},
  year    = {2026},
  volume  = {35},
  pages   = {102797},
  doi     = {10.1016/j.softx.2026.102797}
}
```

You can also retrieve the citation directly in R:

```r
citation("mcqAnalysis")
```

## License

[MIT](https://github.com/Rafhq1403/mcqAnalysis/blob/master/LICENSE.md)
© Rashed Alqahtani
