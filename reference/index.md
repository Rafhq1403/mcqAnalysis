# Package index

## Item-level statistics

Compute item difficulty and discrimination from a response matrix
(examinees in rows, items in columns) and an answer key.

- [`item_difficulty()`](https://rafhq1403.github.io/mcqAnalysis/reference/item_difficulty.md)
  : Item difficulty (p-value)
- [`item_discrimination()`](https://rafhq1403.github.io/mcqAnalysis/reference/item_discrimination.md)
  : Item discrimination
- [`point_biserial()`](https://rafhq1403.github.io/mcqAnalysis/reference/point_biserial.md)
  : Point-biserial correlation

## Distractor analysis

Per-option statistics and Haladyna’s distractor efficiency.

- [`distractor_analysis()`](https://rafhq1403.github.io/mcqAnalysis/reference/distractor_analysis.md)
  : Distractor analysis
- [`distractor_efficiency()`](https://rafhq1403.github.io/mcqAnalysis/reference/distractor_efficiency.md)
  : Distractor efficiency

## Full analysis and output

One-call wrapper returning an mcq_analysis S3 object with dedicated
print, plot, and APA-table methods.

- [`mcq_analysis()`](https://rafhq1403.github.io/mcqAnalysis/reference/mcq_analysis.md)
  : Comprehensive multiple-choice item analysis
- [`plot(`*`<mcq_analysis>`*`)`](https://rafhq1403.github.io/mcqAnalysis/reference/plot.mcq_analysis.md)
  : Plot a difficulty-discrimination scatter for an mcq_analysis object
- [`apa_table()`](https://rafhq1403.github.io/mcqAnalysis/reference/apa_table.md)
  : Generic APA-style table formatter
- [`apa_table(`*`<mcq_analysis>`*`)`](https://rafhq1403.github.io/mcqAnalysis/reference/apa_table.mcq_analysis.md)
  : APA-style table for an mcq_analysis object

## Bundled dataset

- [`mcq_example`](https://rafhq1403.github.io/mcqAnalysis/reference/mcq_example.md)
  : Simulated multiple-choice test data
