#' Generic APA-style table formatter
#'
#' S3 generic for converting analysis objects into publication-ready
#' APA-style tables. The default behavior is dispatched to class-specific
#' methods (e.g., `apa_table.mcq_analysis`). Output formats include data
#' frame, markdown, HTML, and LaTeX for direct inclusion in manuscripts.
#'
#' @param x An object of an appropriate class (e.g., `mcq_analysis`).
#' @param format One of `"data.frame"`, `"markdown"`, `"html"`, or
#'   `"latex"`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A formatted table object whose type depends on `format`.
#'
#' @export
apa_table <- function(x, format = c("data.frame", "markdown", "html", "latex"),
                      ...) {
  UseMethod("apa_table")
}

#' APA-style table for an mcq_analysis object
#'
#' Formats item-level results from an `mcq_analysis` object as a
#' publication-ready APA-style table, with optional Interpretation
#' columns based on conventional CTT cutoffs (Ebel & Frisbie, 1991).
#'
#' @param x An object of class `mcq_analysis`.
#' @param format Output format. One of `"data.frame"` (default),
#'   `"markdown"`, `"html"`, or `"latex"`.
#' @param digits Number of decimal places to display. Default 2.
#' @param include_interpretation Logical. If `TRUE` (default), includes
#'   columns interpreting difficulty and discrimination using
#'   conventional cutoffs.
#' @param ... Additional arguments passed to `knitr::kable()` for non
#'   data-frame formats.
#'
#' @return A data frame (when `format = "data.frame"`) or a character
#'   string formatted in the requested style.
#'
#' @references
#' Ebel, R. L., & Frisbie, D. A. (1991). *Essentials of educational
#' measurement* (5th ed.). Prentice Hall.
#'
#' @examples
#' data(mcq_example)
#' result <- mcq_analysis(mcq_example$responses, mcq_example$key)
#' apa_table(result, format = "data.frame")
#'
#' @export
apa_table.mcq_analysis <- function(x,
                                   format = c("data.frame", "markdown",
                                              "html", "latex"),
                                   digits = 2,
                                   include_interpretation = TRUE,
                                   ...) {
  format <- match.arg(format)
  items <- x$items
  out <- data.frame(
    Item = items$item,
    Key = items$key,
    Difficulty = round(items$difficulty, digits),
    `Point-biserial` = round(items$point_biserial, digits),
    `Discrimination D` = round(items$discrimination_index, digits),
    `Distractor Efficiency` = items$distractor_efficiency,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (include_interpretation) {
    out$`Difficulty Level` <- difficulty_label(items$difficulty)
    out$Discrimination <- discrimination_label(items$point_biserial)
  }
  if (format == "data.frame") {
    return(out)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for non-data.frame formats. ",
         "Install it with install.packages('knitr').", call. = FALSE)
  }
  caption <- sprintf(
    "Item analysis (N = %d students, %d items).",
    x$n_students, x$n_items
  )
  knitr::kable(out, format = format, caption = caption, ...)
}

#' Convert difficulty values to verbal interpretation
#' @noRd
difficulty_label <- function(p) {
  ifelse(p < 0.30, "Hard",
  ifelse(p > 0.90, "Easy", "Moderate"))
}

#' Convert point-biserial values to verbal interpretation
#' @noRd
discrimination_label <- function(rpb) {
  ifelse(is.na(rpb), NA_character_,
  ifelse(rpb >= 0.40, "Excellent",
  ifelse(rpb >= 0.30, "Good",
  ifelse(rpb >= 0.20, "Marginal", "Poor"))))
}
