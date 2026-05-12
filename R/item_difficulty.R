#' Item difficulty (p-value)
#'
#' Computes the proportion of students who answered each item correctly,
#' commonly called the item p-value in classical test theory.
#'
#' Item difficulty is interpreted as the easiness of an item: values near 1
#' indicate an easy item (most students got it correct), while values near 0
#' indicate a hard item. Conventional interpretive guidelines suggest that
#' well-functioning items typically have p-values between 0.30 and 0.90,
#' with optimal difficulty around 0.50 for maximum discrimination
#' (Crocker & Algina, 1986).
#'
#' @param responses A matrix or data frame of student responses, with
#'   students in rows and items in columns. Entries may be character or
#'   numeric (e.g., "A", "B", "C", "D" or 1, 2, 3, 4).
#' @param key A vector of correct answers with length equal to the number
#'   of items.
#' @param na.rm Logical. If `TRUE`, missing responses are excluded from
#'   the calculation on a per-item basis. If `FALSE` (default), missing
#'   responses are scored as incorrect.
#'
#' @return A named numeric vector of item p-values, one per item.
#'
#' @references
#' Crocker, L., & Algina, J. (1986). *Introduction to classical and modern
#' test theory*. Holt, Rinehart and Winston.
#'
#' @examples
#' responses <- matrix(
#'   c("A", "A", "B", "C",
#'     "A", "B", "B", "C",
#'     "A", "A", "C", "D",
#'     "B", "A", "B", "C",
#'     "A", "A", "B", "A"),
#'   nrow = 5, byrow = TRUE,
#'   dimnames = list(NULL, c("Q1", "Q2", "Q3", "Q4"))
#' )
#' key <- c("A", "A", "B", "C")
#' item_difficulty(responses, key)
#'
#' @export
item_difficulty <- function(responses, key, na.rm = FALSE) {
  inputs <- validate_inputs(responses, key)
  scored <- score_responses(inputs$responses, inputs$key, na.rm = na.rm)
  p <- colMeans(scored, na.rm = na.rm)
  names(p) <- colnames(inputs$responses)
  p
}
