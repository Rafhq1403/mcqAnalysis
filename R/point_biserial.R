#' Point-biserial correlation
#'
#' Computes the point-biserial correlation between each item and the total
#' test score (excluding the item itself, i.e., corrected for item overlap).
#' This is the standard classical test theory discrimination index based on
#' the correlation between item performance and overall test performance.
#'
#' Items with point-biserial correlations of 0.30 or above are generally
#' considered to discriminate well between high- and low-ability students.
#' Values between 0.20 and 0.29 are marginal; values below 0.20 indicate
#' poor discrimination, and negative values suggest a problem with the item
#' (Ebel & Frisbie, 1991).
#'
#' @param responses A matrix or data frame of student responses, with
#'   students in rows and items in columns.
#' @param key A vector of correct answers with length equal to the number
#'   of items.
#' @param corrected Logical. If `TRUE` (default), the total score is
#'   computed excluding the item being correlated, yielding the
#'   corrected item-total correlation. If `FALSE`, the total score
#'   includes the item.
#'
#' @return A named numeric vector of point-biserial correlations, one
#'   per item.
#'
#' @references
#' Ebel, R. L., & Frisbie, D. A. (1991). *Essentials of educational
#' measurement* (5th ed.). Prentice Hall.
#'
#' @examples
#' set.seed(1)
#' responses <- matrix(
#'   sample(c("A", "B", "C", "D"), 100, replace = TRUE),
#'   nrow = 20, ncol = 5,
#'   dimnames = list(NULL, paste0("Q", 1:5))
#' )
#' key <- c("A", "B", "C", "A", "B")
#' point_biserial(responses, key)
#'
#' @export
point_biserial <- function(responses, key, corrected = TRUE) {
  inputs <- validate_inputs(responses, key)
  scored <- score_responses(inputs$responses, inputs$key)
  total <- rowSums(scored)
  n_items <- ncol(scored)
  rpb <- numeric(n_items)
  names(rpb) <- colnames(scored)
  for (j in seq_len(n_items)) {
    item_scores <- scored[, j]
    if (corrected) {
      criterion <- total - item_scores
    } else {
      criterion <- total
    }
    if (stats::sd(item_scores) == 0 || stats::sd(criterion) == 0) {
      rpb[j] <- NA_real_
    } else {
      rpb[j] <- stats::cor(item_scores, criterion)
    }
  }
  rpb
}
