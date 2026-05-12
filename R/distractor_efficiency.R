#' Distractor efficiency
#'
#' Computes Haladyna's distractor efficiency for each item: the number of
#' functioning distractors per item. A distractor is considered to be
#' functioning if it meets two criteria: (a) it is selected by at least a
#' threshold proportion of examinees (default 5 percent), and (b) it has
#' a negative point-biserial correlation with the total test score
#' (Haladyna & Downing, 1993). The key (correct answer) is excluded from
#' the count.
#'
#' Distractor efficiency provides a simple integer summary of item quality.
#' A four-option multiple-choice item with three functioning distractors
#' (distractor efficiency = 3) is performing optimally. Items with fewer
#' functioning distractors waste examinee time and reduce the item's
#' contribution to score variance, and they are candidates for revision.
#'
#' @param responses A matrix or data frame of student responses, with
#'   students in rows and items in columns.
#' @param key A vector of correct answers with length equal to the number
#'   of items.
#' @param options Optional character vector listing all possible response
#'   options. If `NULL` (default), the set of options is inferred from
#'   the unique values present in `responses`.
#' @param min_proportion Minimum proportion of examinees selecting a
#'   distractor for it to be considered functioning. Default is 0.05.
#'
#' @return A named numeric vector of distractor efficiency values, one
#'   per item, representing the count of functioning distractors.
#'
#' @references
#' Haladyna, T. M., & Downing, S. M. (1993). How many options is enough
#' for a multiple-choice test item? *Educational and Psychological
#' Measurement*, 53(4), 999-1010.
#'
#' @examples
#' set.seed(1)
#' responses <- matrix(
#'   sample(c("A", "B", "C", "D"), 400, replace = TRUE),
#'   nrow = 100, ncol = 4,
#'   dimnames = list(NULL, paste0("Q", 1:4))
#' )
#' key <- c("A", "B", "C", "A")
#' distractor_efficiency(responses, key)
#'
#' @export
distractor_efficiency <- function(responses, key, options = NULL,
                                  min_proportion = 0.05) {
  da <- distractor_analysis(responses, key, options = options)
  items <- unique(da$item)
  de <- numeric(length(items))
  names(de) <- items
  for (i in seq_along(items)) {
    item_rows <- da[da$item == items[i] & !da$is_key, , drop = FALSE]
    functioning <- !is.na(item_rows$point_biserial) &
                   item_rows$proportion >= min_proportion &
                   item_rows$point_biserial < 0
    de[i] <- sum(functioning)
  }
  de
}
