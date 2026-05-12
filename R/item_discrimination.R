#' Item discrimination
#'
#' Computes a discrimination index for each item using one of two
#' classical methods: the point-biserial correlation between item and
#' total test score, or the upper-lower 27 percent discrimination
#' index proposed by Kelley (1939).
#'
#' The point-biserial method is the most widely used CTT discrimination
#' index. The discrimination index `D` compares the proportion of the
#' upper-scoring group (top 27 percent by total score) who answered the
#' item correctly to the proportion of the lower-scoring group (bottom
#' 27 percent) who answered it correctly. Kelley (1939) demonstrated
#' that the 27 percent cutoff maximizes the difference between extreme
#' groups under a normal distribution of ability.
#'
#' Interpretive guidelines for `D` (Ebel & Frisbie, 1991):
#' \itemize{
#'   \item D >= 0.40: very good item
#'   \item 0.30 <= D < 0.40: good item, possibly subject to improvement
#'   \item 0.20 <= D < 0.30: marginal item, needs improvement
#'   \item D < 0.20: poor item, revise or discard
#' }
#'
#' @param responses A matrix or data frame of student responses, with
#'   students in rows and items in columns.
#' @param key A vector of correct answers with length equal to the number
#'   of items.
#' @param method One of `"point_biserial"` (default) or
#'   `"discrimination_index"`.
#' @param group_pct For `method = "discrimination_index"`, the proportion
#'   of students assigned to each extreme group. Default is 0.27 following
#'   Kelley (1939).
#'
#' @return A named numeric vector of discrimination values, one per item.
#'
#' @references
#' Ebel, R. L., & Frisbie, D. A. (1991). *Essentials of educational
#' measurement* (5th ed.). Prentice Hall.
#'
#' Kelley, T. L. (1939). The selection of upper and lower groups for the
#' validation of test items. *Journal of Educational Psychology*, 30(1),
#' 17-24.
#'
#' @examples
#' set.seed(1)
#' responses <- matrix(
#'   sample(c("A", "B", "C", "D"), 200, replace = TRUE),
#'   nrow = 40, ncol = 5,
#'   dimnames = list(NULL, paste0("Q", 1:5))
#' )
#' key <- c("A", "B", "C", "A", "B")
#' item_discrimination(responses, key)
#' item_discrimination(responses, key, method = "discrimination_index")
#'
#' @export
item_discrimination <- function(responses, key,
                                method = c("point_biserial",
                                           "discrimination_index"),
                                group_pct = 0.27) {
  method <- match.arg(method)
  if (method == "point_biserial") {
    return(point_biserial(responses, key, corrected = TRUE))
  }
  # Discrimination index (upper-lower group method)
  if (group_pct <= 0 || group_pct >= 0.5) {
    stop("`group_pct` must be between 0 and 0.5 (exclusive).", call. = FALSE)
  }
  inputs <- validate_inputs(responses, key)
  scored <- score_responses(inputs$responses, inputs$key)
  total <- rowSums(scored)
  n_students <- nrow(scored)
  n_group <- max(1L, floor(n_students * group_pct))
  ranks <- order(total)
  lower_idx <- ranks[seq_len(n_group)]
  upper_idx <- ranks[(n_students - n_group + 1):n_students]
  upper_p <- colMeans(scored[upper_idx, , drop = FALSE])
  lower_p <- colMeans(scored[lower_idx, , drop = FALSE])
  D <- upper_p - lower_p
  names(D) <- colnames(scored)
  D
}
