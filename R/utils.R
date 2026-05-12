#' Validate response matrix and answer key
#'
#' Internal helper. Checks that the response matrix and key are compatible,
#' coerces to a common storage mode, and returns a list with cleaned objects.
#'
#' @param responses A matrix or data frame of student responses. Rows are
#'   students, columns are items.
#' @param key A vector of correct answers with length equal to the number
#'   of items (columns of responses).
#' @return A list with elements `responses` (character matrix) and `key`
#'   (character vector).
#' @noRd
validate_inputs <- function(responses, key) {
  if (is.data.frame(responses)) {
    responses <- as.matrix(responses)
  }
  if (!is.matrix(responses)) {
    stop("`responses` must be a matrix or data frame.", call. = FALSE)
  }
  if (nrow(responses) < 2L) {
    stop("`responses` must have at least 2 students (rows).", call. = FALSE)
  }
  if (ncol(responses) < 1L) {
    stop("`responses` must have at least 1 item (column).", call. = FALSE)
  }
  if (length(key) != ncol(responses)) {
    stop(
      "`key` length (", length(key),
      ") must equal the number of items (", ncol(responses), ").",
      call. = FALSE
    )
  }
  # Coerce both to character for uniform comparison
  responses <- apply(responses, 2, as.character)
  key <- as.character(key)
  # Preserve item names
  if (is.null(colnames(responses))) {
    colnames(responses) <- paste0("item", seq_len(ncol(responses)))
  }
  names(key) <- colnames(responses)
  list(responses = responses, key = key)
}

#' Score response matrix against key
#'
#' Internal helper. Returns a 0/1 numeric matrix where 1 indicates a correct
#' response. Missing responses (NA) are treated as incorrect (0) unless
#' `na.rm = TRUE`, in which case they remain NA and downstream functions
#' must handle them.
#'
#' @param responses Character matrix of student responses.
#' @param key Character vector of correct answers.
#' @param na.rm Logical. If TRUE, NA responses are preserved as NA.
#' @return A 0/1 numeric matrix (or with NA if na.rm = TRUE).
#' @noRd
score_responses <- function(responses, key, na.rm = FALSE) {
  scored <- matrix(
    NA_integer_,
    nrow = nrow(responses),
    ncol = ncol(responses),
    dimnames = list(rownames(responses), colnames(responses))
  )
  for (j in seq_len(ncol(responses))) {
    scored[, j] <- as.integer(responses[, j] == key[j])
  }
  if (!na.rm) {
    scored[is.na(scored)] <- 0L
  }
  scored
}
