#' Plot a difficulty-discrimination scatter for an mcq_analysis object
#'
#' Produces the classical item quality map: a scatterplot of item
#' difficulty (x-axis) against item discrimination (y-axis), with
#' reference lines marking conventional adequacy cutoffs. Items in the
#' upper-middle region (medium difficulty, high discrimination) are
#' performing well; items in the lower regions are candidates for
#' revision.
#'
#' By default, only flagged items (those falling outside the conventional
#' adequacy region) are labeled, to keep the plot legible when many items
#' cluster in the acceptable region. Use `label = "all"` to label every
#' item, or `label = "none"` to suppress labels entirely.
#'
#' Reference lines are drawn at conventional cutoffs from Ebel and
#' Frisbie (1991): discrimination >= 0.30 (acceptable) and difficulty
#' between 0.30 and 0.90 (informative range).
#'
#' @param x An object of class `mcq_analysis`.
#' @param y Ignored. Present for S3 compatibility.
#' @param discrimination_metric Which discrimination index to plot on the
#'   y-axis. One of `"point_biserial"` (default) or
#'   `"discrimination_index"`.
#' @param label One of `"flagged"` (default, label only problematic items),
#'   `"all"` (label every item), or `"none"` (no labels). Also accepts
#'   `TRUE` (= "all") or `FALSE` (= "none") for backwards compatibility.
#' @param flag_threshold_difficulty Numeric vector of length 2 giving the
#'   informative difficulty range. Default `c(0.30, 0.90)`.
#' @param flag_threshold_discrimination Numeric. Discrimination cutoff
#'   below which an item is considered weak. Default 0.30.
#' @param point_cex Numeric. Point size. Default 1.4.
#' @param label_cex Numeric. Label text size. Default 0.75.
#' @param ... Additional graphical parameters passed to `plot()`.
#'
#' @return The input `mcq_analysis` object, invisibly.
#'
#' @references
#' Ebel, R. L., & Frisbie, D. A. (1991). *Essentials of educational
#' measurement* (5th ed.). Prentice Hall.
#'
#' @examples
#' data(mcq_example)
#' result <- mcq_analysis(mcq_example$responses, mcq_example$key)
#' plot(result)
#' plot(result, label = "all")
#' plot(result, label = "none")
#'
#' @export
plot.mcq_analysis <- function(x, y = NULL,
                              discrimination_metric = c("point_biserial",
                                                        "discrimination_index"),
                              label = c("flagged", "all", "none"),
                              flag_threshold_difficulty = c(0.30, 0.90),
                              flag_threshold_discrimination = 0.30,
                              point_cex = 1.4,
                              label_cex = 0.75,
                              ...) {
  discrimination_metric <- match.arg(discrimination_metric)
  # Backwards-compatible logical input for label
  if (isTRUE(label)) label <- "all"
  if (identical(label, FALSE)) label <- "none"
  label <- match.arg(label)

  difficulty <- x$items$difficulty
  if (discrimination_metric == "point_biserial") {
    discr <- x$items$point_biserial
    y_label <- "Point-biserial discrimination"
  } else {
    discr <- x$items$discrimination_index
    y_label <- "Discrimination index (D)"
  }
  item_names <- x$items$item

  flagged <- difficulty < flag_threshold_difficulty[1] |
             difficulty > flag_threshold_difficulty[2] |
             discr < flag_threshold_discrimination

  point_col <- ifelse(flagged,
                      grDevices::adjustcolor("firebrick", alpha.f = 0.9),
                      grDevices::adjustcolor("black", alpha.f = 0.55))

  # Save & restore graphical parameters
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  graphics::par(mar = c(4.5, 4.5, 3.5, 1.2), las = 1, cex.axis = 0.9)

  y_low <- min(-0.1, min(discr, na.rm = TRUE) - 0.05)
  y_high <- max(1, max(discr, na.rm = TRUE) + 0.05)

  graphics::plot(
    difficulty, discr,
    xlim = c(0, 1),
    ylim = c(y_low, y_high),
    xlab = "Item difficulty (proportion correct)",
    ylab = y_label,
    main = "Difficulty-Discrimination Scatter",
    pch = 19,
    col = point_col,
    cex = point_cex,
    xaxs = "i",
    yaxs = "i",
    ...
  )

  # Soft grid
  graphics::grid(nx = NULL, ny = NULL, lty = 3, col = "gray85")

  # Reference lines (drawn after grid so they remain visible)
  graphics::abline(h = flag_threshold_discrimination,
                   lty = 2, col = "gray45", lwd = 1.1)
  graphics::abline(v = flag_threshold_difficulty,
                   lty = 2, col = "gray45", lwd = 1.1)
  graphics::abline(h = 0, lty = 1, col = "gray70", lwd = 0.8)

  # Re-draw points on top of grid for clarity
  graphics::points(difficulty, discr, pch = 19, cex = point_cex,
                   col = point_col)

  # Subtitle annotating the cutoffs
  graphics::mtext(
    sprintf(
      "Reference cutoffs: difficulty in [%.2f, %.2f], discrimination >= %.2f",
      flag_threshold_difficulty[1],
      flag_threshold_difficulty[2],
      flag_threshold_discrimination
    ),
    side = 3, line = 0.3, cex = 0.85, col = "gray35"
  )

  # Labels
  if (label != "none") {
    to_label <- if (label == "all") seq_along(item_names) else which(flagged)
    if (length(to_label) > 0) {
      # Offset labels slightly to avoid covering the point
      graphics::text(
        difficulty[to_label] + 0.012,
        discr[to_label],
        labels = item_names[to_label],
        pos = 4,
        cex = label_cex,
        col = ifelse(flagged[to_label], "firebrick", "gray25"),
        offset = 0.15
      )
    }
  }

  graphics::legend(
    "bottomleft",
    legend = c("Acceptable item", "Flagged for revision"),
    pch = 19,
    col = c(grDevices::adjustcolor("black", alpha.f = 0.55),
            grDevices::adjustcolor("firebrick", alpha.f = 0.9)),
    pt.cex = point_cex,
    bty = "n",
    cex = 0.85,
    inset = c(0.02, 0.02)
  )

  invisible(x)
}
