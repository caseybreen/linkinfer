# Internal utility functions for linkinfer

#' Detect whether a variable is categorical or continuous
#' @noRd
.detect_var_type <- function(x) {
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    return("categorical")
  }
  if (is.numeric(x) && !is.integer(x) && length(unique(stats::na.omit(x))) <= 5) {
    return("categorical")
  }
  "continuous"
}

#' Weighted mean with NA handling
#' @noRd
.weighted_mean <- function(x, w = NULL) {
  if (is.null(w)) return(mean(x, na.rm = TRUE))
  idx <- !is.na(x) & !is.na(w)
  stats::weighted.mean(x[idx], w[idx])
}

#' Weighted variance
#' @noRd
.weighted_var <- function(x, w = NULL) {
  if (is.null(w)) return(stats::var(x, na.rm = TRUE))
  idx <- !is.na(x) & !is.na(w)
  x <- x[idx]
  w <- w[idx]
  mu <- stats::weighted.mean(x, w)
  sum(w * (x - mu)^2) / sum(w)
}

#' Standardized difference between two groups
#' @noRd
.standardized_diff <- function(mean1, mean2, var1, var2) {
  pooled_sd <- sqrt((var1 + var2) / 2)
  if (pooled_sd == 0) return(0)
  (mean2 - mean1) / pooled_sd
}

#' Trim/winsorize weights
#' @noRd
.trim_weights <- function(w, trim = c(0.01, 0.99)) {
  q <- stats::quantile(w, probs = trim, na.rm = TRUE)
  pmin(pmax(w, q[1]), q[2])
}

#' Effective sample size: (sum(w))^2 / sum(w^2)
#' @noRd
.effective_n <- function(w) {
  sum(w)^2 / sum(w^2)
}

#' Design effect: n * sum(w^2) / (sum(w))^2
#' @noRd
.design_effect <- function(w) {
  n <- length(w)
  n * sum(w^2) / sum(w)^2
}

#' Weighted proportion table for a categorical variable
#' @noRd
.weighted_prop_table <- function(x, w = NULL) {
  if (is.null(w)) {
    tbl <- table(x, useNA = "no")
    return(prop.table(tbl))
  }
  lvls <- sort(unique(x[!is.na(x)]))
  props <- vapply(lvls, function(l) sum(w[x == l & !is.na(x)]), numeric(1))
  props <- props / sum(props)
  names(props) <- lvls
  props
}
