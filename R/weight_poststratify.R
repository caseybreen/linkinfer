#' Post-stratification weights for linked samples
#'
#' Computes post-stratification weights to adjust the linked sample so that
#' its distribution across cells matches the full population. Implements
#' Equation 1 of Breen and Joo (2026): \eqn{w_i = N_z / n_z}.
#'
#' @param population A data.frame of the full population.
#' @param linked A data.frame of the linked subsample.
#' @param strata_vars A character vector of categorical variable names defining
#'   the cross-classification cells. Must be present in both data.frames.
#' @param weights Optional. A character string naming a column of sampling
#'   weights in `population`. If provided, population cell sizes are computed
#'   as weighted sums.
#'
#' @return A list of class `"linkinfer_poststrat"` with elements:
#'   \describe{
#'     \item{weights}{Numeric vector of post-stratification adjustment weights
#'       for linked records.}
#'     \item{strata_table}{A data.frame showing cell definitions, population
#'       counts/proportions, linked counts/proportions, and adjustment factors.}
#'     \item{empty_cells}{Character vector of cells present in the population
#'       but absent from the linked sample (if any).}
#'   }
#'
#' @examples
#' data(census_population)
#' data(linked_sample)
#' weight_poststratify(
#'   population = census_population,
#'   linked = linked_sample,
#'   strata_vars = c("sex", "race")
#' )
#'
#' @export
weight_poststratify <- function(population, linked, strata_vars, weights = NULL) {

  # Resolve weights
  w_pop <- NULL
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1) {
      w_pop <- population[[weights]]
    }
  }

  # Create cell identifier
  pop_cells <- interaction(population[, strata_vars, drop = FALSE], drop = TRUE, sep = "_x_")
  lnk_cells <- interaction(linked[, strata_vars, drop = FALSE], drop = TRUE, sep = "_x_")

  # Population cell sizes
  if (is.null(w_pop)) {
    pop_tab <- as.data.frame(table(pop_cells), stringsAsFactors = FALSE)
    names(pop_tab) <- c("cell", "pop_n")
    pop_tab$pop_n <- as.numeric(pop_tab$pop_n)
  } else {
    pop_tab <- stats::aggregate(w_pop, by = list(cell = pop_cells), FUN = sum)
    names(pop_tab) <- c("cell", "pop_n")
  }
  pop_tab$pop_prop <- pop_tab$pop_n / sum(pop_tab$pop_n)

  # Linked cell sizes
  lnk_tab <- as.data.frame(table(lnk_cells), stringsAsFactors = FALSE)
  names(lnk_tab) <- c("cell", "linked_n")
  lnk_tab$linked_n <- as.numeric(lnk_tab$linked_n)
  lnk_tab$linked_prop <- lnk_tab$linked_n / sum(lnk_tab$linked_n)

  # Merge
  strata_table <- merge(pop_tab, lnk_tab, by = "cell", all.x = TRUE)

  # Identify empty cells
  empty <- strata_table$cell[is.na(strata_table$linked_n) | strata_table$linked_n == 0]
  if (length(empty) > 0) {
    cli::cli_warn(
      "{length(empty)} cell{?s} present in population but empty in linked sample: {.val {empty}}"
    )
  }

  # Compute adjustment factor: pop_prop / linked_prop
  strata_table$adj_factor <- strata_table$pop_prop / strata_table$linked_prop
  strata_table$adj_factor[is.na(strata_table$adj_factor) | is.infinite(strata_table$adj_factor)] <- NA_real_

  # Assign weights to linked records
  cell_weights <- stats::setNames(strata_table$adj_factor, strata_table$cell)
  linked_weights <- as.numeric(cell_weights[as.character(lnk_cells)])

  result <- list(
    weights = linked_weights,
    strata_table = strata_table,
    empty_cells = empty
  )
  class(result) <- "linkinfer_poststrat"
  result
}

#' @export
print.linkinfer_poststrat <- function(x, ...) {
  cli::cli_h2("Post-Stratification Weights")
  cli::cli_text("{.val {nrow(x$strata_table)}} cells defined")
  cli::cli_text("Linked records weighted: {.val {sum(!is.na(x$weights))}}")
  if (length(x$empty_cells) > 0) {
    cli::cli_alert_warning("{length(x$empty_cells)} empty cell{?s} in linked sample")
  }

  w <- x$weights[!is.na(x$weights)]
  cli::cli_text("")
  cli::cli_text("Weight summary:")
  cli::cli_text("  Min: {.val {round(min(w), 3)}}  Median: {.val {round(stats::median(w), 3)}}  Max: {.val {round(max(w), 3)}}")
  cli::cli_text("  Effective N: {.val {round(.effective_n(w), 1)}}")
  invisible(x)
}
