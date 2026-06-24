#' Simulated census population
#'
#' A simulated dataset representing a full census population of 5,000
#' individuals. Linkage to external records is incomplete, with linkage
#' probability depending on name commonness and race.
#'
#' @format A data.frame with 5,000 rows and 8 columns:
#' \describe{
#'   \item{id}{Unique individual identifier.}
#'   \item{age}{Age at census enumeration.}
#'   \item{sex}{Sex (Male/Female).}
#'   \item{race}{Race (White/Black/Other).}
#'   \item{education}{Education level (< HS, HS, Some college, College+).}
#'   \item{urban}{Urban residence (0 = rural, 1 = urban).}
#'   \item{name_commonness}{Simulated name commonness score (0-1). Higher
#'     values indicate more common names, which are harder to link uniquely.}
#'   \item{linked}{Binary indicator: 1 if successfully linked to mortality
#'     records, 0 otherwise.}
#' }


#' Simulated linked sample
#'
#' The subset of the simulated population that was successfully linked to external
#' records. Contains the race change outcome `race_diff`, the age-at-death outcome,
#' and the validation variable `middle_initial_match`.
#'
#' @format A data.frame with rows for linked individuals and 11 columns:
#' \describe{
#'   \item{id}{Unique individual identifier.}
#'   \item{age}{Age at census enumeration.}
#'   \item{sex}{Sex (Male/Female).}
#'   \item{race}{Race (White/Black/Other).}
#'   \item{education}{Education level.}
#'   \item{urban}{Urban residence indicator.}
#'   \item{name_commonness}{Name commonness score.}
#'   \item{age_at_death}{Age at death from mortality records. For false
#'     matches, this is the age at death of a different individual.}
#'   \item{middle_initial}{Middle initial from census records.}
#'   \item{middle_initial_match}{Binary indicator (1=match, 0=mismatch) for the middle initial
#'     between census and linked records.}
#' }

