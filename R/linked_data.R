#' Simulate linked census data
#'
#' Generates a synthetic census‑style population and a linked subsample with
#' characteristics resembling the 2020 US Census. The function is primarily for
#' package vignettes and testing.
#'
#' @param n Integer. Population size to simulate.
#' @param seed Optional integer. If supplied, the random number generator is set locally
#'   to this value before adding stochastic noise to `linked_prob`. This makes the
#'   simulated `linked_prob` reproducible without affecting the global RNG.

#' @param p Numeric in (0,1). Approximate proportion of the population that is
#'   successfully linked to a mortality record.
#' @param v1 Numeric in (0,1). Proportion of linked observations that have a
#'   *valid* middle‑initial variable (i.e., the validation variable is observed).
#' @param v2 Numeric in (0,1). Among linked records with a valid middle‑initial,
#'   the proportion that have a matching middle initial between census and
#'   mortality records.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{population}{Data frame of the full simulated population.}
#'     \item{linked}{Data frame of the linked subsample. Includes the
#'       validation variable `middle_initial_match`, the race change variable
#'       `race_diff`, and the age-at-death outcome `age_at_death`.}
#'   }
#' @export
linked_data <- function(n, p, v1, v2, seed = NULL) {
  if (any(c(p, v1, v2) <= 0 | c(p, v1, v2) >= 1)) {
    rlang::abort("`p`, `v1`, and `v2` must be between 0 and 1 (exclusive).")
  }
  #----- Simulate population -------------------------------------------------
  ID <- 1:n
  age <- sample(0:100, n, replace = TRUE)
  sex <- sample(c("Male", "Female"), n, replace = TRUE)
  race <- sample(c("White", "Black", "Other"), n, replace = TRUE,
                 prob = c(0.6, 0.2, 0.2))
  education <- sample(c("< HS", "HS", "Some college", "College+"), n, replace = TRUE,
                      prob = c(0.2, 0.3, 0.3, 0.2))
  urban <- rbinom(n, 1, 0.7)
  name_commonness <- runif(n)
  # linkage probability higher for less common names with added stochastic error
  linked_prob_raw <- (1 - name_commonness) * 0.8 + 0.1  # base range roughly 0.1‑0.9
  # Add random noise to introduce variability (so models are not perfectly predicted)
  linked_prob_raw <- pmin(pmax(linked_prob_raw + rnorm(n, sd = 0.05), 0), 1)
  # Stretch raw probabilities to span the full [0, 1] interval
  lp_min <- min(linked_prob_raw)
  lp_max <- max(linked_prob_raw)
  linked_prob_norm <- (linked_prob_raw - lp_min) / (lp_max - lp_min)
  # Scale to achieve the desired overall linkage proportion `p`
  scaling <- p / mean(linked_prob_norm)
  linked_prob <- pmin(linked_prob_norm * scaling, 1)
  # Determine linked records probabilistically based on the final probabilities
  linked <- rbinom(n, 1, linked_prob)

  # Mortality outcome generation removed as mortality is no longer part of the simulated data.

  population <- data.frame(
    ID = ID,
    age = age,
    sex = sex,
    race = race,
    education = education,
    urban = urban,
    name_commonness = name_commonness,
    linked_prob = linked_prob,
    linked = linked,
    stringsAsFactors = FALSE
  )

  #----- Construct linked sample --------------------------------------------
  linked_idx <- which(linked == 1)
  linked_pop <- population[linked_idx, , drop = FALSE]

  # race_diff: true change rate ~0.1, higher among lower linked_prob
  diff_base <- 0.1
  diff_prob <- pmin(diff_base + 0.2 * (1 - linked_pop$linked_prob), 1)
  linked_pop$race_diff <- rbinom(nrow(linked_pop), 1, diff_prob)

  # validation variable: middle_initial_match
  # First decide whether middle initial is observed (valid) according to v1
  has_mid <- rbinom(nrow(linked_pop), 1, v1)
  # Match probability increases with linked_prob (higher linked_prob => higher chance)
  match_prob_mid <- pmin(0.5 + 0.5 * linked_pop$linked_prob, 1)  # between 0.5‑1
  # For those with observed middle initial, generate matches with overall proportion v2
  match <- rbinom(sum(has_mid), 1, v2) * has_mid[has_mid == 1]
  # Build vector for all linked rows
  middle_initial_match <- rep(NA_integer_, nrow(linked_pop))
  middle_initial_match[has_mid == 1] <- match

  linked_pop$middle_initial_match <- middle_initial_match

  # age_at_death: additional years lived after census based on demographics
  # Base extra years (Poisson with lambda=2) then scale by modifiers
  base_extra <- rpois(nrow(linked_pop), lambda = 2)
  mod_female <- ifelse(linked_pop$sex == "Female", 1.2, 1)
  mod_white <- ifelse(linked_pop$race == "White", 1.2, 1)
  mod_young <- ifelse(linked_pop$age < 40, 1.3, 1)
  mod_edu <- ifelse(linked_pop$education == "College+", 1.3,
                ifelse(linked_pop$education == "HS", 1.1, 1))
  mod_urban <- ifelse(linked_pop$urban == 1, 1.1, 1)
  mult <- mod_female * mod_white * mod_young * mod_edu * mod_urban
  add_years <- pmax(round(base_extra * mult), 0)
  # Age at death: add simulated extra years to age for all linked records
  linked_pop$age_at_death <- linked_pop$age + add_years

  # Return both data frames
  list(population = population, linked = linked_pop)
}
