# Simulate example datasets for linkinfer
#
# Scenario: Census records linked to mortality records.
# The full census population has demographics (age, sex, race, birthplace).
# Linkage to mortality records is incomplete -- individuals with common names
# are harder to link (more missed matches), and some false matches exist.

set.seed(2026)

n_pop <- 5000

# --- Census population ---
census_pop <- data.frame(
  id = 1:n_pop,
  age = round(rnorm(n_pop, 45, 15)),
  sex = sample(c("Male", "Female"), n_pop, replace = TRUE),
  race = sample(
    c("White", "Black", "Other"),
    n_pop, replace = TRUE, prob = c(0.7, 0.2, 0.1)
  ),
  education = sample(
    c("< HS", "HS", "Some college", "College+"),
    n_pop, replace = TRUE, prob = c(0.25, 0.35, 0.20, 0.20)
  ),
  urban = sample(c(0, 1), n_pop, replace = TRUE, prob = c(0.4, 0.6)),
  name_commonness = runif(n_pop, 0, 1)
)

# Outcome: age at death (correlated with education and race)
edu_effect <- c("< HS" = 0, "HS" = 1.5, "Some college" = 3, "College+" = 5)
race_effect <- c("White" = 0, "Black" = -2, "Other" = -0.5)

census_pop$age_at_death <- round(
  70 +
    edu_effect[census_pop$education] +
    race_effect[census_pop$race] +
    0.1 * census_pop$age +
    rnorm(n_pop, 0, 8)
)

# Middle initial: validation variable (not used in linkage)
census_pop$middle_initial <- sample(LETTERS, n_pop, replace = TRUE)

# --- Linkage process ---
# Linkage probability depends on name commonness (common names = harder to link)
# and race (Black individuals slightly less likely to be linked due to data quality)
link_prob <- plogis(
  0.5 -
    1.5 * census_pop$name_commonness +
    ifelse(census_pop$race == "Black", -0.3, 0) +
    0.01 * census_pop$age
)

census_pop$linked <- rbinom(n_pop, 1, link_prob)

# Among linked records, introduce ~5% false matches
n_linked <- sum(census_pop$linked)
n_false <- round(0.05 * n_linked)
false_idx <- sample(which(census_pop$linked == 1), n_false)

# For false matches, scramble the mortality outcome (age_at_death)
census_pop$age_at_death_observed <- census_pop$age_at_death
census_pop$age_at_death_observed[false_idx] <- sample(
  census_pop$age_at_death, n_false
)

# For false matches, middle initials will disagree
census_pop$middle_initial_mortality <- census_pop$middle_initial
census_pop$middle_initial_mortality[false_idx] <- sample(LETTERS, n_false, replace = TRUE)

# Flag false matches (in real data this is unknown)
census_pop$is_false_match <- FALSE
census_pop$is_false_match[false_idx] <- TRUE

# --- Save datasets ---
# Full population (what researcher has from census)
census_population <- census_pop[, c(
  "id", "age", "sex", "race", "education", "urban",
  "name_commonness", "linked"
)]

# Linked sample (what researcher has after linkage)
linked_sample <- census_pop[census_pop$linked == 1, c(
  "id", "age", "sex", "race", "education", "urban",
  "name_commonness", "age_at_death_observed",
  "middle_initial", "middle_initial_mortality",
  "is_false_match"
)]
names(linked_sample)[names(linked_sample) == "age_at_death_observed"] <- "age_at_death"
rownames(linked_sample) <- NULL

usethis::use_data(census_population, linked_sample, overwrite = TRUE)
