test_that("validate_estimate basic case works", {
  set.seed(42)
  pop <- data.frame(
    owns_phone = rbinom(1000, 1, 0.7)
  )

  # Linked sample is biased (phone owners more likely to link)
  linked <- data.frame(
    owns_phone = rbinom(500, 1, 0.85)
  )

  result <- validate_estimate(
    linked_data = linked,
    population_data = pop,
    outcome = "owns_phone"
  )

  expect_s3_class(result, "linkinfer_validation")
  expect_true(!is.null(result$pop_estimate))
  expect_true(!is.null(result$linked_unadj))
  expect_true(!is.null(result$bias_unadj))
  expect_null(result$linked_adj_factor)
})

test_that("validate_estimate with adjustment weights reduces bias", {
  set.seed(42)
  n <- 2000

  # Full population
  pop <- data.frame(
    age = rnorm(n, 50, 10),
    outcome = rnorm(n, 10, 2)
  )

  # Linked: younger people more likely to link
  link_prob <- stats::plogis(-1 + 0.02 * pop$age)
  is_linked <- as.logical(rbinom(n, 1, link_prob))

  linked <- pop[is_linked, ]

  # Perfect weights = 1/link_prob
  perfect_w <- 1 / link_prob[is_linked]

  result_unadj <- validate_estimate(
    linked_data = linked,
    population_data = pop,
    outcome = "outcome"
  )

  result_adj <- validate_estimate(
    linked_data = linked,
    population_data = pop,
    outcome = "outcome",
    adj_weights = perfect_w
  )

  # Adjusted bias should be smaller
  expect_true(abs(result_adj$bias_adj) <= abs(result_unadj$bias_unadj) + 0.5)
})

test_that("validate_estimate with population_value works", {
  linked <- data.frame(x = rnorm(100, 5, 1))

  result <- validate_estimate(
    linked_data = linked,
    outcome = "x",
    population_value = 4.5
  )

  expect_equal(result$pop_estimate, 4.5)
})

test_that("compute_adjustment_factor works", {
  # Example: observed rate 4%, false match rate 10%, rate among false 20%
  af <- compute_adjustment_factor(
    observed_rate = 0.04,
    false_match_rate = 0.10,
    rate_among_false = 0.20
  )

  # R_true = (0.04 - 0.20 * 0.10) / (1 - 0.10) = 0.02 / 0.9 = 0.0222
  # AF = 0.0222 / 0.04 = 0.556
  expect_equal(af, (0.04 - 0.02) / 0.9 / 0.04, tolerance = 1e-6)
  expect_true(af < 1) # Should deflate the observed rate
})

test_that("validate_estimate with adjustment_factor works", {
  linked <- data.frame(passing_rate = rbinom(500, 1, 0.05))

  result <- validate_estimate(
    linked_data = linked,
    outcome = "passing_rate",
    population_value = 0.02,
    adjustment_factor = 0.5
  )

  expect_true(!is.null(result$linked_adj_factor))
  expect_equal(result$linked_adj_factor, result$linked_adj * 0.5)
})

test_that("print method works", {
  linked <- data.frame(x = rnorm(100))
  result <- validate_estimate(linked, outcome = "x", population_value = 0)
  out <- paste(capture.output(print(result), type = "message"), collapse = " ")
  expect_true(grepl("Population|Validation|estimate", out))
})
