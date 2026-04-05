test_that("make_targets computes correct proportions", {
  data <- data.frame(
    gender = c("M", "M", "F", "F", "F"),
    region = c("N", "S", "N", "S", "N")
  )

  targets <- make_targets(data, vars = c("gender", "region"))

  expect_equal(length(targets), 2)
  expect_equal(targets$gender[["F"]], 0.6)
  expect_equal(targets$gender[["M"]], 0.4)
  expect_equal(targets$region[["N"]], 0.6)
  expect_equal(targets$region[["S"]], 0.4)
})

test_that("make_targets works with weights", {
  data <- data.frame(
    gender = c("M", "M", "F"),
    wt = c(2, 1, 3)
  )

  targets <- make_targets(data, vars = "gender", weights = "wt")

  # Weighted: M = 3/6 = 0.5, F = 3/6 = 0.5
  expect_equal(targets$gender[["M"]], 0.5)
  expect_equal(targets$gender[["F"]], 0.5)
})

test_that("weight_rake works with autumn", {
  skip_if_not_installed("autumn")
  set.seed(42)

  # Linked sample is 70% male, population is 50/50
  linked_data <- data.frame(
    gender = c(rep("M", 70), rep("F", 30)),
    age_grp = sample(c("young", "old"), 100, replace = TRUE)
  )

  targets <- list(
    gender = c(M = 0.5, F = 0.5)
  )

  result <- weight_rake(linked_data, targets)

  expect_s3_class(result, "linkinfer_rake")
  expect_equal(length(result$weights), 100)

  # After raking, weighted gender proportions should match targets
  w <- result$weights
  prop_m <- sum(w[linked_data$gender == "M"]) / sum(w)
  expect_equal(prop_m, 0.5, tolerance = 0.01)
})
