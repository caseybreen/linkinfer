test_that("weight_ipw logistic method works", {
  set.seed(42)
  n <- 1000
  age <- rnorm(n, 50, 10)
  p_link <- stats::plogis(-1 + 0.02 * age)
  linked_flag <- rbinom(n, 1, p_link)

  population <- data.frame(age = age)
  linked <- population[linked_flag == 1, , drop = FALSE]

  result <- weight_ipw(population, linked, covariates = "age", method = "logistic")

  expect_s3_class(result, "linkinfer_ipw")
  expect_equal(length(result$weights), nrow(linked))
  expect_true(all(result$weights > 0))
  expect_equal(result$method, "logistic")
})

test_that("weight_ipw ml method works", {
  skip_if_not_installed("ranger")
  set.seed(42)
  n <- 500
  population <- data.frame(
    age = rnorm(n, 50, 10),
    edu = sample(1:4, n, replace = TRUE)
  )
  linked_flag <- rbinom(n, 1, stats::plogis(-0.5 + 0.01 * population$age))
  linked <- population[linked_flag == 1, , drop = FALSE]

  result <- weight_ipw(population, linked, covariates = c("age", "edu"),
                       method = "ml", ranger_args = list(num.trees = 100))

  expect_s3_class(result, "linkinfer_ipw")
  expect_equal(result$method, "ml")
  expect_equal(length(result$weights), nrow(linked))
})

test_that("weight_ipw stabilized vs unstabilized", {
  set.seed(42)
  n <- 500
  population <- data.frame(x = rnorm(n))
  linked_flag <- rbinom(n, 1, 0.6)
  linked <- population[linked_flag == 1, , drop = FALSE]

  stab <- weight_ipw(population, linked, covariates = "x", stabilize = TRUE)
  unstab <- weight_ipw(population, linked, covariates = "x", stabilize = FALSE)

  # Stabilized weights should have mean closer to 1
  expect_true(abs(mean(stab$weights) - 1) < abs(mean(unstab$weights) - 1) + 1)
})

test_that("ipw_diagnostics works", {
  set.seed(42)
  n <- 500
  population <- data.frame(age = rnorm(n, 50, 10))
  linked_flag <- rbinom(n, 1, 0.6)
  linked <- population[linked_flag == 1, , drop = FALSE]

  ipw_result <- weight_ipw(population, linked, covariates = "age")
  diag <- ipw_diagnostics(ipw_result, population, linked)

  expect_s3_class(diag, "linkinfer_ipw_diagnostics")
  expect_true("balance" %in% names(diag))
  expect_true("weight_summary" %in% names(diag))
  expect_true("effective_n" %in% names(diag$weight_summary))
})

test_that("print methods work", {
  set.seed(42)
  population <- data.frame(x = rnorm(200))
  linked_flag <- rbinom(200, 1, 0.5)
  linked <- population[linked_flag == 1, , drop = FALSE]

  result <- weight_ipw(population, linked, covariates = "x")
  out <- paste(capture.output(print(result), type = "message"), collapse = " ")
  expect_true(grepl("Inverse|Weight|Method", out))

  diag <- ipw_diagnostics(result, population, linked)
  out2 <- paste(capture.output(print(diag), type = "message"), collapse = " ")
  expect_true(grepl("IPW|Balance|Effective", out2))
})
