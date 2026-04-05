test_that("compare_linked works with continuous variables", {
  set.seed(42)
  population <- data.frame(
    age = rnorm(1000, 50, 10),
    income = rnorm(1000, 50000, 15000)
  )
  # Linked sample is slightly younger
  linked <- data.frame(
    age = rnorm(600, 48, 10),
    income = rnorm(600, 50000, 15000)
  )

  result <- compare_linked(population, linked, vars = c("age", "income"))

  expect_s3_class(result, "linkinfer_comparison")
  expect_equal(nrow(result), 2)
  expect_true(all(c("variable", "pop_mean", "linked_mean", "std_diff") %in% names(result)))
  expect_true(abs(result$std_diff[result$variable == "age"]) > 0.05)
})

test_that("compare_linked works with categorical variables", {
  set.seed(42)
  population <- data.frame(
    gender = sample(c("M", "F"), 500, replace = TRUE, prob = c(0.5, 0.5))
  )
  linked <- data.frame(
    gender = sample(c("M", "F"), 300, replace = TRUE, prob = c(0.6, 0.4))
  )

  result <- compare_linked(population, linked, vars = "gender")

  expect_s3_class(result, "linkinfer_comparison")
  expect_true(all(result$type == "categorical"))
  expect_true(all(result$level %in% c("F", "M")))
})

test_that("compare_linked handles simple numeric data", {
  population <- data.frame(x = 1:10)
  linked <- data.frame(x = 1:5)

  result <- compare_linked(population, linked, vars = "x")
  expect_equal(result$pop_mean, 5.5)
  expect_equal(result$linked_mean, 3)
})

test_that("print method works", {
  population <- data.frame(x = rnorm(100))
  linked <- data.frame(x = rnorm(50))
  result <- compare_linked(population, linked, vars = "x")
  out <- capture.output(print(result))
  expect_true(any(grepl("variable|pop_mean|Linked", out)))
})
