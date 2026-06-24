test_that("weight_rake correctly balances marginal distributions", {
  set.seed(42)

  # Population: equal gender distribution
  population <- data.frame(gender = rep(c("M", "F"), each = 500))

  # Linked sample: over‑represents males (70% male)
  linked_data <- data.frame(gender = c(rep("M", 700), rep("F", 300)))

  result <- weight_rake(linked_data, population, vars = "gender")

  expect_s3_class(result, "linkinfer_rake")
  expect_equal(length(result$weights), nrow(linked_data))

  # After raking, weighted gender proportions should be ~0.5 each
  w <- result$weights
  prop_m <- sum(w[linked_data$gender == "M"]) / sum(w)
  expect_equal(prop_m, 0.5, tolerance = 0.01)
})
