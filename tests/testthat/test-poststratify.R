test_that("weight_poststratify produces correct adjustment factors", {
  # Population: 50% urban, 50% rural
  population <- data.frame(
    area = c(rep("urban", 500), rep("rural", 500))
  )
  # Linked: 70% urban, 30% rural (urban overrepresented)
  linked <- data.frame(
    area = c(rep("urban", 350), rep("rural", 150))
  )

  result <- weight_poststratify(population, linked, strata_vars = "area")

  expect_s3_class(result, "linkinfer_poststrat")
  expect_equal(length(result$weights), 500)

  # Rural should get higher weight (underrepresented in linked)
  rural_w <- result$weights[linked$area == "rural"]
  urban_w <- result$weights[linked$area == "urban"]
  expect_true(mean(rural_w) > mean(urban_w))
})

test_that("weight_poststratify warns on empty cells", {
  population <- data.frame(group = c("A", "B", "C"))
  linked <- data.frame(group = c("A", "B"))

  expect_warning(
    weight_poststratify(population, linked, strata_vars = "group"),
    "empty"
  )
})

test_that("weight_poststratify handles multiple strata", {
  set.seed(42)
  population <- data.frame(
    gender = sample(c("M", "F"), 2000, replace = TRUE),
    age_grp = sample(c("young", "old"), 2000, replace = TRUE)
  )
  linked <- data.frame(
    gender = sample(c("M", "F"), 1000, replace = TRUE),
    age_grp = sample(c("young", "old"), 1000, replace = TRUE)
  )

  result <- weight_poststratify(population, linked, strata_vars = c("gender", "age_grp"))

  expect_s3_class(result, "linkinfer_poststrat")
  expect_equal(nrow(result$strata_table), 4)
})
