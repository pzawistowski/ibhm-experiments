context("weighted correlation")

library(IBHM)

test_that("weighted linear correlation is sane - case 1", {
  # Given
  x1 = 1:20
  x2 = 2*x1
  
  # When & Then
  expect_equal(weightedR(x1,x2, rep(1, 20)), 1.0)
})

test_that("weighted linear correlation is sane - case 2", {
  # Given
  x1 = 1:20
  x2 = -3*x1
  
  # When & Then
  expect_equal(weightedR(x1,x2, rep(1, 20)), -1.0)
})


test_that("weighted linear correlation is sane - case 3", {
  # Given
  n = 10
  x1 = c(runif(n), 1:n, runif(n))
  x2 = c(runif(n), 2*(1:n), runif(n))
  w = c(rep(0, n), rep(1,n), rep(0,n))
  
  # When & Then
  expect_equal(weightedR(x1,x2, w), 1.0)
})


test_that("weighted linear correlation handles degenerate weights", {
  # Given
  x1 = 1:20
  x2 = -3*x1
  
  # When & Then
  expect_true(is.nan(weightedR(x1,x2, rep(0, 20))))
})
