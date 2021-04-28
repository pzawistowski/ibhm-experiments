context("scalarization")

library(IBHM)

v = function(...){
  matrix(data=c(...), ncol=1)  
}

test_that("radial scalarization is sane - 1D", {
  # Given
  radial =  ScalFunctions("radial")[[1]]
  x = matrix(1:3, ncol = 1)

  # When & Then
  expect_equal(radial(x, c(1,1)), v(0,1,4))
  expect_equal(radial(x, c(1,2)), v(1,0,1))
  expect_equal(radial(x, c(2,2)), v(2,0,2))
})


test_that("radial scalarization is sane - 2D", {
  # Given
  radial = ScalFunctions("radial")[[1]]
  x = matrix(1:4, ncol = 2, byrow = T)
  
  # When & Then
  expect_equal(radial(x, c(1,1,2)), v(0,8))
})


test_that("root.radial scalarization is sane - 1d", {
  # Given
  radial = ScalFunctions("root.radial")[[1]]
  x = matrix(1:3, ncol = 1)
  
  # When & Then
  expect_equal(radial(x, c(1,1)), v(0,1,2))
  expect_equal(radial(x, c(1,2)), v(1,0,1))
  expect_equal(radial(x, c(2,2)), v(2,0,2))
})


test_that("root.radial scalarization is sane - 2d", {
  # Given
  radial = ScalFunctions("root.radial")[[1]]
  x = matrix(1:4, ncol = 2, byrow = T)
  
  # When & Then
  expect_equal(radial(x, c(1,1,2)), v(0,sqrt(8)))
})


test_that("dot.pr scalarization is sane - 1d", {
  # Given
  dopr = ScalFunctions("dot.pr")[[1]]
  x = matrix(1:3, ncol = 1)
  
  # When & Then
  expect_equal(dopr(x, c(0,1)), v(1,2,3))
  expect_equal(dopr(x, c(1,1)), v(2,3,4))
  expect_equal(dopr(x, c(2,0)), v(2,2,2))
})


test_that("dot.pr scalarization is sane - 2d", {
  # Given
  dopr = ScalFunctions("dot.pr")[[1]]
  x = matrix(1:6, ncol = 2, byrow = T)
  
  # When & Then
  expect_equal(dopr(x, c(0,1,0)), v(1,3,5))
  expect_equal(dopr(x, c(0,0,1)), v(2,4,6))
  expect_equal(dopr(x, c(0,1,1)), v(3,7,11))
  expect_equal(dopr(x, c(1,0,0)), v(1,1,1))
})