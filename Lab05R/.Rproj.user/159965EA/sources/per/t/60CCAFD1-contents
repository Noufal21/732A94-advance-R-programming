context("thenmapsApi")

test_that("Wrong input throws an error.", {
  object = Lab05R::thenmapsApi$new()
  expect_error(object$worldmap('199w') )  
  expect_error(object$multiData("1994","wrongSource") )
  expect_error(object$multiData("192e3","se-4") )  
})