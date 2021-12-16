# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

test_that("Objects work", {
  check_connected()
  expect_type(CreateObject("NLM.Model"), "environment")
  obj <- CreateObject("NLM.Model")
  expect_type(obj$info, "list")
  expect_type(obj$info$MethodNames[1], "character")
  expect_error(obj$Labelz, "Labelz not found in R or GISDK objects")
  obj$Label <- "A logit model"
  expect_equal(obj$Label, "A logit model")
  obj$Clear()
  expect_null(obj$Label, "A logit model")
})
