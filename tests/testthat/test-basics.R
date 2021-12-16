# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

test_that("GISDK functions work", {
  check_connected()
  folder <- RunMacro("G30 Tutorial Folder")
  expect_equal(
    dk$OpenTable("airports", "ffb", list(paste0(folder, "airports.bin"), NA)),
    "airports"
  )
  expect_error(
    RunFunction("CreateObject"),
    "Use caliperR::CreateObject()",
    fixed = TRUE
  )
})

test_that("RunMacro works", {
  check_connected()
  SetAlternateInterface(ui_path)
  expect_type(RunMacro("G30 Tutorial Folder"), "character")
  SetAlternateInterface()
})

test_that("logging works", {
  dk$ShowMessage("This is a test.")
  tbl <- read_log()
  expect_equal(class(tbl), "data.frame")
})

test_that("Type conversion works", {
  check_connected()
  SetAlternateInterface(ui_path)
  expect_equal(RunMacro("return array"), list(1, 2))
  expect_equal(RunMacro("return vector"), c(1, 2))
  expect_equal(
    RunMacro("parse opts array", list("one" = 1)),
    "The first option name is one. The first option value is 1."
  )
  expect_equal(RunMacro("test nested vector")$test, c(1, 2, NA))
  SetAlternateInterface(ui_path)
  expect_mapequal(
    RunMacro("return named array"),
    list("one" = 1, "two" = 2)
  )
  expect_setequal(
    RunMacro("return nested array"),
    list(list(1, 2), list(3, 4))
  )
  SetAlternateInterface()
  expect_type(caliperR:::convert_nulls_and_slashes(NA), "complex")
  # Identify and modify file path strings correctly
  expect_equal(caliperR:::convert_nulls_and_slashes("a/b"), "a/b")
  expect_equal(caliperR:::convert_nulls_and_slashes("C:/Users"), "C:\\Users")
})

test_that("A nested list without names converts correctly", {
  check_connected()
  test1 <- list(
    NA,
    "a/b",
    list(
      NA,
      "c/d",
      "C:/Users"
    )
  )
  expect_setequal(
    caliperR:::process_gisdk_args(test1),
    list(list(NA_complex_, "a/b", list(NA_complex_, "c/d", "C:\\Users")))
  )
})

test_that("A nested list with names converts correctly", {
  check_connected()
  test1 <- list(
    "one" = NA
  )
  expect_setequal(
    caliperR:::process_gisdk_args(test1),
    list(list(list("one", NA_complex_)))
  )

  test2 <- list(
    "one" = list(1, 2, 3)
  )
  expect_setequal(
    caliperR:::process_gisdk_args(test2),
    list(list(list("one", list(1, 2, 3))))
  )

  test3 <- list(
    "one" = NA,
    "two" = list(
      "two_b" = "C:/Users"
    )
  )
  expect_setequal(
    caliperR:::process_gisdk_args(test3),
    list(list(list("one", NA_complex_), list("two", list("two_b", "C:\\Users"))))
  )
})

test_that("compiling gisdk works", {
  rsc_file <- system.file("extdata", "gisdk", "testing", "gisdk.rsc", package = "caliperR")
  ui_path <- compile_gisdk(rsc_file, tempfile(fileext = ".dbd"))
  SetAlternateInterface(ui_path)
  expect_equal(RunMacro("first macro"), "Hello World!")
  SetAlternateInterface()
})
