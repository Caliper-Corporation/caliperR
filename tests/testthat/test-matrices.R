# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

open_matrix <- function() {
  orig <- system.file("extdata", "gisdk", "testing", "toy_matrix.mtx", package = "caliperR")
  mtx_file <- tempfile(fileext = ".mtx")
  file.copy(orig, mtx_file)
  mtx <- dk$OpenMatrix(mtx_file, NA)
  return(mtx)
}

test_that("matrix objects are created", {
  check_connected()
  matrix <- open_matrix()
  expect_type(matrix, "environment")
  expect_s4_class(matrix$handle, "COMIDispatch")
  expect_type(matrix$cores, "list")
  expect_s4_class(matrix$cores[[1]], "MatrixCurrency")
  expect_s4_class(matrix$cores[[1]]@com, "COMIDispatch")
  expect_equal(names(matrix$indices), c("row", "column"))
  expect_type(matrix$indices$row, "character")
})

test_that("matrix generics work", {
  check_connected()
  matrix <- open_matrix()
  expect_is(summary(matrix), "data.frame")
  expect_is(as.data.frame(matrix), "data.frame")
  expect_is(as.matrix(matrix), "list")
  expect_is(as.matrix(matrix$core_a), "matrix")
})

test_that("matrix indices work", {
  check_connected()
  matrix <- open_matrix()
  matrix$column_index <- "subset"
  c_labels <- dk$GetMatrixColumnLabels(matrix$core_a)
  expect_equal(c_labels, list("1", "2", "3"))
  df <- as.data.frame(matrix)
  expect_equal(nrow(df), 15)
})

test_that("updating a matrix works", {
  check_connected()
  mtx <- open_matrix()
  new_data <- matrix(seq(1, 4), nrow = 2, ncol = 2,
                     dimnames = list(c(1,2), c(1,2)))
  mtx$core_b <- new_data
  mtx <- as.matrix(mtx$core_b)
  expect_equal(as.list(mtx)[1:2], list(1, 2))
  expect_equal(as.list(mtx)[6:7], list(3, 4))
})

test_that("adding a core works", {
  check_connected()
  mtx <- open_matrix()
  mtx$CreateCore("core_c")
  expect_equal(length(mtx$cores), 3)
})

test_that("adding a matrix index works", {
  check_connected()
  mtx <- open_matrix()
  mtx$CreateIndex("new", c(1, 2, 3, 4))
  expect_equal(length(mtx$indices$row), 3)
  mtx$row_index <- "new"
  expect_equal(nrow(as.matrix(mtx$core_a)), 4)
})
