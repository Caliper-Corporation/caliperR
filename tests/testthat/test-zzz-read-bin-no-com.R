# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

test_that("reading with and without COM give same data", {
  check_connected()
  bin_file <- system.file(
    "extdata", "gisdk", "testing", "toy_table.bin", package = "caliperR"
  )
  df1 <- read_bin(bin_file)
  disconnect()
  df2 <- read_bin(bin_file)
  expect_true(all(df2 == df1, na.rm = TRUE)) # that all data is the same
  expect_true(all(unlist(lapply(df1, typeof)) == unlist(lapply(df2, typeof))))
})

test_that("reading a bin file works without COM", {
  disconnect()
  bin_file <- system.file(
    "extdata", "gisdk", "testing", "toy_table.bin", package = "caliperR"
  )
  df <- read_bin(bin_file)
  dnames <- read_bin(bin_file, returnDnames = TRUE)
  expect_equal(df$second[[1]], "a") # white space removed
  expect_true(is.na(df$second[[3]])) # convert null characters to NA
  expect_equal(typeof(df[[3]]), 'character') # preserve type for null fields
  expect_equal(typeof(df[[5]]), 'double') # preserve type for null fields
  expect_equal(nrow(df), 6) # make sure the deleted record isn't read in.
  expect_equal(Hmisc::label(df)[[1]], "first field") # field descriptions
})

test_that("writing a bin file works without COM", {
  disconnect()
  bin_file <- system.file(
    "extdata", "gisdk", "testing", "toy_table.bin", package = "caliperR"
  )
  df1 <- read_bin(bin_file)
  dnames <- read_bin(bin_file, returnDnames = TRUE)
  temp_bin <- tempfile(fileext = ".bin")
  write_bin(df1, temp_bin, dnames = dnames)
  df2 <- read_bin(temp_bin)
  expect_true(all(unlist(lapply(df1, typeof)) == unlist(lapply(df2, typeof))))
  temp_dcb <- gsub("\\.bin", "\\.dcb", temp_bin)
})
