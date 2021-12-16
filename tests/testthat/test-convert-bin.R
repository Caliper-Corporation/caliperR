# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

# to reduce test time, I grouped all the COM test and non-COM tests together.
# This eliminates repeated connecting/disconnecting.

test_that("reading a bin file works over COM", {
  check_connected()
  bin_file <- system.file(
    "extdata", "gisdk", "testing", "toy_table.bin", package = "caliperR"
  )
  df <- read_bin(bin_file)
  dnames <- read_bin(bin_file, returnDnames = TRUE)
  expect_equal(df$field_b[[1]], "a") # white space removed
  expect_true(is.na(df$field_b[[3]])) # convert null characters to NA
  expect_equal(typeof(df[[3]]), 'character') # preserve type for null fields
  expect_equal(typeof(df[[5]]), 'double') # preserve type for null fields
  expect_equal(nrow(df), 6) # make sure the deleted record isn't read in.
  expect_equal(Hmisc::label(df)[[1]], "first field") # field descriptions
})
