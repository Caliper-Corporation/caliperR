# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

library(testthat)
library(caliperR)

check_connected <- function() {
  if (!connected()) {
    skip("Not connected to Caliper software")
  }
}

# unzip the compiled UI file
zip_file <- system.file("extdata", "gisdk", "testing", "my_ui.zip", package = "caliperR")
tempdir <- tempdir()
unzip(zip_file, exdir = tempdir, setTimes = TRUE)
ui_path <- file.path(tempdir, "my_ui.dbd")

dk <- connect()
test_check("caliperR")

# for interactive testing:
# test_dir(path = "tests/testthat")
