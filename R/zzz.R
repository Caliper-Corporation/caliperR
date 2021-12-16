# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

# Store package-wide variables here rather than the global environment
caliper_env <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  disconnect()
}

.onDetach <- function(libpath) {
  disconnect()
}
