# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

#' R class that gives access to GISDK functions
#'
#' This class makes it easy to call any arbitrary GISDK function from R.
#'
#' Performs multiple dispatch based on the name of the method called. If the
#' named is one of a few caliperR functions, those are executed. If not, then
#' then the name is assumed to represent a GISDK function and is called over
#' COM.
#'
#' @import R6
#' @export

GisdkClass <- R6::R6Class("GisdkClass",
  public = list(

    #' @description
    #' On creation, attempts to connect to Caliper software if not
    #' already connected.
    #' @return A new \code{GisdkClass} object.
    initialize = function() {
      if (!connected()) {
        connect()
      }
    },

    #' @description
    #' Class implementation of \code{caliperR::RunMacro()}.
    #' @param macro_name \code{string} The GISDK macro name to run.
    #' @param ... The arguments to pass to the GISDK macro.
    #' @param process_result \code{boolean} Whether to attempt to convert GISDK
    #' data types to R. Defaults to true.
    RunMacro = function(macro_name, ..., process_result = TRUE) {
      caliperR::RunMacro(macro_name, ..., process_result)
    },

    #' @description
    #' Class implementation of \code{caliperR::CreateObject()}.
    #' @param class_name \code{string} The name of the GISDK object to create.
    #' @param ... Any arguments to be passed to the object during creation.
    CreateObject = function(class_name, ...) {
      caliperR::CreateObject(class_name, ...)
    },

    #' @description
    #' Class implementation of \code{caliperR::SetAlternateInterface()}.
    #' @param ui_file \code{string} File path to the compiled UI file.
    SetAlternateInterface = function(ui_file = NULL) {
      caliperR::SetAlternateInterface(ui_file)
    },

    #' @description
    #' Class implementation of \code{caliperR::GetInterface()}.
    GetInterface = function() {
      caliperR::GetInterface()
    }
  )
)

#' S3 method for calling \code{GisdkClass} object methods
#'
#' Makes \code{GisdkClass} objects smarter about whether you are calling a
#' method from the R object or the underlying GISDK library.
#'
#' @details
#'
#' If \code{name} is a method of the R object, then it is executed. Otherwise,
#' it tries to execute a GISDK function of the same name.
#'
#' @param x A \code{GisdkClass} object
#' @param name The method to dispatch
#' @param ... The arguments to pass to the method.
#' @inheritParams RunMacro
#' @export

`$.GisdkClass` <- function(x, name, ..., process_result = TRUE) {
  args <- list(...)
  # If the name references an R method/attribute
  if (exists(name, envir = x)) {
    .subset2(x, name)
  # Otherwise, it must reference a GISDK function
  } else {
    function(...) {
      caliperR::RunFunction(name, ...)
    }
  }
}
