# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

#' Creates an R object that represents a GISDK object
#'
#' A simple function that merely abstracts Rs creation language for R6 objects
#' to make it look more like GISDK. See \code{\link{CaliperClass}} for details
#' on the class and how to work with objects.
#'
#' @export
#' @param class_name The class name of the GISDK object to create.
#' @param ... Any additional arguments to use during GISDK object creation.
#' @examples
#' \dontrun{
#' obj <- CreateObject("NLM.Model")
#' }

CreateObject <- function(class_name, ...) {
  CaliperClass$new(class_name, ...)
}

#' R class representing GISDK objects
#'
#' This class simplifies working with GISDK objects over COM from R.
#'
#' When created, an object of this class contains a COM pointer to a sister
#' object in Caliper software. In addition to this reference, contained in the
#' \code{ref} field, there are several other fields and methods to facilitate
#' object manipulation. Finally, the \code{$} operator has been overloaded for
#' this class to make code easier to write.
#'
#' @import R6
#' @export
#' @examples
#' \dontrun{
#' # object creation (two options)
#' obj <- CreateObject("NLM.Model")
#' obj <- CaliperClass$new("NLM.Model")
#'
#' # get info about the GISDK object fields and methods
#' obj$info
#'
#' # set an attribute/field label of the GISDK object
#' # (Does not modify the R object in any way)
#' obj$Label <- "a logit model"
#'
#' # run one of the GISDK object's methods: Clear()
#' # (Does not modify the R object in any way)
#' obj$Clear()
#'
#' # object methods can be chained
#' obj$
#'  Clear()$
#'  Read()
#' }

CaliperClass <- R6::R6Class("CaliperClass",
  public = list(

    #' @field g_class_name The class name of the underlying GISDK object
    #'   (e.g. "NLM.Model").
    g_class_name = NULL,

    #' @field ref This is a COM pointer and represents the object created in a
    #'   Caliper program.
    ref = NULL,

    #' @field info This is a simple R list that can be used to see the fields
    #'   and methods of the underlying GISDK object.
    info = NULL,

    #' @description
    #' Create a new \code{CaliperClass}
    #' @param class_name \code{string} The name of the GISDK class to create.
    #' @param ... Additional arguments to be supplied to GISDK object creation.
    #' @return A new \code{CaliperClass} object.
    initialize = function(class_name, ...) {
      if (!connected()) stop("Not connected to Caliper software.")
      if (!is.character(class_name)) stop("'class_name' must be a string")
      tryCatch(
        self$ref <- RunMacro("create_object", NA, class_name, ...),
        error = function(e) {
          e$message <- paste0("Object '", class_name, "' could not be created")
          stop(e)
        }
      )
      self$g_class_name <- class_name
      flat_info_list <- RunMacro("get_class_info", self$ref)
      self$info <- lapply(flat_info_list, unlist)
    },

    #' @description
    #' Runs a method of the associated GISDK object.
    #' @param method \code{string} The name of the method to apply.
    #' @param ... Additional arguments supplied to the GISDK method.
    apply_gisdk_method = function(method, ...) {
      stopifnot(is.character(method))
      args <- list(...)
      args <- list("apply_method", self$ref, method, args)
      tryCatch(
        do.call(RunMacro, args),
        error = function(e) {
          e$message <- paste0(method, "() failed.")
          stop(e)
        }
      )
      invisible(self)
    },

    #' @description
    #' Retrieve an attribute from the GISDK object.
    #' @param attribute \code{string} The name of the GISDK attribute to get.
    #' @return The GISDK attribute value.
    get_gisdk_attribute = function(attribute) {
      stopifnot(is.character(attribute))
      args <- list("get_attribute", self$ref, attribute)
      tryCatch(
        value <- do.call(RunMacro, args),
        error = function(e) {
          e$message <- paste0("Setting ", attribute, " failed.")
          stop(e)
        }
      )
      process_gisdk_result(value)
    },

    #' @description
    #' Set an attribute of the GISDK object.
    #' @param attribute \code{string} The name of the GISDK attribute to get.
    #' @param value The value to assign to the attribute.
    set_gisdk_attribute = function(attribute, value) {
      stopifnot(is.character(attribute))
      value <- process_gisdk_args(value)[[1]]
      args <- list("set_attribute", self$ref, attribute, value)
      tryCatch(
        do.call(RunMacro, args),
        error = function(e) {
          e$message <- paste0("Setting ", attribute, " failed.")
          stop(e)
        }
      )
      invisible(self)
    }
  )
)

#' S3 method for calling \code{CaliperClass} object methods
#'
#' Makes \code{CaliperClass} objects smarter about whether you are calling a
#' method from the R object or the underlying GISDK object over COM.
#'
#' @details
#'
#' If \code{name} is an attribute of the R object (like \code{$info}), then
#' the value of that attribute is returned. Otherwise, it looks into the fields
#' and methods of the underlying GISDK object to determine what to do.
#'
#' @param x A \code{CaliperClass} object
#' @param name the method to dispatch
#' @export

`$.CaliperClass` <- function(x, name) {
  info <- .subset2(x, "info")
  # If the name references an R method/attribute
  if (exists(name, envir = x)) {
    .subset2(x, name)
  # If the name references a method of the GISDK object
  } else if (name %in% info$MethodNames) {
    function(...) {
      .subset2(x, "apply_gisdk_method")(name, ...)
    }
  # If the name references a field of the GISDK object
  } else if (name %in% info$FieldNames) {
      .subset2(x, "get_gisdk_attribute")(name)
  } else {
    stop(paste0(name, " not found in R or GISDK objects"))
  }
}

#' S3 method for assigning \code{CaliperClass} object attributes
#'
#' Makes \code{CaliperClass} objects smarter about whether you are assigning a
#' value to the R object or the underlying GISDK object over COM.
#'
#' If \code{name} is an attribute of the R object (like \code{$info}), then
#' the value is assigned to that attribute. Otherwise, it looks into the GISDK
#' object attributes and will set that value.
#'
#' @param x A \code{CaliperClass} object
#' @param name the attribute to assign
#' @param value the value to be assigned
#' @export

`$<-.CaliperClass` <- function(x, name, value) {
  # If the name references an R attribute
  if (exists(name, envir = x)) {
    assign(name, value, envir = x)
  # If the name references a GISDK field
  } else if (name %in% x$info$FieldNames) {
    .subset2(x, "set_gisdk_attribute")(name, value)
  } else {
    stop(paste0("'", name, "' not found in R or GISDK objects"))
  }
  invisible(x)
}
