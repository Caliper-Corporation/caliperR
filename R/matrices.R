# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

#' S3 method for converting a \code{CaliperMatrix} into a \code{data.frame}
#'
#' @param x \code{CaliperMatrix}
#' @param row.names See \code{as.data.frame}
#' @param optional See \code{as.data.frame}
#' @param ... additional arguments passed to \code{as.data.frame}
#' @return Returns a \code{data.frame} with \code{from} and \code{to} columns
#'   along with a column for each core of the matrix. The \code{data.frame}
#'   respects the current row and column indices.
#' @import data.table
#' @export

as.data.frame.CaliperMatrix <- function(x, row.names = NULL,
                                        optional = FALSE, ...) {
  temp_file <- tempfile(fileext = ".csv")
  core_names <- names(x$cores)
  RunFunction(
    "CreateTableFromMatrix", x$handle, temp_file, "CSV",
    list(Complete = TRUE)
  )
  df <- data.table::fread(temp_file, header = FALSE)
  colnames(df) <- c("from", "to", core_names)
  setDF(df, rownames = row.names)

  # CreateTableFromMatrix exports all the data regardless of which index
  # is active. Filter the from/to columns to match the active index.
  c_labels <- RunFunction("GetMatrixColumnLabels", x$cores[[1]]@com)
  c_labels <- as.numeric(c_labels)
  r_labels <- RunFunction("GetMatrixRowLabels", x$cores[[1]]@com)
  r_labels <- as.numeric(r_labels)
  df <- df[df$from %in% r_labels & df$to %in% c_labels, ]

  return(df)
}

#' S3 method to convert all cores in a CaliperMatrix to R matrices
#'
#' To convert individual cores of a \code{CaliperMatrix}, see
#' \code{\link{as.matrix.MatrixCurrency}}.
#'
#' @param x \code{CaliperMatrix}
#' @param ... other arguments passed to \code{as.matrix()}
#' @return A named list of R matrices.
#' @method as.matrix CaliperMatrix
#' @export

as.matrix.CaliperMatrix <- function(x, ...) {
  sapply(
    x$cores,
    function(x, ...) as.matrix(x, ...),
    USE.NAMES = TRUE, simplify = FALSE
  )
}

#' S3 method for summarizing a \code{CaliperMatrix}
#'
#' Statistics return include things like sum, mean, min, etc.
#'
#' @param object A \code{CaliperMatrix} object.
#' @param ... Additional arguments passed to the \code{summary} generic
#'   function.
#' @return A \code{data.frame} of summary statistics. These summary statistics
#'   are always for the full Caliper matrix and do not respect the current
#'   index.
#' @import data.table
#' @return A \code{data.frame} in with a row for each core.
#' @export

summary.CaliperMatrix <- function(object, ...) {
  stats <- RunFunction("MatrixStatistics", object$handle, NA)
  list_of_rows <- mapply(
    function(x, name) {
      df <- as.data.frame(x)
      df$Core <- name
      return(df)
    },
    x = stats,
    name = names(stats),
    SIMPLIFY = FALSE
  )

  df <- data.table::rbindlist(list_of_rows)
  setcolorder(df, "Core")
  return(as.data.frame(df))
}

#' CaliperMatrix class
#'
#' This class makes it easier to interact with Caliper matrices in R.
#'
#' See \code{vignette("caliper-matrices")} for more details on interacting with
#' matrices. This includes info on S3 methods for bringing them into R formats
#' like \code{data.frames} and \code{as.matrix}.
#'
#' @import R6
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @export

CaliperMatrix <- R6::R6Class(
  "CaliperMatrix",
  public = list(

    #' @field handle This is a COM pointer and represents the object in Caliper
    #'   software.
    handle = NULL,

    #' @field cores This is a named list of cores. Each element contains a
    #'   \code{\link{MatrixCurrency-class}}.
    cores = NULL,

    #' @field indices This lists the row and column indices available for the
    #'   matrix.
    indices = NULL,

    #' @description
    #' Create a new \code{CaliperMatrix} object.
    #' @param matrix Either the file path to the matrix (.mtx) or a COM pointer
    #'   to the matrix handle.
    #' @return a new \code{CaliperMatrix} object.
    initialize = function (matrix) {
      if (typeof(matrix) == "character") {
        stopifnot(file.exists(matrix))
        self$handle <- RunFunction("OpenMatrix", matrix, NA)
      }
      if (class(matrix) == "COMIDispatch") {
        self$handle <- matrix
      }
      base_indices <- RunFunction("GetMatrixBaseIndex", self$handle)
      private$current_row_index <- base_indices[[1]]
      private$current_column_index <- base_indices[[2]]
      self$create_index_info()
      self$create_core_list()
    },

    #' @description
    #' Sets the indices field of the \code{CaliperMatrix}.
    create_index_info = function () {
      indices <- RunFunction(
        "GetMatrixIndexNames", self$handle, process_result = FALSE
      )
      indices <- lapply(indices, function(x) unlist(x))
      indices <- setNames(indices, c("row", "column"))
      self$indices <- indices
    },

    #' @description
    #' Sets the \code{cores} field of the \code{CaliperMatrix}.
    create_core_list = function () {
      result <- RunFunction(
        "CreateMatrixCurrencies", self$handle, private$current_row_index,
        private$current_column_index, NA
      )
      result <- lapply(result, function(x) make_MatrixCurrency(x))
      self$cores <- result
      invisible(self)
    },

    #' @description
    #' Sends R data back to the matrix in Caliper software.
    #' @param core_name \code{string} The name of the core to update.
    #' @param data \code{matrix} of data.
    update_gisdk_data = function(core_name, data) {
      stopifnot(core_name %in% names(self$cores))
      stopifnot("matrix" %in% class(data))

      info <- RunFunction("GetMatrixInfo", self$handle)
      ri_pos <- which(self$indices$row == self$row_index)
      if (dim(data)[1] > info[[6]]$`RowIndex Sizes`[[ri_pos]]) {
        stop("Your data has more rows than the matrix currency")
      }
      ci_pos <- which(self$indices$column == self$column_index)
      if (dim(data)[2] > info[[6]]$`ColIndex Sizes`[[ci_pos]]) {
        stop("Your data has more columns than the matrix currency")
      }

      df <- as.data.frame(data) %>%
        dplyr::mutate(from = rownames(.)) %>%
        tidyr::pivot_longer(-from, names_to = "to", values_to = "value") %>%
        dplyr::mutate(core = core_name)
      temp_csv <- tempfile(fileext = ".bin")
      data.table::fwrite(df, temp_csv)
      view <- create_unique_view_name()
      view <- RunFunction("OpenTable", view, "CSV", list(temp_csv, NA))

      ok <- RunFunction(
        "VerifyIndex", self$handle, self$row_index, paste0(view, "|"),
        "from"
      )
      if (ok != "Yes") stop(
        "Your data has row ids that are not found in the matrix"
      )
      ok <- RunFunction(
        "VerifyIndex", self$handle, self$column_index, paste0(view, "|"),
        "to"
      )
      if (ok != "Yes") stop(
        "Your data has column ids that are not found in the matrix"
      )

      RunFunction(
        "UpdateMatrixFromView", self$handle, paste0(view, "|"),
        "from", "to", "core", list("value"), "Replace", NA
      )
      invisible(self)
    },

    #' @description
    #' Create a new core in the matrix.
    #' @param core_name \code{string} The name of the core to create.
    CreateCore = function(core_name) {
      RunFunction("AddMatrixCore", self$handle, core_name)
      self$create_core_list()
    },

    #' @description
    #' Create a new index.
    #' @param index_name \code{string} The name of the new index.
    #' @param current_ids \code{vector} of the row/column IDs to include in new
    #'   index.
    #' @param new_ids \code{vector} Use this to assign new IDs to the rows/columns
    #'   of this index if desired.
    #' @param index_type \code{string} Whether the index can be applied to rows,
    #'   column, or (the default) both.
    CreateIndex = function(index_name, current_ids, new_ids = NULL,
                           index_type = c("both", "row", "column")) {
      index_type <- match.arg(index_type)
      if (!is.null(new_ids)) {
        stopifnot(length(new_ids) == length(current_ids))
        df <- data.frame(current_ids = current_ids, new_ids = new_ids)
        new_id_col <- "new_ids"
      } else {
        df <- data.frame(current_ids = current_ids)
        new_id_col <- NA
      }
      old_id_col <- "current_ids"
      view <- df_to_view(df)
      viewset <- paste0(view, "|")
      RunFunction(
        "CreateMatrixIndex", index_name, self$handle, index_type, viewset,
        old_id_col, new_id_col
      )
      self$create_index_info()
    }
  ),
  active = list(

    #' @field row_index This can be used to retrieve or set the current row index.
    row_index = function (name) {
      if (missing(name)) return(private$current_row_index)
      if (!(name %in% self$indices$row)) {
        stop(paste0(
          "Name must be one of ", paste(self$indices$row, collapse = ", ")
        ))
      }
      self$cores <- NULL
      private$current_row_index <- name
      self$create_core_list()
    },

    #' @field column_index This can be used to retrieve or set the current column index.
    column_index = function (name) {
      if (missing(name)) return(private$current_column_index)
      if (!(name %in% self$indices$column)) {
        stop(paste0(
          "Name must be one of ", paste(self$indices$column, collapse = ", ")
        ))
      }
      self$cores <- NULL
      private$current_column_index <- name
      self$create_core_list()
    }
  ),
  private = list(
    current_row_index = NULL,
    current_column_index = NULL
  )
)

#' S3 method for calling \code{CaliperMatrix} object attributes
#'
#' Makes it easier to call a matrix core directly using \code{matrix$core_name}.
#'
#' @details
#'
#' If \code{name} is an attribute of the R object (like \code{$info}), then
#' the value of that attribute is returned. Otherwise, it looks into the fields
#' and methods of the underlying GISDK object to determine what to do.
#'
#' @param x A \code{CaliperMatrix} object
#' @param name the method to dispatch
#' @export

`$.CaliperMatrix` <- function(x, name) {
  core_names <- names(.subset2(x, "cores"))
  # If the name references an R method/attribute
  if (exists(name, envir = x)) {
    .subset2(x, name)
  # If the name is a core
  } else if (name %in% core_names) {
    .subset2(x, "cores")[[name]]
  } else {
    stop(paste0(name, " is not a valid attribute or core name."))
  }
}

#' Send matrix data back to Caliper software
#'
#' Standard assignment would simply overwrite the matrix currency with whatever
#' data you assigned. This allows for properly formed matrix data to be sent
#' back to Caliper software and update the matrix currency.
#'
#' @param x A \code{CaliperMatrix} object
#' @param name the attribute to assign
#' @param value the value to be assigned
#' @export

`$<-.CaliperMatrix` <- function(x, name, value) {
  # If the name references an R attribute
  if (exists(name, envir = x)) {
    assign(name, value, envir = x)
    # If the name references a core
  } else if (name %in% names(x$cores)) {
    .subset2(x, "update_gisdk_data")(name, value)
    .subset2(x, "create_core_list")
  } else {
    stop(paste0("'", name, "' is not an R attribute or core name"))
  }
  invisible(x)
}

#' The matrix currency class
#'
#' This is a simple S4 class that wraps a COMIDispatch pointer. That pointer
#' points to a matrix currency in Caliper software.
#'
#' @import methods
#' @keywords internal

setClass("MatrixCurrency", representation(com = "COMIDispatch"))

#' Creates a matrix_currency object
#'
#' This is used to wrap a COMIDispatch pointer (representing a GISDK matrix
#' currency).
#'
#' @param x \code{COMIDispatch} Pointer to a GISDK matrix currency.
#' @return A \code{MatrixCurrency} object.
#' @keywords internal

make_MatrixCurrency <- function(x) {
  gisdk_type <- RunMacro("get_object_type", x)
  if (gisdk_type != "matcurrency") stop("x is not a matrix currency")
  mc <- new("MatrixCurrency", com = x)
  return(mc)
}

#' Generic to convert MatrixCurrency class to an R matrix
#'
#' Converts a Caliper matrix into R format allowing it to be worked on in the R
#' environment.
#'
#' @param x \code{MatrixCurrency} One of the cores of the \code{CaliperMatrix}
#'   class.
#' @param ... Other arguments passed to \code{as.matrix}
#' @import data.table
#' @return An R matrix.
#' @method as.matrix MatrixCurrency
#' @export

as.matrix.MatrixCurrency <- function(x, ...) {
  com_ptr <- x@com
  ri <- RunFunction("GetMatrixVector", com_ptr, list("Index" = "Row"))
  ri <- as.list(as.character(ri))
  ci <- RunFunction("GetMatrixVector", com_ptr, list("Index" = "Column"))
  ci <- as.list(as.character(ci))
  temp_csv <- tempfile(fileext = ".csv")
  RunFunction("ExportMatrix", com_ptr, ci, "Rows", "CSV", temp_csv, NA)
  tbl <- data.table::fread(temp_csv)
  mtx <- as.matrix(tbl, rownames = "V1")
  colnames(mtx) <- ci
  return(mtx)
}
setMethod("as.matrix", "MatrixCurrency", as.matrix.MatrixCurrency)
