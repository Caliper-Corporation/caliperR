# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

#' caliperR: Communicate with Caliper software from R
#'
#' The caliperR package provides functions to make it easier to communicate
#' with Caliper software over COM.
#'
#' @docType package
#' @name caliperR
NULL

#'Create a connection to a Caliper software product
#'
#'Will create a connection over COM to one of Caliper's installed software
#'products. (The software must be installed with a valid license.)
#'
#' @param software One of either "TransCAD", "TransModeler", or "Maptitude". If
#'  left \code{NULL}, the function will search (in that order) and create the first
#'  connection it can.
#' @param silent \code{boolean} Whether to display a connected message.
#' @import RDCOMClient
#' @importFrom utils unzip
#' @return Nothing. Sets the COM object to a global environment variable
#'   (\code{CALIPER_DK})
#' @export

connect <- function(software = NULL, silent = FALSE){

  # Argument checking
  valid_software_values <- c("TransCAD", "TransModeler", "Maptitude")
  if (!is.null(software)){
    if (any(tolower(software) == tolower(valid_software_values))) {
      pos <- which(tolower(software) == tolower(valid_software_values))
      software <- valid_software_values[pos]

    } else {
      stop(
        paste0(
          "(caliperR::connect) Invalid value for 'software'. Valid values are: ",
          paste(valid_software_values, collapse = ", ")
        )
      )
    }
  }
  if (!is.logical(silent)) {
    stop("(caliperR::connect) 'silent' must be logical (true/false)")
  }

  # To prevent orphan processes, disconnect previous connections if the user
  # connects to a different software. e.g. was connected to TransCAD and now
  # wants to connect to Maptitude.
  if (connected() && !is.null(software)) {
    current_software <- get_package_variable("CALIPER_SOFTWARE")
    if (software != current_software) disconnect()
  }

  if (is.null(software)) {
    software_to_try <- valid_software_values
  } else {
    software_to_try <- software
  }
  for (software in software_to_try) {
    suppressWarnings(
      try(
        {
          dk <-  RDCOMClient::COMCreate(paste0(software, ".AutomationServer"))
          set_package_variable("CALIPER_DK", dk)
          set_package_variable("CALIPER_SOFTWARE", software)
          set_package_variable("CALIPER_UI", "gis_ui")
        },
        silent = TRUE
      )
    )
    if (exists("dk")) break
  }
  if (!exists("dk")) stop(
    "Could not connect to Caliper software. Check that it is installed."
  )

  # Set a package variable that points to gisdk_utils, a gisdk UI with helper
  # functions for this package
  zip_file <- system.file("extdata", "gisdk", "gisdk_utils",
                          "gisdk_utils.zip", package = "caliperR")
  tempdir <- tempdir()
  unzip(zip_file, exdir = tempdir, setTimes = TRUE)
  ui_path <- file.path(tempdir, "gisdk_utils.dbd")
  set_package_variable("GISDK_UTILS_UI", ui_path)

  # Initialize the client and clear the log/report files. If the software is
  # already open locally, it will connect to it and not produce a log file.
  info <- RunMacro("init_client")
  set_package_variable("CALIPER_INFO", info)
  try({
    close(file(info$LogFile, open="w"))
    repot_file <- gsub("Errors\\.log", "Report\\.xml", info$LogFile)
    close(file(repot_file, open="w"))
  }, silent = TRUE)

  if (!silent) {
    SetAlternateInterface(get_package_variable("GISDK_UTILS_UI"))
    p_info <- RunMacro("GetProgram")
    path <- p_info[[1]]
    software <- paste(p_info[[2]], p_info[[5]], "build", p_info[[4]],
                      paste0("(", p_info[[3]], ")"))
    message("Connected to ", software, "\n(", path, ")")
  }

  return(GisdkClass$new())
}

#' Close the COM connection to Caliper software and kills the process
#'
#' @return Nothing.
#' @export

disconnect <- function() {
  if (connected()) {
    try({
      SetAlternateInterface(get_package_variable("GISDK_UTILS_UI"))
      RunMacro("Exit")
    }, silent = TRUE
    )
    try({
      remove("CALIPER_DK", envir = caliper_env)
      remove("CALIPER_SOFTWARE", envir = caliper_env)
      remove("CALIPER_UI", envir = caliper_env)
      remove("GISDK_UTILS_UI", envir = caliper_env)
    }, silent = TRUE)
  }
}

#' Checks if R is connected to Caliper software
#'
#' @export

connected <- function() {
  check <- tryCatch(
    error = function(cnd) FALSE,
    get_package_variable("CALIPER_DK")
  )
  if (class(check) == "COMIDispatch") return(TRUE)
  return(FALSE)
}

#' Shows the Caliper log file
#'
#' @importFrom tidyr unite
#' @return Returns the log as a data frame.
#' @importFrom utils read.table
#' @export

read_log <- function() {
  log_file <- get_package_variable("CALIPER_INFO")$LogFile
  try({
    tbl <- read.table(log_file, sep = " ") %>%
      tidyr::unite(col = "log entries", sep = " ")
  }, silent = TRUE)
  if (exists("tbl")) {
    return(tbl)
  } else return(NULL)
}

#' Compiles a GISDK script file into a UI
#'
#' @param rsc_file \code{string} Script file path.
#' @param ui_file \code{string} Optional output location of the compiled code.
#'   By default, it will use the same file name as the \code{rsc_file}, but with
#'   a .dbd extension.
#' @import stringr
#' @return The file path of the compiled '.dbd' file. Like other Caliper databases,
#'   the compiled UI is made up of several related files.
#' @export

compile_gisdk <- function(rsc_file, ui_file = NULL) {
  if (stringr::str_sub(rsc_file, -4, -1) != ".rsc")
    stop("'rsc_file' extension must be '.rsc'")
  if (is.null(ui_file)) {
    ui_file <- gsub(".rsc", ".dbd", rsc_file, fixed = TRUE)
  } else if (stringr::str_sub(ui_file, -4, -1) != ".dbd")
    stop("'ui_file' extension must be '.dbd'")

  RunMacro("compile_file", rsc_file, ui_file)
}

#' Runs a GISDK macro
#'
#' A GISDK macro is a function defined in a GISDK scrip (.rsc). When working in
#' Caliper software, these are always called in GISDK using the
#' \code{RunMacro()} function.
#'
#' To run GISDK functions (like \code{OpenTable()}) see \code{\link{RunFunction}}.
#'
#' @param macro_name \code{string} Name of the GISDK macro to run
#' @param process_result \code{boolean} Whether to attempt to process the
#'   result into a native R format.
#' @param ... Used to pass arguments to the GISDK macro
#' @export
#' @examples
#' \dontrun{
#' # These won't work unless Caliper software is installed.
#' RunMacro("G30 Tutorial Folder")
#' RunMacro("add", 1, 2)
#' #> 3
#' RunMacro("parse opts array", list("one" = 1, "two" = 2))
#' #> "The first option name is one. The first option value is 1."
#' }

RunMacro <- function(macro_name, ..., process_result = TRUE) {
  stopifnot(connected())
  dk <- get_package_variable("CALIPER_DK")
  dk_ui <- get_package_variable("CALIPER_UI")
  gisdk_args <- process_gisdk_args(...)
  args <- c(list(macro_name, dk_ui), gisdk_args)
  result <- do.call(dk$RunUIMacro, args)
  if (process_result) result <- process_gisdk_result(result)
  return(result)
}

#' Runs a GISDK function
#'
#' GISDK functions are core functions (like \code{OpenTable()}) that are called
#' in Caliper software without using \code{RunMacro()}.
#'
#' To run GISDK macros (like \code{RunMacro("G30 Tutorial Folder")}) see
#' \code{\link{RunMacro}}.
#'
#' @param macro_name \code{string} Name of the GISDK function to run
#' @param process_result \code{boolean} Whether to attempt to process the
#'   result into a native R format.
#' @param ... Used to pass arguments to the GISDK function
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # These won't work unless Caliper software is installed.
#' table_name <- RunFunction("OpenTable", "airports", "ffb", list(paste0(folder, "airports.bin"), NA))
#' num_rows <- RunFunction("GetRecordCount", table_name, NA)
#' num_rows
#' #> 280
#' }

RunFunction <- function(macro_name, ..., process_result = TRUE) {
  stopifnot(connected())
  invalid_macro_names <- c(
    "RunMacro",
    "CreateObject",
    "SetAlternateInterface",
    "GetInterface"
  )
  if (macro_name %in% invalid_macro_names) {
    stop(paste0(
      "Use caliperR::", macro_name, "()"
    ))
  }
  dk <- get_package_variable("CALIPER_DK")
  gisdk_args <- process_gisdk_args(...)
  args <- c(list(macro_name), gisdk_args)
  result <- do.call(dk$RunMacro, args)
  if (process_result) result <- process_gisdk_result(result)
  return(result)
}

#' Change the Caliper UI
#'
#' Often, a user will have created their own GISDK functions and compiled them
#' to a UI file (.dbd). To run them (using \code{\link{RunMacro}}), first use
#' this function to point to the custom UI. This can also be used to set the
#' UI back to the default.
#'
#' To see the current UI, use \code{\link{GetInterface}}.
#'
#' @param ui_file \code{string} File path to the custom UI. If null, will set
#'   the interface back to the default.
#' @export

SetAlternateInterface <- function(ui_file = NULL) {
  if (is.null(ui_file) || ui_file == "default" || ui_file == "gis_ui") {
    ui_file = "gis_ui"
  } else {
    ui_file <- gsub("/", "\\", ui_file, fixed = TRUE)
    if (!file.exists(ui_file)){
      stop("(caliperR::SetAlternateInterface) 'ui_file' not found")
    }
  }
  set_package_variable("CALIPER_UI", ui_file)
}

#' Retrieves the current GISDK interface
#'
#' To set the current UI, use \code{\link{SetAlternateInterface}}.
#'
#' @export

GetInterface <- function() {
  ui_file <- get_package_variable("CALIPER_UI")
  if (ui_file == "gis_ui") {
    return("default")
  } else {
    ui_file <- gsub("\\", "/", ui_file, fixed = TRUE)
    return(ui_file)
  }
}

#' Convert R arguments into GISDK flavors
#'
#' It calls \code{\link{convert_to_named_array}} and
#' \code{\link{convert_nulls_and_slashes}} as appropriate on each argument passed.
#'
#' @param arg_list \code{list} of args that are converted.
#' @keywords internal

process_gisdk_args <- function(...) {
  arg_list <- list(...)
  if (length(arg_list) == 0) return(NA_complex_)

  for (i in 1:length(arg_list)) {
    arg <- arg_list[[i]]
    if (is.object(arg)) {
      if (any(class(arg) == "CaliperClass")) {
        arg_list[[i]] <- arg$ref
        next
      } else if (any(class(arg) == "MatrixCurrency")) {
        arg_list[[i]] <- arg@com
        next
      } else if (any(class(arg) == "CaliperMatrix")) {
        arg_list[[i]] <- arg$handle
        next
      } else if (any(class(arg) == "data.frame")) {
        arg <- as.list(arg)
      } else next
    }
    if (length(arg) == 0) {
      arg <- NA_complex_
      } else {
      arg <- convert_to_named_array(arg)
      arg <- convert_nulls_and_slashes(arg)
    }
    arg_list[[i]] <- arg
  }
  return(arg_list)
}


#' Used internally to convert R's named vectors/lists to GISDK named arrays.
#'
#' @param named_list A named list (or vector)
#' @import RDCOMClient
#' @return a pointer object that GISDK will interpret as a named array
#' @keywords internal

convert_to_named_array <- function(named_list) {
  # Argument checking
  if (is.null(names(named_list))) return(named_list)

  n <- names(named_list)
  l <- unname(named_list)
  nest <- function(n, l) {
    l_is_named <- !is.null(names(l))
    l <- convert_to_named_array(l)
    if (l_is_named) {
      as.list(c(n, l))
    } else {
      as.list(c(n, list(l)))
    }
  }
  result <- unname(mapply(nest, n, l, SIMPLIFY = FALSE))
  return(result)
}

#' Used to convert GISDK named arrays to R's named lists.
#'
#' When a GISDK named array comes across COM, it has a specific format that
#' isn't easy to use. This converts it into an R named list.
#'
#' @param nested_list The list object that results whenever GISDKs named arrays
#'   are passed through COM.
#' @return A named list
#' @keywords internal

convert_to_named_list <- function(nested_list) {
  names <- unlist(lapply(nested_list, "[", 1))
  values <- unlist(lapply(nested_list, "[", 2), recursive = FALSE)
  names(values) <- names
  return(values)
}

#' Checks if an object is a GISDK named array
#'
#' When returned over COM, these arrays have a very specific format. This
#' function is used in conjunction with \code{\link{convert_to_named_list}} in
#' \code{\link{process_gisdk_result}} to identify and convert GISDK named arrays
#' into R's named lists.
#'
#' @param object The object to be tested
#' @return \code{boolean} TRUE/FALSE
#' @keywords internal

is_gisdk_named_array <- function(object) {
  if (typeof(object) != "list") return(FALSE)
  v1 <- lapply(object, typeof) == "list"
  if (!all(v1)) return(FALSE)
  v2 <- lapply(object, length) == 2
  if (!all(v2)) return(FALSE)
  v3 <- lapply(object, function(x) typeof(x[[1]])) == "character"
  if (!all(v3)) return(FALSE)
  return(TRUE)
}

#' Converts R's \code{NA}, \code{NULL}, and \code{/} to formats GISDK can use
#'
#' \code{NA} and \code{NULL} are converted to \code{NA_complex_} and is
#' understood by GISDK/C++ as \code{null}. \code{/} is converted to \code{\\}.
#' @return Returns the argument passed in with any \code{NA/NULL} converted.
#' @keywords internal

convert_nulls_and_slashes <- function(arg) {
  if (is.object(arg)) return(arg)
  if (length(arg) > 1) {
    if (typeof(arg) == "list"){
      for (i in 1:length(arg)) {
        arg[[i]] <- convert_nulls_and_slashes(arg[[i]])
      }
    # If it is a vector
    } else {
      arg <- as.list(arg)
      if (is.character(arg)) {
        arg[is.na(arg)] <- ""
        return(arg)
      } else arg[is.na(arg)] <- NA_complex_
    }
  } else {
    if (is.null(arg) | is.na(arg)) {
      arg <- NA_complex_
      return(arg)
    }
    if (is_file_path(arg)) {
      arg <- gsub("/", "\\", arg, fixed = TRUE)
      return(arg)
    }
  }

  return(arg)
}

#' Converts GISDK output into R structures
#'
#' Simple objects pass through the COM interface as usable R structures.
#' GISDK arrays, for example, become R vectors. Other things, like GISDK
#' vectors, come across as pointers to the object inside the Caliper
#' process. This function attemps to coerce those pointers into usable
#' R data structures.
#'
#' @param result A returned value from Caliper software.
#' @keywords internal

process_gisdk_result <- function(result) {
  if (is_gisdk_named_array(result)) {
    result <- convert_to_named_list(result)
  }

  if (typeof(result) == "list") {
    result <- lapply(result, process_gisdk_result)
  } else {
    if (class(result) != "COMIDispatch") return(result)
    type <- RunMacro("get_object_type", result)
    if (type == "vector") {
      result <- RunFunction("V2A", result)
      result[sapply(result, is.null)] <- NA
      result <- unlist(result)
    }
    if (type == "matrix") result <- CaliperMatrix$new(result)
  }

  return(result)
}


#' Sets the value of a package-wide variable
#'
#' \code{caliperR} uses several package-wide variables to enable communication
#' between functions and simplify function arguments.
#'
#' @param package_variable The package variable to set
#' @param value the value to set with
#' @keywords internal

set_package_variable <- function(package_variable, value) {

  package_variables <- c(
    "CALIPER_DK", "CALIPER_SOFTWARE", "CALIPER_UI", "GISDK_UTILS_UI",
    "CALIPER_INFO"
  )
  if (!(package_variable %in% package_variables)) {
    stop(paste(
      "'package_variable' must be one of",
      paste(package_variables, collapse = ", ")
    ))
  }

  if (package_variable == "CALIPER_DK" & class(value) != "COMIDispatch") {
    stop("CALIPER_DK must be class COMIDispatch")
  }

  software_options <- c("TransCAD", "TransModeler", "Maptitude")
  if (package_variable == "CALIPER_SOFTWARE" && !(value %in% software_options)) {
    stop(paste(
      "CALIPER_SOFTWARE must be one of",
      paste(software_options, collapse = ", ")
    ))
  }

  if (package_variable == "CALIPER_UI") {
    if (typeof(value) != "character") stop("'value' must be a file path")
    if (!file.exists(value) & value != "gis_ui") {
      stop("CALIPER_UI file does not exist")
    }
  }

  if (package_variable == "GISDK_UTILS_UI") {
    if (typeof(value) != "character") stop("'value' must be a file path")
    if (!file.exists(value)) {
      stop("GISDK_UTILS_UI file does not exist")
    }
  }

  assign(package_variable, value, envir = caliper_env)
}

#' Gets the value of a \code{caliperR} package-wide variable
#'
#' @inheritParams set_package_variable
#' @keywords internal

get_package_variable <- function(package_variable) {
  return(get(package_variable, envir = caliper_env))
}

#' Checks to see if a string looks like a file path
#' @keywords internal

is_file_path <- function(s) {
  if (!is.character(s)) return(FALSE)
  if (grepl(":\\\\", s)) return(TRUE)
  if (grepl(":/", s)) return(TRUE)
  return(FALSE)
}
