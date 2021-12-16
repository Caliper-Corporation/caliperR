# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

#' Converts a Caliper view into an R data.frame
#'
#' @param view_name The name of the view open in Caliper software.
#' @param set_name An optional set name can be provided to only return
#'   records in a selection set from the view.
#' @import data.table
#' @export

df_from_view <- function(view_name, set_name = NULL) {
  # Make sure view_name is an open view
  current_views <- RunFunction("GetViews")[[1]]
  if (!(view_name %in% current_views)){
    software <- get_package_variable("CALIPER_SOFTWARE")
    stop("View '", view_name, "' not open in ", software)
  }
  if (!is.null(set_name)) {
    current_sets <- unlist(RunFunction("GetSets", view_name))
    if (!(set_name %in% current_sets)){
      stop("Set '", set_name, "' not in view '", view_name, "'")
    }
  }

  viewset <- paste0(view_name, "|", set_name)
  csv <- tempfile(fileext = ".csv")
  RunFunction("ExportView", viewset, "CSV", csv, NA, list("CSV Header" = "true"))
  df <- data.table::fread(csv, na.strings = "")
  df <- as.data.frame(df)
  r_types <- vector(mode = "character", length = length(df))

  # Correct field types for any empty fields
  for (i in 1:length(df)) {
    field_name <- bracket_field(colnames(df)[[i]])
    field_spec <- paste0(view_name, ".", field_name)
    caliper_type <- RunFunction("GetFieldType", field_spec)
    r_types[[i]] <- TcTypeToRType2(caliper_type)
  }
  df <- correct_empty_columns(df, r_types)

  return(df)
}

#' Converts a data.frame into a view.
#'
#' Updates an existing view or creates a new one as needed.
#' @param df \code{data.frame} The data to update the view with.
#' @param view_name \code{string} The view to update.
#' @param set_name \code{string} Optional selection set name. If provided,
#'   only the rows within the selection set of \code{view_name} will be updated.
#' @export

df_to_view <- function(df, view_name = NULL, set_name = NULL) {

  if (!is.null(view_name)) update_view(df, view_name, set_name)
  else {
    view_name <- create_view(df)
    return(view_name)
  }
}

#' Updates an existing Caliper view with data from a data.frame
#'
#' @inheritParams df_to_view
#' @import data.table
#' @return \code{string} The view name.
#' @keywords internal

update_view <- function(df, view_name, set_name = NULL) {
  # Make sure view_name and set_name exist
  current_views <- unlist(RunFunction("GetViews"))
  if (!(view_name %in% current_views)) {
    software <- get_package_variable("CALIPER_SOFTWARE")
    stop("View '", view_name, "' not open in ", software)
  }
  if (!is.null(set_name)) {
    current_sets <- unlist(RunFunction("GetSets", view_name))
    if (!(set_name %in% current_sets)){
      stop("Set '", set_name, "' not in view '", view_name, "'")
    }
  }

  csv <- tempfile(fileext = ".csv")
  data.table::fwrite(df, csv)
  view <- RunFunction("OpenTable", "temp", "CSV", list(csv, NA))
  column_names <- as.list(colnames(df))
  viewset <- paste0(view, "|")
  data <- RunFunction("GetDataVectors", viewset, column_names, NA, process_result = FALSE)
  names(data) <- column_names
  viewset <- paste0(view_name, "|", set_name)
  RunFunction("SetDataVectors", viewset, data, NA)

  return(view_name)
}

#' Creates a new Caliper view with data from a data.frame
#'
#' The data view is a "MEM" (or memory table), and is not associated with
#' a file.
#'
#' @inheritParams df_to_view
#' @return \code{string} The name of the view.
#' @keywords internal

create_view <- function(df) {
  csv <- tempfile(fileext = ".csv")
  data.table::fwrite(df, csv)
  view <- RunFunction("OpenTable", "temp", "CSV", list(csv, NA))
  viewset <- paste0(view, "|")
  view_name <- create_unique_view_name()
  RunFunction("ExportView", viewset, "MEM", view_name, NA, NA)
  return(view_name)
}

#' Creates a view name unique in the current Caliper session
#' @return \code{string} A unique view name.
#' @keywords internal

create_unique_view_name <- function() {
  current_views <- RunFunction("GetViews")[[1]]
  for (i in 1:1000) {
    view_name <- paste0("r_view_", i)
    if (!(view_name %in% current_views)) return(view_name)
  }
}

#' Close all views open in Caliper software
#' @keywords internal

close_views <- function() {
  for (view in RunFunction("GetViews")[[1]]) {
    RunFunction("CloseView", view)
  }
}

#' Check for special characters and return 'clean' field name
#'
#' Caliper functions require square brackets [] around fields with
#' characters like spaces, periods, and others. This function checks
#' for those special characters and adds brackets if needed.
#' @keywords internal

bracket_field <- function(field_name) {

  # does the string contain a punctuation character, space, or begin
  # with a number?
  if (stringr::str_detect(string = field_name, pattern = "[[:punct:]]| |^\\d")) {
    field_name = paste0("[", field_name, "]")
  }
  return(field_name)
}
