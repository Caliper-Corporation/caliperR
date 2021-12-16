# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

#' Configures the environment for TransCAD
#'
#' Sets the error to dump.frames to prevent execution halt
#' Turns off command echo
#'
#' @importFrom utils dump.frames
#' @keywords internal

setupTcCommandPrompt<- function() {
  options(error=dump.frames)
  # options(echo=FALSE)
}

#' Checks if filename is a valid .bin file
#'
#' Stops the program if the filename is not a valid .bin extension
#'
#' @param binFilename A data table with columns to convert
#' @keywords internal

checkIfValidBinFile <- function(binFilename) {
    binFilenameLength <- nchar(binFilename)
    binFilenameExtension <- substr(binFilename,binFilenameLength-3,binFilenameLength)
    if (tolower(binFilenameExtension) != ".bin") {
        stop("Filename does not have the .bin extension")
    }
}


#' Coerces data.frame columns to the specified classes
#'
#' Coerces data.frame columns to the specified classes
#'
#' @param dataTable A data table with columns to convert
#' @param colClasses A vector specifying the classes of each column
#' @keywords internal

convertColClasses <- function(dataTable, colClasses) {
    colClasses <- rep(colClasses, len=length(dataTable))
    dataTable[] <- lapply(seq_along(dataTable), function(i) switch(colClasses[i],
        Date=as.Date(dataTable[[i]], origin='1970-01-01'),
        POSIXct=as.POSIXct(dataTable[[i]], origin='1970-01-01'),
        as(dataTable[[i]], colClasses[i]) ))
}

#' Convert TransCAD type string to R datatype strings
#'
#' Convert TransCAD type string to R datatype strings
#'
#' @param typeChar String denoting the TC type
#' @return The correponding R type
#' @keywords internal

TcTypeToRType <- function(typeChar) {
    switch(as.character(typeChar),
    "C" = "character",
    "I" = "integer",
    "S" = "integer",
    "R" = "numeric",
    "F" = "numeric",
    "Date" = "Date",
    "Time" = "POSIXct",
    "DateTime" = "POSIXct",
    typeChar)
}

#' Convert TransCAD type string to R datatype strings
#'
#' Unlike \code{\link{TcTypeToRType}}, this function works on the Caliper types
#' returned by GISDKs \code{GetFieldType}, which are words like "Integer" instead
#' of characters like "I".
#'
#' @inheritParams TcTypeToRType
#' @return The correponding R type
#' @keywords internal

TcTypeToRType2 <- function(typeChar) {
  switch(as.character(typeChar),
         "String" = "character",
         "Integer" = "integer",
         "Real" = "numeric",
         "DateTime" = "POSIXct",
         typeChar)
}

#' Convert R datatype strings to TransCAD type string
#'
#' Convert R datatype strings to TransCAD type string
#'
#' @param typeChar String denoting the R type
#' @return The correponding TC type
#' @keywords internal

RTypeToTcType <- function(typeChar) {
    switch(as.character(typeChar),
    "character" = "C",
    "integer" = "I",
    "numeric" = "R",
    "Date" = "Date",
    "POSIXct" = "DateTime",
    typeChar)
}

#' Convert TransCAD missing values to R NAs
#'
#' Convert TransCAD missing values to R NAs
#'
#' @param value Value of the data table entry to convert
#' @param typeChar String denoting the TC type
#' @return Either the value passed in or an NA
#' @keywords internal

TcMissToRNa <- function(value, typeChar) {
    switch(as.character(typeChar),
    "I" = {
        if (value == -2147483647)
            value <- NA_integer_
        value},
    "S" = {
        if (value == -32767)
            value <- NA_character_
        value},
    "R" = {
        if (value == -1.7976931348623157e+308)
            value <- NA_real_
        value},
    "F" = {
        if (value == -3.402823466e+38)
            value <- NA_real_
        value},
    value)
}

#' Convert R NAs to TransCAD missing values
#'
#' Convert R NAs to TransCAD missing values
#'
#' @param value Value of the data table entry to convert
#' @param typeChar String denoting the TC type
#' @return Either the value passed in or a TC missing integer/real number
#' @keywords internal

RNaToTcMiss <- function(value, typeChar) {
    switch(as.character(typeChar),
    "I" = {
        if (is.na(value))
            value <- as.integer(-2147483647)
        value},
    "R" = {
        if (is.na(value))
            value <- -1.7976931348623157e+308
        value},
    "C" = {
      if (is.na(value))
        value <- ""
      value},
    value)
}

#' Return the byte length for each R data type
#'
#' Return the byte length for each R data type
#'
#' @param binDataCol The data table column for which data type to return
#' @return byte length for each R data type
#' @keywords internal

getByteLength <- function(binDataCol) {
    dataType <- class(binDataCol)
    # If the data frame has column labels/descriptions, remove that class
    dataType <- dataType[dataType != "labelled"]
    switch(as.character(dataType),
    "character" = max(
      4,
      max(sapply(binDataCol, nchar, type="bytes", keepNA = FALSE))
    ),
    "integer" = 4,
    "numeric" = 8,
    "Date" = 4,
    "POSIXct" = 8)
}

#' Return the display length for each R data type
#'
#' Return the display length for each R data type
#'
#' @param binDataCol The data table column for which data type to return
#' @return A default character length for each field type
#' @keywords internal

getDisplayLength <- function(binDataCol) {
    dataType <- class(binDataCol)
    # If the data frame has column labels/descriptions, remove that class
    dataType <- dataType[dataType != "labelled"]
    switch(as.character(dataType),
    "character" = max(sapply(binDataCol, nchar, type="chars")),
    "integer" = 10,
    "numeric" = 10,
    "Date" = 12,
    "POSIXct" = 22)
}

#' Correct the type for empty columns
#'
#' Empty columns read in through various means end up with type = logical.
#' This function uses the dcbKey to correct them.
#'
#' @param df \code{data.frame} The data.frame to correct.
#' @param r_types \code{list/vector} of data type strings.
#' @keywords internal

correct_empty_columns <- function(df, r_types) {
  for (i in 1:ncol(df)) {
    r_type <- r_types[[i]]
    if (all(is.na(df[[i]]))) {
      df[[i]] <- switch(
        r_type,
        "character" = as.character(df[[i]]),
        "integer" = as.integer(df[[i]]),
        "numeric" = as.numeric(df[[i]]),
        "Date" = as.Date(df[[i]]),
        "POSIXct" = as.POSIXct(df[[i]])
      )
    }
  }
  df
}

#' Read a bin file
#'
#' Read the data in binfilename and return a data table. Any field descriptions
#' will become column labels (use \code{View()} or \code{Hmisc::label()} to see
#' them).
#'
#' @param binFilename \code{string} The bin filename to read
#' @param returnDnames \code{bool} To return display names (if present in bin)
#' @return The data table read from the bin located at binFilename
#' @export
#' @import data.table Hmisc
#' @importFrom utils read.csv
#' @importFrom stats setNames

read_bin <- function(binFilename, returnDnames = FALSE) {
    checkIfValidBinFile(binFilename)

    # If connected to Caliper software over COM, use it to read faster
    if (connected()) {
      view <- RunFunction("OpenTable", "temp", "FFB", list(binFilename, NA))
      df <- df_from_view(view)
      RunFunction("CloseView", view)
    }

    # Find the corresponding .dcb file
    dcbFilename <- paste(substr(binFilename,1,nchar(binFilename)-3),"dcb", sep="")

    # Sometimes has words (i.e. "binary") after bytes per row
    nBytesPerRow <- as.numeric(read.csv(text=readLines(dcbFilename)[2], header=FALSE, sep=" ")[1])
    dcbFile <- fread(dcbFilename, skip=2, header = FALSE)
    dcbNames <- dcbFile[[1]]
    TcDataType <- dcbFile[[2]]
    fieldDescrs <- dcbFile[[10]]

    if (length(dcbFile) == 13){
      if (returnDnames) {
        display_names <- setNames(dcbFile[[13]], dcbNames)
        return(display_names)
      } else {
        # If not returning dnames, remove them if they exist
        dcbFile[, 13] <- NULL
      }
    } else {
      if (returnDnames) return(NA)
    }

    binFileSize <-file.info(binFilename)$size
    nRows <- ceiling(binFileSize / nBytesPerRow)
    nCols <- length(dcbNames)

    # Create a row key and order it by starting byte
    dcbKey <- data.table(
                index = c(1:nCols),
                field_name = dcbFile[[1]],
                startByte = dcbFile[[3]],
                dataType = sapply(dcbFile[[2]], TcTypeToRType),
                byteLength = dcbFile[[4]],
                descr = dcbFile[[10]]
              )
    setkey(dcbKey, "startByte")

    # If not connected to Caliper software over COM, read the bin file in line
    # by line.
    if (!connected()) {
      df <- read_bin_without_com(binFilename, dcbKey, nBytesPerRow)
    }

    # Steps to take whether data was read with or without COM
    df <- correct_empty_columns(df, dcbKey$dataType)
    descriptions <- setNames(fieldDescrs, dcbNames)
    Hmisc::label(df) <- as.list(descriptions)

    return(df)
}

#' Read the bin matrix into a data table without a COM connection
#'
#' Helper function to \code{\link{read_bin}}. If Caliper software is not
#' installed, this function reads and translates the bytes directly.
#'
#' @param binFilename \code{string} Path of bin file to read.
#' @param dcbKey \code{data.frame} The FFB table structure df. Created by
#'   read_bin.
#' @param nBytesPerRow \code{numeric} The number of bytes per row.
#' @keywords internal

read_bin_without_com <- function(binFilename, dcbKey, nBytesPerRow) {

  # Read in the raw bytes
  binFileSize <-file.info(binFilename)$size
  nRows <- binFileSize / nBytesPerRow
  binFile <- file(binFilename, "rb")
  on.exit(close(binFile))
  rawBinData <- readBin(binFile, what="raw", n=binFileSize)

  # Remove any rows marked as deleted
  del_pattern <- charToRaw('\x91\x8b\x4a\x5c\xbc\xdb\x4f\x14\x63\x23\x7f\x78\xa6\x95\x0d\x27')
  del_pattern <- del_pattern[1:min(nBytesPerRow, 16)]
  p <- which(rawBinData %in% del_pattern)
  p1 <- diff(p)
  p2 <- data.table::frollsum(p1, length(del_pattern) - 1)
  p3 <- which(p2 == length(del_pattern) - 1)
  contains_deleted_records <- length(p3) > 0
  deleted_rows <- 0
  while (contains_deleted_records) {
    start_pos <- p[p3[1]-length(del_pattern) + 2]
    end_pos <- start_pos + nBytesPerRow - 1
    rawBinData <- rawBinData[-c(start_pos:end_pos)]
    deleted_rows <- deleted_rows + 1
    p <- which(rawBinData %in% del_pattern)
    p1 <- diff(p)
    p2 <- data.table::frollsum(p1, length(del_pattern) - 1)
    p3 <- which(p2 == 15)
    contains_deleted_records <- length(p3) > 0
  }
  nRows <- nRows - deleted_rows

  binMatrix <- matrix(rawBinData, nBytesPerRow, nRows)

  # create start/stop list from a single vector of byte lenghts
  end <- cumsum(dcbKey$byteLength)
  start <- c(1, end + 1)
  start <- start[-length(start)]
  row_list <- mapply(
    function(s, e) {seq(s, e)},
    s = start, e = end,
    SIMPLIFY = FALSE
  )
  m <- split_matrix(binMatrix, row_list, list(1:nRows))

  l <- mapply(
    function(m, r_type, nRows) {
      tc_type <- RTypeToTcType(r_type)

      if (r_type != "character") {
        converted <- readBin(m, r_type, nRows)
        converted <- sapply(converted, TcMissToRNa, tc_type)
      } else {
        converted <- apply(m, 2, function(x) readChar(x, length(x), useBytes=TRUE))
        converted <- unname(sapply(converted, trimws))
        converted <- ifelse(converted == "", NA_character_, converted)
      }

    },
    m = m,
    r_type = dcbKey$dataType,
    nRows = nRows,
    SIMPLIFY = FALSE
  )

  names(l) <- dcbKey$field_name
  df <- as.data.frame(l, stringsAsFactors = FALSE)

  return(df)
}

#' Splits a matrix by row and column indices
#'
#' Helper matrix to \code{\link{read_bin_without_com}}.
#'
#' @param M \code{matrix} A matrix full of raw bytes.
#' @param list_of_rows \code{list} The indices of rows to split
#'   (e.g. \code{list(1:4, 5:8)}).
#' @param list_of_cols \code{list} The indices of columns to split.
#'   (e.g. \code{list(0:3)}).
#' @keywords internal

split_matrix <- function(M, list_of_rows, list_of_cols){
  temp = expand.grid(list_of_rows, list_of_cols)
  lapply(seq(nrow(temp)), function(i) {
    M[unlist(temp[i, 1]), unlist(temp[i, 2]) ]
  })
}

#' Write data table's dcb file
#'
#' To write the data table into a TransCAD binary file (.bin),
#' we must first write a key in a .dcb file
#'
#' @param dcbKey The dcb data table to write in dcbFilename
#' @inheritParams write_bin
#' @import data.table
#' @keywords internal

writeDcbFile <- function(
  dcbKey, dcbFilename, description = "", dnames = NA) {
    dcbFile <- file(dcbFilename, "wb")
    on.exit(close(dcbFile))
    rowSize <- sum(unlist(dcbKey$byteLength))
    writeLines(c(description,as.character(rowSize)), dcbFile)

    nRows <- nrow(dcbKey)
    bytePosition <- 1
    for (i in 1:nRows) {
        decimalLength <- 0
        if (dcbKey[i, "dataType"] == "R"){
            decimalLength <- 4
        }

        line <- paste(
          paste('"',dcbKey[i, "colNames"],'"', sep=""),
          dcbKey[i, "dataType"],
          bytePosition,
          dcbKey[[i, "byteLength"]],
          0,
          dcbKey[[i, "displayLength"]],
          decimalLength,
          ',""',
          paste0("\"", dcbKey[i, "fieldDescrs"], "\""),
          ',"Copy",',
          sep = ","
        )

        # Add display names if they exist
        if(!(paste(dnames, collapse = "") == "NA")){
          colName <- dcbKey[[i, "colNames"]]
          display_name <- as.character(dnames[colName])
          if(!is.na(display_name)){
            if (display_name != "") {
              line <- paste0(line, "\"", display_name, "\"")
            }
          }
        }

        writeLines(line, dcbFile)
        bytePosition <- bytePosition + dcbKey[[i, "byteLength"]]
    }
}

#' Write data table into bin file
#'
#' Write the data table into a TransCAD binary file (.bin)
#' given the filename and description. If the table has
#' column labels, those will become field descriptions.
#'
#' @param binData \code{data.frame} The data table to write
#' @param binFilename \code{string} The file in which to write
#' @param description \code{string} A description of the binData
#' @param dnames \code{Named vector} of display names where each name is a
#'   column name and each value is the display name to use. For example:
#'   \code{c("hh" = "Households")} will assign the display name "Households" to
#'   column "hh".
#' @param n2i \code{bool} Whether to try to convert numerics to integers.
#'   This leads to integer columns in the bin (instead of real), which
#'   are smaller and easier to read. Defaults to \code{TRUE}. Conversion
#'   does not happen if the column contains decimal values.
#' @export
#' @import data.table stringr
#' @import Hmisc

write_bin <- function(
  binData, binFilename, description='', dnames = NA, n2i = TRUE) {
  checkIfValidBinFile(binFilename)

  # If connected to Caliper software over COM, use it for faster file creation.
  # The remaining code will overwrite the dcb file to respect the display names,
  # descriptions, etc.
  if (connected()) {
    write_bin_using_com(binData, binFilename)
    return()
  }

  # Remove the 'labelled' class from columns if it exists
  for (i in 1:length(binData)) {
    class(binData[[i]]) <- setdiff(class(binData[[i]]), 'labelled')
  }

  # Create the dcbKey and write the corresponding .dcb file
  dcbFilename <- paste(substr(binFilename,1,nchar(binFilename)-3),"dcb", sep="")
  dataType <- sapply(sapply(binData, class), RTypeToTcType)
  dataType <- dataType[dataType != "labelled"]
  dcbKey <- data.table::data.table(
    colNames = colnames(binData),
    dataType = dataType,
    byteLength = sapply(binData, getByteLength),
    displayLength = sapply(binData, getDisplayLength),
    fieldDescrs = Hmisc::label(binData)
  )
  writeDcbFile(dcbKey, dcbFilename, description, dnames)

  write_bin_without_com(binData, binFilename, dcbKey)
}

#' Uses Caliper software over COM to write a bin file.
#'
#' When available, this approach is faster than writing line by line from R.
#' Helper function for \code{\link{write_bin}}.
#'
#' @inheritParams write_bin
#' @import data.table
#' @keywords internal

write_bin_using_com <- function(binData, binFilename) {

  checkIfValidBinFile(binFilename)
  if (!connected()) return()

  csv <- tempfile(fileext = ".csv")
  data.table::fwrite(binData, csv)
  view <- RunFunction("OpenTable", "temp", "CSV", list(csv, NA))
  viewset <- paste0(view, "|")
  RunFunction("ExportView", viewset, "FFB", binFilename, NA, NA)
  RunFunction("CloseView", view)
}

#' Write a FFB (.bin) file without a COM connection
#'
#' This function allows R to write directly to a FFB file even if Caliper
#' software is not installed on the machine. Helper function for
#' \code{\link{write_bin}}.
#'
#' @inheritParams write_bin
#' @param dcbKey The FFB table structure. This is create by \code{\link{write_bin}}
#' @keywords internal

write_bin_without_com <- function(binData, binFilename, dcbKey) {

  # Iterate over the columns of the data frame and the rows of the
  # dcbKey to create a matrix of raw bytes that can be written out
  # one column at a time.
  result <- mapply(
    function(x, tc_type, byteLength) {
      r_type <- TcTypeToRType(as.character(tc_type))
      nRows <- length(x)
      y <- sapply(x, RNaToTcMiss, tc_type)
      y <- switch(
        r_type,
        integer = as.integer(y),
        numeric = as.numeric(y),
        character = as.character(y),
        POSIXct = as.POSIXct(y),
        Date = as.Date(y)
      )
      if (r_type == "character") {
        z <- stringr::str_pad(y, byteLength, side = "right")
        m <- unname(sapply(z, charToRaw))
      } else {
        z <- writeBin(y, raw(), byteLength)
        m <- matrix(z, byteLength, nRows)
      }
      m
    },
    x = binData,
    tc_type = dcbKey$dataType,
    byteLength = dcbKey$byteLength,
    SIMPLIFY = FALSE
  )
  result <- do.call(rbind, result)
  if (!file.exists(binFilename)) file.create(binFilename)
  binFile <- file(binFilename, "wb")
  on.exit(close(binFile))
  apply(result, 2, function(x, connection = binFile) {
    writeBin(x, con = connection)
    return()
  })
}
