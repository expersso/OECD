utils::globalVariables(c("en", "id", "description", "label.en", "."))

#' Get the data structure of a dataset.
#'
#' Returns a list of data frames containing names and descriptions of the variables
#' of a specified series.
#'
#' @param dataset A string containing the code for a dataset
#'
#' @return A list of data frames.
#'
#' @examples
#' \dontrun{
#' get_data_structure("OECD.SDD.NAD.SEEA/DSD_NAT_RES@DF_NAT_RES/1.0")
#' }
#'
#' @export
#' @import methods
get_data_structure <- function(dataset) {

  dataset <- gsub(",", "/", dataset)

  url <- paste0(
    "https://sdmx.oecd.org/public/rest/dataflow/", dataset, "?references=all"
  )
  data_structure <- rsdmx::readSDMX(url)

  # First data frame in returned list: data frame with full variables names
  variable_desc <- suppressWarnings(
    rsdmx:::as.data.frame.SDMXConcepts(data_structure@concepts)
  )

  # Clean up names (keeping only id and English description)
  variable_desc[] <- lapply(variable_desc, as.character)

  names(variable_desc)[length(names(variable_desc))] <- "description"
  variable_desc <- variable_desc[, c("id", "description")]

  # List of data frames in returned list: descriptions of factor levels
  code_names <- data_structure@codelists@codelists
  code_names <- vapply(code_names, function(x) x@id, "character")

  code_list <- lapply(code_names, function(x) {
    df <- as.data.frame(data_structure@codelists, codelistId = x)
    try(
      {
        df <- df[, c("id", "label.en")]
        names(df)[2] <- "label"
      },
      silent = TRUE
    )
    df
  })

  names(code_list) <- gsub(paste0("CL_", dataset, "_"), "", code_names)

  full_df_list <- c("VAR_DESC" = list(variable_desc), code_list)
  full_df_list
}

#' Download OECD data sets.
#'
#' Returns a data frame with the requested data, downloaded through the OECD's API.
#'
#' @param dataset A string with the code for the desired data set
#'
#' @param filter A character vectors specifying the filter to be applied to
#' each dimension of the dataset (see \code{examples} below).
#'
#' @param start_time Starting time/period for data. If left blank, no time filter is
#' applied (i.e. all observations since the earliest available observation are
#' downloaded).
#'
#' @param end_time End time/period for data.
#'
#' @param last_n_observations Number of most recent observations to download.
#'
#' @param ... Additional parameters passed to \code{read_sdmx}
#'
#' @return A data frame
#'
#' @examples
#' # Get entire dataset
#' \dontrun{
#' df <- get_dataset("OECD.SDD.NAD.SEEA/DSD_NAT_RES@DF_NAT_RES,1.0", "AUS+CAN.A....")
#' }
#' \dontrun{
#' head(df, 10)
#' }
#'
#' @export
get_dataset <- function(dataset, filter = NULL, start_time = NULL, end_time = NULL,
                        last_n_observations = NULL, ...) {

  dataset <- gsub("/", ",", dataset)

  path <- paste0(
    "/public/rest/data/", dataset, "/", filter
  )

  url_list <- list(
    "scheme" = "https",
    "hostname" = "sdmx.oecd.org",
    "path" = path,
    "query" = list(
      "startPeriod" = start_time,
      "endPeriod" = end_time,
      "lastNObservations" = last_n_observations,
      "dimensionAtObservation" = "AllDimensions"
    )
  )
  class(url_list) <- "url"

  url <- httr::build_url(url_list)

  df <- readsdmx::read_sdmx(url, ...)
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}
