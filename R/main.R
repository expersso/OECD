utils::globalVariables(c("en", "id", "description", "label.en", "."))

#' Get a data frame with information on all available datasets.
#' 
#' Returns a data frame with two variables: \code{id} and \code{description}
#' 
#' @param ... Additional parameters passed to \code{data.frame} (e.g.
#'   stringsAsFactors = FALSE).
#'   
#' @return A data frame.
#'   
#' @seealso \code{\link{search_dataset}} to search for a specific data set or a 
#'   keyword in the description, and \code{\link{get_data_structure}} to get the
#'   dimensions of specified data set.
#'   
#' @examples
#' \dontrun{datasets <- get_datasets()}
#' \dontrun{head(datasets)}
#' 
#' @export
get_datasets <- function(...) {
  
  url <- "http://stats.oecd.org/RestSDMX/sdmx.ashx/GetKeyFamily/all"
  
  page <- xml2::read_xml(url)
  
  id <- xml2::xml_attr(xml2::xml_find_all(page, "//*[@agencyID='OECD']"), "id")
  
  title <- xml2::xml_text(
    xml2::xml_find_all(page, "//*[@agencyID='OECD']/*[@xml:lang='en']"))
  
  df <- data.frame(id, title, ...)
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}

#' Search codes and descriptions of available OECD series
#' 
#' Returns a data frame containing the series codes and descriptions for the 
#' OECD series which match the given criteria.
#' 
#' @param string A regular expression string to search for.
#' 
#' @param data The data frame to search. This can be either a data frame 
#' previously fetched using \code{\link{get_datasets}} (recommended) or left 
#' blank, in which case a temporary data frame is fetched. The second option 
#' adds a few seconds to each search query.
#' 
#' @param ignore.case Whether the search should be case-insensitive.
#' 
#' @return A data frame.
#' 
#' @seealso \code{\link{get_datasets}}
#' 
#' @examples
#' \dontrun{dsets <- get_datasets()}
#' \dontrun{search_dataset("employment", dsets)}
#' 
#' @export
search_dataset <- function(string, data = get_datasets(),  ignore.case = TRUE) {
  data[grepl(string, data$title, ignore.case = ignore.case), ]
}

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
#' \dontrun{get_data_structure("DUR_D")}
#'
#' @export
#' @import methods
get_data_structure <- function(dataset) {
  
  url <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/",
                dataset)
  data_structure <- rsdmx::readSDMX(url)
  
  # First data frame in returned list: data frame with full variables names
  variable_desc <- data.frame(data_structure@concepts)
  
  # Clean up names (keeping only id and English description)
  variable_desc[] <- lapply(variable_desc, as.character)
  
  variable_desc$en[is.na(variable_desc$en)] <- 
    variable_desc$Name.en[!is.na(variable_desc$Name.en)]
  
  names(variable_desc)[length(names(variable_desc))] <- "description"
  variable_desc <- variable_desc[, c("id", "description")]
  
  # List of data frames in returned list: descriptions of factor levels
  code_names <- data_structure@codelists@codelists
  code_names <- vapply(code_names, function(x) x@id, "character")
  
  code_list <-  lapply(code_names, function(x) {
    df <- as.data.frame(data_structure@codelists, codelistId = x)
    try({
      df <- df[, c("id", "label.en")]
      names(df)[2] <- "label"
    }, silent = TRUE)
    df
  })
  
  names(code_list) <- gsub(paste0("CL_", dataset, "_"), "", code_names)
  
  full_df_list <- c("VAR_DESC" = list(variable_desc), code_list)
  full_df_list
}

#' Browse the metadata related to a series.
#' 
#' Opens up a web browser with the metadata related to the requested series.
#' 
#' @param dataset A string specifying the code of the series.
#' @param ... Additional parameters passed to browseURL.
#' 
#' @return Opens a web page in the default web browser.
#' 
#' @examples
#' \dontrun{browse_metadata("DUR_D")}
#' 
#' @export
browse_metadata <- function(dataset, ...) {
  
  url <- sprintf(
    "http://stats.oecd.org/OECDStat_Metadata/ShowMetadata.ashx?Dataset=%s&Lang=en",
    dataset)

  utils::browseURL(url, ...)
}

#' Download OECD data sets.
#' 
#' Returns a data frame with the requested data, downloaded through the OECD's API.
#' 
#' @param dataset A string with the code for the desired data set
#' 
#' @param filter A list of character vectors specifying filters to be applied to 
#' each dimension of the dataset (see \code{examples} below). If no filter is specified,
#' the function downloads all dimensions unfiltered.
#' 
#' @param start_time Starting time for data. If left blank, no time filter is 
#' applied (i.e. all observations since the earliest available observation are 
#' downloaded). If end_time is specified, a \code{start_time} must also be specified.
#' 
#' @param end_time End time for data.
#' 
#' @param pre_formatted boolean. Set to TRUE if filter to be applied is already 
#' formatted (e.g. if copied from the OECD's SDMX generator (see example below)).
#' 
#' @param ... Additional parameters passed to \code{data.frame} (e.g.
#'   stringsAsFactors = FALSE).
#'   
#' @return A data frame
#' 
#' @examples
#' # Get entire dataset 
#' \dontrun{df <- get_dataset("EPL_OV")}
#' \dontrun{head(df, 10)}
#' 
#' # Apply filter on dimensions "country" and "series"
#' \dontrun{df <- get_dataset("EPL_OV", 
#'                            filter = list(c("DEU", "FRA"), 
#'                            c("EPRC_V1", "EPRC_V2")), 
#'                            start_time = 2008, end_time = 2010)}
#' \dontrun{head(df, 10)}
#'
#' # Use pre-formatted filter copied from stats.oecd.org
#' \dontrun{df <- get_dataset("PATS_REGION", 
#'                filter = "PCT_A.INVENTORS.BEL+BE10+BE21.TOTAL+BIOTECH+ICT",
#'                start_time = 2008, end_time = 2010, pre_formatted = TRUE)}
#' \dontrun{head(df, 10)}
#' 
#' @export
get_dataset <- function(dataset, filter = NULL, start_time = NULL, end_time = NULL, 
                        pre_formatted = FALSE, ...) {
  
  # Case error
  if (is.null(filter) && pre_formatted) {
    stop("If pre_formatted is TRUE, you must provide a value to the filter argument.")
  }
  
  # Case all data
  if (is.null(filter) && !pre_formatted) {
    filter <- "all"
  } 
  
  # Case user-provided filter
  if (!is.null(filter) && !pre_formatted) {
      filter <- lapply(filter, function(x) paste(x, collapse = "+")) 
      filter <- paste(filter, collapse = ".")
  }
  
  # Case pre-formatted filter
  if (!is.null(filter) && pre_formatted) {
    filter <- filter
  }
  
  path <- sprintf("restsdmx/sdmx.ashx/GetData/%s/%s/all", dataset, filter)
  
  url_list <- list("scheme"   = "http", 
                   "hostname" = "stats.oecd.org",
                   "path"     = path,
                   "query"    = list("startTime" = start_time,
                                     "endTime" = end_time))
  class(url_list) <- "url"
  
  url <- httr::build_url(url_list)
  df <- as.data.frame(rsdmx::readSDMX(url), ...)
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}