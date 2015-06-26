#' Get a data frame with information on all available datasets.
#' 
#' Returns a data frame with two variables: \code{id} and \code{description}
#' 
#' @return A data frame.
#' 
#' @seealso \code{\link{search_dataset}} to search for a specific data set or a 
#' keyword in the description, and \code{\link{get_data_structure}} to get the 
#' dimensions of specified data set.
#'
#' @examples
#' datasets <- get_datasets()
#' head(datasets)
#' 
#' @export
get_datasets <- function() {
  
  "http://stats.oecd.org/RestSDMX/sdmx.ashx/GetKeyFamily/all" %>% 
    httr::GET() %>% 
    httr::content() %>% 
    XML::xmlToList() %>% 
    .[["KeyFamilies"]] %>% 
    plyr::ldply(function(x) data.frame("id" = x$.attrs["id"], "description" = x$Name$text)) %>% 
    .[, -1]  
}

#' Get the data structure of a dataset.
#' 
#' Returs a list of data frames containing names and descriptions of the variables 
#' of a specified series.
#' 
#' @param dataset A string containing the code for a dataset
#' 
#' @return A list of data frames.
#' 
#' @examples
#' get_data_structure("DUR_D")
#'
#' @export
get_data_structure <- function(dataset) {
  
  url <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", dataset)
  data_structure <- rsdmx::readSDMX(url)
  
  # First data frame in returned list: data frame with full variables names
  variable_desc <- data_structure %>% 
    slot("concepts") %>% 
    as.data.frame() %>%
    dplyr::rename("description" = en)
  
  # Drop French descriptions
  variable_desc[] <- lapply(variable_desc, as.character)
  variable_desc$description[is.na(variable_desc$description)] <- 
    variable_desc$Name.en[!is.na(variable_desc$Name.en)]
  
  variable_desc <- variable_desc %>% dplyr::select(id, description)
  
  # List of data frames in returned list: descriptions of factor levels
  code_names <- data_structure %>% 
    slot("codelists") %>% 
    slot("codelists") %>% 
    vapply(function(x) slot(x, "id"), FUN.VALUE = "character")
  
  code_list <- code_names %>% 
    lapply(function(x) {
      data_structure %>% 
        slot("codelists") %>% 
        as.data.frame(codelistId = x) %>% 
        dplyr::select(id, label.en) %>% 
        dplyr::rename("label" = label.en)
    }
    ) %>% 
    `names<-`(stringr::str_replace(code_names, paste0("CL_", dataset, "_"), ""))
  
  c("VAR_DESC" = list(variable_desc), code_list)
}

#' Browse the metadata related to a series.
#' 
#' Opens up a web browser with the metadata related to the requested series.
#' 
#' @param series A string specifying the code of the series.
#' 
#' @return Opens a web page in the default web browser.
#' 
#' @examples
#' # browse_metadata("DUR_D")
#' 
#' @export
browse_metadata <- function(dataset) {
  "http://stats.oecd.org/OECDStat_Metadata/ShowMetadata.ashx?Dataset=%s&Lang=en" %>% 
    sprintf(dataset) %>% 
    browseURL(url)
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
#' @param start_time If left blank, no time filter is applied (i.e. all observations since
#' the earliest available observation are downloaded). If end_time is specified, 
#' a \code{start_time} must also be specified.
#' 
#' @param end_time
#' 
#' @return A data frame
#' 
#' @examples
#' # Get entire dataset 
#' df <- get_dataset("EPL_OV")
#' head(df, 10)
#' 
#' # Apply filter on dimensions "country" and "series"
#' df <- get_dataset("EPL_OV", filter = list(c("DEU", "FRA"), c("EPRC_V1", "EPRC_V2")), 
#'        start_time = 2008, end_time = 2010)
#' head(df, 10)
#' 
#' @export
get_dataset <- function(dataset, filter = NULL, start_time = NULL, end_time = NULL) {
    
  if (is.null(filter)) {
    filter <- "all"
  } else {
    filter <- filter %>% 
      lapply(function(x) paste(x, collapse = "+")) %>% 
      paste(collapse = ".")
  }
  
  path <- sprintf("restsdmx/sdmx.ashx/GetData/%s/%s/all", dataset, filter)
  
  url_list <- list("scheme"   = "http", 
                   "hostname" = "stats.oecd.org",
                   "path"     = path,
                   "query"    = list("startTime" = start_time,
                                     "endTime" = end_time))
  class(url_list) <- "url"
  url <- httr::build_url(url_list)
  rsdmx::readSDMX(url) %>% 
    as.data.frame()
}