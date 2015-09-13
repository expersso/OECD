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
#' #datasets <- get_datasets()
#' #head(datasets)
#' 
#' @export
#' @importFrom dplyr "%>%"
get_datasets <- function() {
  
  "http://stats.oecd.org/RestSDMX/sdmx.ashx/GetKeyFamily/all" %>%
    xml2::read_xml() %>%
    xml2::as_list() %>%
    data.frame(id = sapply(bb[[2]], attr, "id"),
               description = sapply(bb[[2]], function(x) x[[1]][[1]]),
               stringsAsFactors = FALSE)
}

#' Search codes and descriptions of available OECD series
#' 
#' Returns a data frame containing the series codes and descriptions for the 
#' OECD series which match the given criteria.
#' 
#' @param string A string to search for. Can be a regular expression.
#' 
#' @param data The data frame to search. This can be either a data frame 
#' previously fetched using \code{\link{get_datasets}} (recommended) or left 
#' blank, in which case a temporary data frame is fetched. The second option 
#' adds a few seconds to each search query.
#' 
#' @param ignore.case Whether the search should be case-insensitive.
#' Defaults to \code{TRUE}.
#' 
#' @return A data frame.
#' 
#' @seealso \code{\link{get_datasets}}
#' 
#' @examples
#' #dsets <- get_datasets()
#' #search_dataset("employment", dsets)
#' @export
search_dataset <- function(string, data = get_datasets(), ignore.case = TRUE) {
  
  data %>% 
    dplyr::filter(grepl(string, description, ignore.case = ignore.case)) %>%
    as.data.frame()
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
    `names<-`(gsub(paste0("CL_", dataset, "_"), "", code_names))
  
  c("VAR_DESC" = list(variable_desc), code_list)
}

#' Browse the metadata related to a series.
#' 
#' Opens up a web browser with the metadata related to the requested series.
#' 
#' @param dataset A string specifying the code of the series.
#' 
#' @return Opens a web page in the default web browser.
#' 
#' @examples
#' \dontrun{browse_metadata("DUR_D")}
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
#' @param start_time Starting time for data. If left blank, no time filter is 
#' applied (i.e. all observations since the earliest available observation are 
#' downloaded). If end_time is specified, a \code{start_time} must also be specified.
#' 
#' @param end_time End time for data.
#' 
#' @param pre_formatted boolean. Set to TRUE if filter to be applied is already 
#' formatted (e.g. if copied from the OECD's SDMX generator (see example below)).
#' 
#' @return A data frame
#' 
#' @examples
#' # Get entire dataset 
#' #df <- get_dataset("EPL_OV")
#' #head(df, 10)
#' 
#' # Apply filter on dimensions "country" and "series"
#' #df <- get_dataset("EPL_OV", filter = list(c("DEU", "FRA"), c("EPRC_V1", "EPRC_V2")), 
#' #       start_time = 2008, end_time = 2010)
#' #head(df, 10)
#'
#' # Use pre-formatted filter copied from stats.oecd.org
#' #df <- get_dataset("PATS_REGION", filter = "PCT_A.INVENTORS.BEL+BE10+BE21.TOTAL+BIOTECH+ICT", start_time = 2008, end_time = 2010, pre_formatted = TRUE)
#' #head(df, 10)
#' 
#' @export
get_dataset <- function(dataset, filter = NULL, start_time = NULL, end_time = NULL, 
                        pre_formatted = FALSE) {
  
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
      filter <- filter %>% 
        lapply(function(x) paste(x, collapse = "+")) %>% 
        paste(collapse = ".")
  }
  
  # Case pre-formatted filter
  if (!is.null(filter) && pre_formatted) {
    filter <- filter
  }
  
  path <- sprintf("restsdmx/sdmx.ashx/GetData/%s/%s/all", dataset, filter)
    
  url_list = paste0("http://stats.oecd.org/", path)

  if (!is.null(start_time)) {
    start_time = paste0("startTime=", start_time)
  }

  if (!is.null(end_time)) {
    end_time = paste0("endTime=", end_time)
  }

  if (!is.null(start_time) && !is.null(end_time)) {
    url_list = paste0(url_list, "?", start_time, 
                      ifelse(is.null(end_time), "", "&"), end_time)
  } else if (!is.null(start_time) | !is.null(end_time)) {
    url_list = paste0(url_list, "?", start_time, end_time)
  }
  return(url_list)

  url_list %>% 
    rsdmx::readSDMX() %>% 
    as.data.frame()
}