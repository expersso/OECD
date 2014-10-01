library(XML)
library(lubridate)

# Make urls for GET requests
makeURL <- function(query, filter = "") {
  filter <- paste0(filter, collapse = " and ")
  filter <- ifelse(filter == "", "", 
                   paste0("$filter=", filter, "&"))
  url <- paste0("http://stats.oecd.org/OECDStatWCF_OData/OData.svc/", 
                query, 
                filter, 
                "$format=json")
  url <- str_replace_all(url, " ", "%20")
  return(url)
}

# Make filter string for countries
makeCountryFilter <- function(country = "all", country_term = "LOCATION") {
  suppressWarnings(
    if(country != "all") {
    countries <- paste0(sprintf("(%s eq '%s')", country_term, country), collapse = " or ")
    return(countries)
  } else return(""))
}

# Make filter string for time
makeTimeFilter <- function(time = NULL, time_term = "TIME") {
  suppressWarnings(
    if(!is.null(time)) {
      time <- sprintf("(%s ge '%s')", time_term, time)
      return(time)
    } else {
      return("")
    })
}

#' Get the dimensions of a dataset.
#' 
#' Rreturs a data frame containing names and descriptions 
#' of the variables of a specified series.
#' 
#' @param series A string containing the code for a series.
#' 
#' @return A data frame.
#' 
#' @seealso \code{\link{getDatasets}}, \code{\link{searchSeries}}
#' 
#' @examples
#' getDimensions("DUR_D")
getDimensions <- function(series) {
  url <- makeURL(query = paste0("GetDimension?DatasetCode=", series, "&"))
  data <- content(GET(url))
  data <- do.call(rbind, lapply(data[[2]], data.frame, stringsAsFactors = FALSE))
  return(data)
}

#' Get a data frame with information on all available datasets.
#' 
#' Returns a data frame with three variables: DatasetCode, 
#' DatasetTitle, and DatasetMetadata.
#' 
#' @return A data frame.
#' 
#' @seealso \code{\link{searchSeries}} to search for a specific DatasetCode or a 
#' keyword in the DatasetTitle, and \code{\link{getDimensions}} to get the 
#' dimensions of specified DatasetCode.
#'
#' @examples
#' datasets <- getDatasets()
#' datasets[1:10, 1:2]
getDatasets <- function() {
  url <- makeURL("GetDatasets?")
  data <- content(GET(url))
  data <- data[[2]]
  data <- ldply(data, "unlist")
  return(data)
}

#' Search codes and descriptions of available OECD series
#' 
#' Returns a data frame containing the series codes, descriptions, and 
#' (optionally) metadata for the OECD series which match the given criteria.
#' 
#' @param data The data frame to search. This can be either a data frame 
#' previously fetched using \code{\link{getDatasets}} (recommended) or left 
#' blank, in which case a temporary data frame is fetched. The second option 
#' adds a few seconds to each search query.
#' 
#' @param string A string to search for. Can include regular expressions.
#' 
#' @param metadata Whether or not to include a variable with metadata in the 
#' returned data frame.
#' 
#' @return A data frame.
#' 
#' @seealso \code{\link{getDatasets()}}
#' 
#' @examples
#' datasets <- getDatasets()
#' searchSeries(datasets, "unemployment")
searchSeries <- function(data = getDatasets(), string = "unemployment", metadata = FALSE) {
  results <- data[str_detect(data$DatasetTitle, ignore.case(string)),]
  if(!metadata) results <- results[,-3]
  return(results)
}

#' Download OECD series.
#' 
#' Returns a data frame with the requested data, downloaded from the OECD's API.
#' 
#' @param series A string with the code for the desired series.
#' @param country A character vector with iso3c codes 
#' \code{(e.g. c("USA", "DEU"))}.
#' 
#' @param since First year of data.
#' 
#' @return Data frame with country-year observations.
#' 
#' @examples
#' df <- getSeries("D_DUR", country = c("DEU", "FRA"), since = 2008)
#' head(df)
getSeries <- function(series, country = "all", since = NULL) {
  df_dim <- getDimensions(series)
  country_term <- df_dim$DimensionCode[df_dim$DimensionCode %in% c("COU", "COUNTRY", "LOCATION")]
  time_term <- df_dim$DimensionCode[df_dim$DimensionCode %in% c("TIME", "YEAR", "YEA")]
  
  if(length(country_term) == 0) {
    stop("Unit of observation not countries. This functionality not yet implemented.")
  }
  
  url <- makeURL(query = paste0(series, "?"), 
                 filter = paste0(makeCountryFilter(country = country, 
                                          country_term = country_term), 
                                 makeTimeFilter(time = since, 
                                                time_term = time_term)))
  
  result <- content(GET(url))
  result <- rbind.fill(lapply(result[[2]], function(f) {
    as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
  }))
  names(result) <- tolower(names(result))
  return(result)
}

# Get XML with variable descriptions
getCodelistnode <- function(series) {
  url <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", series)
  xml_desc <- content(GET(url))
  codelist <- xmlRoot(xml_desc)[["CodeLists"]]
  return(codelist)
}

#' Get a list of data frames with variable descriptions.
#' 
#' Returns a list of data frames containing descriptions of the variable values 
#' in the requested series.
#' 
#' The data frames returned with \code{\link{getSeries}} often contain variables
#' whose values are not obvious. For example, 
#' \code{\link{getSeries("MIG_UNEMP_GENDER")}}
getDesc <- function(series) {
  codelist_node <- getCodelistnode(series)
  codelist_children <- xmlChildren(codelist_node)
  df_desc_list <- list()
  
  for(i in 2:length(names(codelist_children))) {
    list <- xmlChildren(codelist_children[[i]])[-1] 
    df_code_temp <- do.call(rbind, lapply(list, xmlAttrs))
    df_desc_temp <- xmlToDataFrame(list)
    df_desc <- data.frame(df_code_temp, df_desc_temp, row.names = NULL, stringsAsFactors = FALSE)
    df_desc <- df_desc[,-ncol(df_desc)]
    names(df_desc) <- tolower(names(df_desc))
    df_desc_list[[i-1]] <- df_desc
  }
  names(df_desc_list) <- tolower(str_extract(sapply(codelist_children, xmlGetAttr, "id")[-1], "[A-Z]*$"))
  return(df_desc_list)
}

# Browse the metadata related to a series
browseMetadata <- function(series, data = getDatasets()) {
  metadata <- data[which(data$DatasetCode == series),3]
  tempxml <- tempfile("temp", fileext = ".xml")
  writeLines(metadata, con = tempxml)
  browseURL(tempxml)
}