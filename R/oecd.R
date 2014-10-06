#' Get the dimensions of a dataset.
#' 
#' Returs a data frame containing names and descriptions of the variables 
#' of a specified series.
#' 
#' @param series A string containing the code for a series.
#' 
#' @return A data frame.
#' 
#' @seealso \code{\link{getDesc}} for similar functionality regarding variable
#' values.
#' 
#' @examples
#' getDimensions("DUR_D")
#' @export
getDimensions <- function(series) {
  url <- makeURL(query = paste0("GetDimension?DatasetCode=", series, "&"))
  data <- httr::content(httr::GET(url))
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
#' @export
getDatasets <- function() {
  url <- makeURL("GetDatasets?")
  data <- httr::content(httr::GET(url))
  data <- data[[2]]
  data <- plyr::ldply(data, "unlist")
  return(data)
}

#' Search codes and descriptions of available OECD series
#' 
#' Returns a data frame containing the series codes, descriptions, and 
#' (optionally) metadata for the OECD series which match the given criteria.
#' 
#' @param string A string to search for. Can include regular expressions.
#' 
#' @param data The data frame to search. This can be either a data frame 
#' previously fetched using \code{\link{getDatasets}} (recommended) or left 
#' blank, in which case a temporary data frame is fetched. The second option 
#' adds a few seconds to each search query.
#' 
#' @param metadata Whether or not to include a variable with metadata in the 
#' returned data frame.
#' 
#' @return A data frame.
#' 
#' @seealso \code{\link{getDatasets}}
#' 
#' @examples
#' datasets <- getDatasets()
#' searchSeries(datasets, "unemployment")
#' @export
searchSeries <- function(string = "unemployment", data = getDatasets(), metadata = FALSE) {
  results <- data[stringr::str_detect(data$DatasetTitle, stringr::ignore.case(string)),]
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
#' df <- getSeries("DUR_D", country = c("DEU", "FRA"), since = 2008)
#' head(df)
#' @export
getSeries <- function(series, country = "all", since = NULL) {
  df_dim <- getDimensions(series)
  country_term <- df_dim$DimensionCode[df_dim$DimensionCode %in% c("COU", "COUNTRY", "LOCATION")]
  time_term <- df_dim$DimensionCode[df_dim$DimensionCode %in% c("TIME", "YEAR", "YEA")]
  
  if(length(country_term) == 0) {
    stop("Unit of observation not countries. This functionality not yet implemented.")
  }
  
  url <- makeURL(query = paste0(series, "?"), 
                 filter = c(makeCountryFilter(country = country, 
                                          country_term = country_term), 
                                 makeTimeFilter(time = since, 
                                                time_term = time_term)))
  
  result <- httr::content(httr::GET(url))
  result <- plyr::rbind.fill(lapply(result[[2]], function(f) {
    as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
  }))
  names(result) <- tolower(names(result))
  return(result)
}

#' Get a list of data frames with variable descriptions.
#' 
#' Returns a list of data frames containing descriptions of the variable values 
#' in the requested series.
#' 
#' The data frames returned with \code{\link{getSeries}} often contain variables
#' whose values are not obvious. For example, 
#' \code{getSeries("MIG_UNEMP_GENDER")} returns a data frame with the 
#' variable \code{birth} which has the values \code{FB} and \code{NB}, and the
#' variable \code{rate} which has the values \code{N_RATE} and \code{U_RATE}.
#' Since the meaning of these are not obvious, \code{getDesc} returns a list of 
#' data frames that can serve as human-readable look-up tables of variables 
#' (see examples below).
#' 
#' @param series A string specifying the code of the series.
#' 
#' @return A list of data frames, each corresponding to one variable in the 
#' specified series.
#' 
#' @seealso \code{\link{getDimensions}} provides similar functionality for 
#' variables names (e.g. getDimensions("MIG_UNEMP_GENDER"), to learn that 
#' the variable \code{birth} refers to "place of birth".
#' 
#' @examples
#' series <- "MIG_UNEMP_GENDER"
#' df <- getSeries(series)
#' unique(df$birth)
#' desc <- getDesc(series)
#' desc$birth
#' 
#' @export
getDesc <- function(series) {
  codelist_node <- getCodelistnode(series)
  codelist_children <- XML::xmlChildren(codelist_node)
  df_desc_list <- list()
  
  for(i in 2:length(names(codelist_children))) {
    list <- XML::xmlChildren(codelist_children[[i]])[-1] 
    df_code_temp <- do.call(rbind, lapply(list, XML::xmlAttrs))
    df_desc_temp <- XML::xmlToDataFrame(list, stringsAsFactors = FALSE)
    df_desc <- data.frame(df_code_temp, df_desc_temp, 
                          row.names = NULL, stringsAsFactors = FALSE)
    df_desc <- df_desc[,-ncol(df_desc)]
    names(df_desc) <- tolower(names(df_desc))
    df_desc_list[[i-1]] <- df_desc
  }
  names(df_desc_list) <- tolower(stringr::str_extract(sapply(codelist_children, 
                                                               XML::xmlGetAttr, 
                                                               "id")[-1], 
                                                        "[A-Z]*$"))
  return(df_desc_list)
}

#' Browse the metadata related to a series.
#' 
#' Opens up a web browser with the metadata related to the requested series.
#' 
#' @param series A string specifying the code of the series.
#' @param data A data frame with the data set codes, titles, and metadata. 
#' This can be either a data frame previously fetched using 
#' \code{\link{getDatasets}} (recommended) or left blank, in which case a 
#' temporary data frame is fetched. The second option adds a few seconds to 
#' each browse request.
#' 
#' @return Opens a temporary XML file in the default web browser.
#' 
#' @examples
#' datasets <- getDatasets()
#' browseMetadata("DUR_D", datasets)
#' 
#' @export
browseMetadata <- function(series, data = getDatasets()) {
  metadata <- data[which(data$DatasetCode == series),3]
  tempxml <- tempfile("temp", fileext = ".xml")
  writeLines(metadata, con = tempxml)
  browseURL(tempxml)
}