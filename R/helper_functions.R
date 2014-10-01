#### Helper functions (not exported) ####

# Make urls for GET requests
makeURL <- function(query, filter = "") {
  filter <- paste0(filter, collapse = " and ")
  filter <- ifelse(filter == "", "", 
                   paste0("$filter=", filter, "&"))
  url <- paste0("http://stats.oecd.org/OECDStatWCF_OData/OData.svc/", 
                query, 
                filter, 
                "$format=json")
  url <- stringr::str_replace_all(url, " ", "%20")
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

# Get XML with variable descriptions
getCodelistnode <- function(series) {
  url <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", series)
  xml_desc <- httr::content(httr::GET(url))
  codelist <- XML::xmlRoot(xml_desc)[["CodeLists"]]
  return(codelist)
}