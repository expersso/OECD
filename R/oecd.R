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

# Get dimensions of a dataset
getDimensions <- function(series) {
  url <- makeURL(query = paste0("GetDimension?DatasetCode=", series, "&"))
  data <- content(GET(url))
  data <- do.call(rbind, lapply(data[[2]], data.frame, stringsAsFactors = FALSE))
  return(data)
}

# Download dataframe with all OECD datasets
getDatasets <- function() {
  url <- makeURL("GetDatasets?")
  data <- content(GET(url))
  data <- data[[2]]
  data <- ldply(data, "unlist")
  return(data)
}

# Search reference series in data frame from getDatasets
searchSeries <- function(data = getDatasets(), string = "unemployment", metadata = FALSE) {
  results <- data[str_detect(data$DatasetTitle, ignore.case(string)),]
  if(!metadata) results <- results[,-3]
  return(results)
}

# Download reference series
getSeries <- function(series, country = "all", since = NULL) {
  df_dim <- getDimensions(series)
  country_term <- df_dim$DimensionCode[df_dim$DimensionCode %in% c("COU", "COUNTRY", "LOCATION")]
  time_term <- df_dim$DimensionCode[df_dim$DimensionCode %in% c("TIME", "YEAR", "YEA")]
  
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

# Get a list of dataframes with variable descriptions
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

#### Test code ####

datasets <- getDatasets()
q <- searchSeries(datasets, "unemployment")
series <- "CPL"
mig <- getSeries(series, "all", 2009)
desc <- getDesc(series)
getDimensions(series)
test$indicator_desc <- desc$indicator$description[match(test$indicator, desc$indicator$value)]
test <- getSeries(series, c("FRA", "DEU"))
desc <- getDesc("SNA_TABLE1")

browseMetadata("SNA_TABLE1")
