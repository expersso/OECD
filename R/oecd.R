library(XML)

# Make urls for GET requests
makeURL <- function(query, filter = "") {
  url <- paste0("http://stats.oecd.org/OECDStatWCF_OData/OData.svc/", 
                query, 
                "?$filter=", 
                filter, 
                "&$format=json")
  return(url)
}

makeCountryFilter <- function(country = "all") {
  suppressWarnings(
    if(!country == "all") {
    countries <- sprintf("(LOCATION eq '%s')", country) %>% 
    paste0(collapse = " or ") %>% 
    str_replace_all(" ", "%20")
    return(countries)
  } else {
    return("")
  })
}

# Download data frame with all OECD datasets
getDatasets <- function() {
  url <- makeURL("GetDatasets")
  data <- content(GET(url))
  data <- data[[2]]
  data <- ldply(data, "unlist")
  return(data)
}

# Search reference series in data frame from getDatasets
searchRefseries <- function(data = getDatasets(), string = "gdp", metadata = FALSE) {
  results <- data[str_detect(data$DatasetTitle, ignore.case(string)),]
  if(!metadata) results <- results[,-3]
  return(results)
}

# Download reference series
getRefseries <- function(series, country = "all") {
  url <- makeURL(series, makeCountryFilter(country))
  result <- content(GET(url))
  result <- rbind.fill(lapply(result[[2]], function(f) {
    as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
  }))
  names(result) <- tolower(names(result))
  return(result)
}

# Get meaningful variable names
getNames <- function(data, series) {
  url_def <- makeURL(paste0("GetDimension?DatasetCode=", series))
  df_def <- content(GET(url_def))
  df_def <- do.call(rbind, lapply(df_def[[2]], data.frame, stringsAsFactors = FALSE))
  names(data)[names(data) %in% df_def$DimensionCode] <- 
    df_def$DimensionName[names(data) %in% df_def$DimensionCode]
  names(data) <- tolower(names(data))
  return(data)
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

datasets <- getDatasets()
q <- searchRefseries(datasets, "gdp")
series <- "TABLE3_AEO2013_V2"
test <- getRefseries(series)
test <- getNames(test, series)
desc <- getDesc(series)

test$indicator_desc <- desc$indicator$description[match(test$indicator, desc$indicator$value)]

test <- getRefseries("SNA_TABLE1", country = c("FRA", "DEU"))
desc <- getDesc("SNA_TABLE1")

test$time <- as.numeric(test$time)
test %>%
  filter(transact == "B1_GA", measure == "VIXOB") %>%
  group_by(location) %>%
  mutate(rebased = value/value[time == 1975]) %>%
  qplot(data=., x = time, y = rebased, color = location, group = location, geom = "line")