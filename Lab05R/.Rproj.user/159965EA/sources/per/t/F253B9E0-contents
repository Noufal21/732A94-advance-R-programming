#' Calculate greatest common divisor
#' The class for linear regression
#' @name thenmapsApi
#' @rdname thenmapsApi
#' @aliases  thenmapsApi 
#' @title thenmap API
#' @import methods
#' @import httr
#' @import leaflet
#' @export thenmapsApi
#' 
#' @field center_cor list of lat and lbg.
#' 
#' 
thenmapsApi <- setRefClass(Class = "thenmapsApi",
                           fields = list(
                             center_cor = 'list')
                          
)
thenmapsApi$methods(
  #' @name initialize
  #' @title initialize object
  #' @description method description. method description
  initialize = function()
  {
    year = 1945
    .self$center_cor =list('fi-8'=c(64.310546, 26.672296,5),
                           'ch-8' = c(46.865152, 7.95670,8),
                           'no-4' = c(63.106342, 10.827992,5),
                           'dk-7'=c(56.038714, 9.267665,8),
                           'se-4'=c(59.3260668,17.8419719,5),
                           'us-4' = c(38.418421, -102.086314,4),
                           'gl-7'=c(72.249125, -40.777655,4))
  }
)

thenmapsApi$methods(
  #' @name worldmap
  #' @title get worldmap on year
  #' @description method description. method description
  #' @param year parameter description
  #'
  worldmap = function(year)
  {
    url = paste('http://api.thenmap.net/v2/world-2/geo/',year,'?geo_props=info|name|flag|id',sep = '')
    geoJSON = getData(url)
    return (geoJSON)
  }
)
thenmapsApi$methods(
  #' @name multiData
  #' @title get data by year and datasoruce
  #' @description method description. method description
  #' @param year parameter description
  #'@param dataSource parameter description
  #'
  multiData = function(year,dataSource)
  {
    dataSource = .self$getSource(year = year,dataSource = dataSource)
    url = paste('http://api.thenmap.net/v2/',dataSource,'/geo/',year,'?geo_props=info|name|flag|id',sep = '')
    geoJSON = getData(url)
  }
)
thenmapsApi$methods(
  #' @name getData
  #' @title run the https request to get the data
  #' @description method description. method description
  #' @param year parameter description
  #' @param dataSource parameter description
  getData = function(url)
  {
    getRequest =  GET(url)
    content= content(getRequest,'text')
    if(grepl("is not a valid date.",content))
      stop("Error")
    if(grepl('Sorry, that dataset does not exist',content))
      stop("Error")
    geoJSON = content %>% paste(collapse = "\n")
    return(geoJSON)
  }
)
thenmapsApi$methods(
  #' @name getSource
  #' @title get source according to timeline
  #' @description method description. method description
  #' @param year parameter description
  #'@param dataSource parameter description
  getSource = function(year,dataSource)
  {
    source = dataSource
    if(source == 'no-4' && year> 2006)
    {
      source = 'no-7'
    }
    if(source == "se-4" && year > 1974)
    {
      source = 'se-7'
    }
    return(source)
  }
)