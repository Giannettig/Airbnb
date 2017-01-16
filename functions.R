#Do some geocoding augmentation on the data
#install.packages("httr")
library(httr)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)


#========================================================GEOCODING AUGMENTATION USING API================================================================#



#' Reverse Geocode
#'
#' @param lat - latitude
#' @param lng - longitude
#' @param key - MapQuest API Key
#' @param call - the number of call if the function is called in a loop (i)
#'

getGeoResult<-function(lat,lng,key,call="not provided"){
  r <- RETRY("GET","http://open.mapquestapi.com/nominatim/v1/reverse.php", times=3,
             query = list(format="json", lat = lat, lon=lng,key=key, zoom=18, addressdetails=1))
  
  results<-if(r$status_code==200) {
    content(r,"parsed") #data.frame(,stringsAsFactors=FALSE)
  }else {
    stop(paste("API Call n.",call,"failed. status:",content(r)$status, sep=" "),call.=TRUE)}
}

#' Forward Geocode
#'
#' @param address
#' @param key - MapQuest API Key
#' @param call - the number of call if the function is called in a loop (i)
#' 
getGeolocation<-function(address, key,call="not provided"){
  r <- httr::RETRY("GET","http://www.mapquestapi.com/geocoding/v1/address", times=3,
             query = list(format="json", location=address,key=key))
  
  results<-if(r$status_code==200) {
    httr::content(r,"parsed") #data.frame(,stringsAsFactors=FALSE)
  }else {
    stop(paste("API Call n.",call,"failed. status:",content(r)$status, sep=" "),call.=TRUE)}
}


#' Enchance Geodata
#' 
#' Enchances a dataframe containing a longitude and latitude column with data from the the Map Quest API (mapquest.com) 
#' 
#' @param data - a dataframe containing the longitude and latitude column
#' @param key - MapQuest API key
#'
#' @return a dataframe with new columns retrieved by the api


enhanceGeodata<-function(data,key){
  
  ##Set the column names by calling the Open Street API with some dummy data
  result<-NULL
  
  ##Now check the in data
  if(class(data)!="data.frame") stop("The input is not in a data frame format")
  if( !("longitude" %in% names(data) & "latitude" %in% names(data))) stop("PLease make sure there are two columns called longitude and latitude in the input data")
  
  observations<-dim(data)[1]
  
  ##Set the progress bar
  pb <- winProgressBar(title="Mapquest Geocoding Augmentation", label="Connecting to API", min=0, max=observations, initial=0)
  
  ##Loop through the data and create
  for(i in 1:observations)
  {
    result<-rbind(result,getGeoResult(data$latitude[i],data$longitude[i],key,i))
    setWinProgressBar(pb, (i/observations)*100, label = paste("Augmenting row: ",i,"/",observations,rsep=""))
  }
  close(pb)
  
  adresa<-data.table::rbindlist(result$address,fill = TRUE)
  
  export<-data%>%
    cbind(result,adresa)%>%
    select(-address,-licence)%>%
    mutate(road=ifelse(is.na(road),ifelse(is.na(pitch),ifelse(is.na(pedestrian),footway,pedestrian),pitch),road))
  
}
  
  #' Enchance Address
  #' 
  #' Enchances a dataframe containing a longitude and latitude column with data from the the Map Quest API (mapquest.com) 
  #' 
  #' @param data - a dataframe containing an address column
  #' @param key - MapQuest API key
  #'
  #' @return a dataframe with new columns retrieved by the api

  
  
  enhanceAddress<-function(data,key){
    
    ##Set the column names by calling the Open Street API with some dummy data
    result<-NULL
    
    ##Now check the in data
    if(class(data)!="data.frame") stop("The input is not in a data frame format")
    if( !("address" %in% names(data))) stop("PLease make sure there is an address column in the input data")
    
    observations<-dim(data)[1]
    
    ##Set the progress bar
    pb <- winProgressBar(title="Mapquest Address Augmentation", label="Connecting to API", min=0, max=observations, initial=0)
    
    ##Loop through the data and create
    for(i in 1:observations)
    {
      result<-rbind(result,getGeolocation(data$address,key,i))
      setWinProgressBar(pb, (i/observations)*100, label = paste("Augmenting row: ",i,"/",observations,rsep=""))
    }
    close(pb)
    
    adresa<-data.table::rbindlist(result$address,fill = TRUE)
    
    export<-data%>%
      cbind(result,adresa)%>%
      select(-address,-licence)%>%
      mutate(road=ifelse(is.na(road),ifelse(is.na(pitch),ifelse(is.na(pedestrian),footway,pedestrian),pitch),road))
  
    
  }
  
#   #========================================================SREALITY WEB SCRAPER================================================================#
#   
#   #sreality web scraper
#   library(rvest)
#   
#   url<-"https://www.sreality.cz/hledani/prodej/byty/praha-1,praha-2,praha-3,praha-5,praha-6,praha-7,praha-10,praha-8?velikost=2%2Bkk,2%2B1,3%2Bkk,3%2B1,4%2Bkk,4%2B1&cena-od=0&cena-do=5000000"
#   
#   try<-read_html(url)
#   
#   #http://www.computerworld.com/article/2971265/application-development/how-to-drive-a-web-browser-with-r-and-rselenium.html
#   #https://www.r-bloggers.com/scraping-with-selenium/
#   #https://scrapinghub.com/scrapy-cloud/
#   #https://scrapinghub.com/portia/
#   
#   
#   library('RSelenium')
#   checkForServer() # search for and download Selenium Server java binary.  Only need to run once.
#   startServer() # run Selenium Server binary
#   remDr <- remoteDriver(browserName="firefox", port=4444) # instantiate remote driver to connect to Selenium Server
#   remDr$open(silent=T) # open web browser
#   remDr$navigate(url) # navigates to webpage
#   
# } 