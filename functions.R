#Do some geocoding augmentation on the data
#install.packages("httr")
library(httr)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)


#========================================================GEOCODING AUGMENTATION USING API================================================================#

getGeoResult<-function(lat,lng,key,call="not provided"){
  r <- RETRY("GET","http://open.mapquestapi.com/nominatim/v1/reverse.php", times=3,
             query = list(format="json", lat = lat, lon=lng,key=key, zoom=18, addressdetails=1))
  
  results<-if(r$status_code==200) {
    content(r,"parsed") #data.frame(,stringsAsFactors=FALSE)
  }else {
    stop(paste("API Call n.",call,"failed. status:",content(r)$status, sep=" "),call.=TRUE)}
}

getLatitude<-function(address,key,call="not provided"){
  r <- httr::RETRY("GET","http://www.mapquestapi.com/geocoding/v1/address", times=3,
             query = list(format="json", location=address,key=key))
  
  results<-if(r$status_code==200) {
    content(r,"parsed") #data.frame(,stringsAsFactors=FALSE)
  }else {
    stop(paste("API Call n.",call,"failed. status:",content(r)$status, sep=" "),call.=TRUE)}
}



#' enhanceGeodata
#' 
#' Enchances a dataframe containing a longitude and latitude column with data from the the Map Quest API (mapquest.com) 
#' 
#' @param data - a dataframe containing the longitude and latitude column
#' @param key - MapQuest API key
#'
#' @return a dataframe with new columns retrieved by the api


enhanceGeodata<-function(data,key){
  
  result<-NULL
  
  ##Now check the in data
  if(class(data)!="data.frame") stop("The input is not in a data frame format")
  if( !("longitude" %in% names(data) & "latitude" %in% names(data))) stop("PLease make sure there are two columns called longitude and latitude in the input data")
  
  observations<-dim(data)[1]
  
  ##Set the progress bar
  if(.Platform$OS.type=="windows"){
  pb <- winProgressBar(title="Mapquest Geocoding Augmentation", label="Connecting to API", min=0, max=observations, initial=0)
  }
  ##Loop through the data and create result
  for(i in 1:observations)
  {
    
    res<-getGeoResult(data$latitude[i],data$longitude[i],key,i)
    result<-rbind(result,res)
    if(.Platform$OS.type=="windows"){
    setWinProgressBar(pb, (i/observations)*100, label = paste("Augmenting row: ",i,"/",observations,rsep=""))}
  }
  if(.Platform$OS.type=="windows"){close(pb)}
  
  result<-data.frame(result)
  adresa<-data.table::rbindlist(result$address,fill=TRUE)
  result<-select(result,-address)
  adresa<- cbind(result,adresa)
  
  names(adresa)<-sapply(names(adresa),function(x){ifelse((x %in% names(data)), x<-paste(x,"-geo",sep=""),x)} )
  
  export<-data%>%
    cbind(adresa)%>%
    select(-address,-licence)%>%
    mutate(road=ifelse(is.na(road),ifelse(is.na(pitch),ifelse(is.na(pedestrian),footway,pedestrian),pitch),road))
}

#' Enchance Geodata
#' 
#' Enchances a dataframe containing an address column with data from the the Map Quest API (mapquest.com) 
#' 
#' @param data - a dataframe containing the longitude and latitude column
#' @param key - MapQuest API key
#'
#' @return a dataframe with new columns retrieved by the api

enhanceAddress<-function(data,key){
  
  result<-NULL
  
  ##Now check the in data
  if(class(data)!="data.frame") stop("The input is not in a data frame format")
  names(data)<-tolower(names(data))
  if( !("address" %in% names(data) | "adress" %in% names(data))) stop("PLease make sure there an adress column in the input data")
  
  observations<-dim(data)[1]
  
  ##Set the progress bar
  if(.Platform$OS.type=="windows"){
    pb <- winProgressBar(title="Mapquest Geocoding Augmentation", label="Connecting to API", min=0, max=observations, initial=0)
  }
  ##Loop through the data and create result
  for(i in 1:observations)
  {
    res<-getLatitude(data$address[i],key,i)$results
    res2<-as.data.frame(map(res,"locations")[[1]][[1]])
    result<-rbind(result,res2)
    if(.Platform$OS.type=="windows"){
      setWinProgressBar(pb, (i/observations)*100, label = paste("Augmenting row: ",i,"/",observations,rsep=""))}
  }
  if(.Platform$OS.type=="windows"){close(pb)}

  names(adresa)<-sapply(names(adresa),function(x){ifelse((x %in% names(data)), x<-paste(x,"-geo",sep=""),x)} )
  
  export<-data%>%
    cbind(result)%>%
    mutate(
      street = street,
      neighborhood = adminArea6,
      areaType = adminArea6Type,
      city	= adminArea5,
      county =	adminArea4,
      region	= adminArea3,
      country	= adminArea1,
      postalCode = postalCode,
      geocodeQuality = geocodeQuality,
      sideOfStreet = sideOfStreet,
      latitude = latLng.lat,
      longitude = latLng.lng,
      mapUrl = mapUrl
      )%>%
      select(
        -adminArea6, 
        -adminArea6Type,
        -adminArea5,
        -adminArea5Type,
        -adminArea4,
        -adminArea4Type,
        -adminArea3,	
        -adminArea3Type,
        -adminArea1,
        -adminArea1Type,
        -geocodeQualityCode,
        -dragPoint,
        -sideOfStreet,
        -linkId,
        -unknownInput,
        -type,
        -latLng.lat,
        -latLng.lng,
        -displayLatLng.lat,
        -displayLatLng.lng
      )
}
  #========================================================SREALITY WEB SCRAPER================================================================#
  
  # #sreality web scraper
  # library(rvest)
  # 
  # url<-"https://www.sreality.cz/hledani/prodej/byty/praha-1,praha-2,praha-3,praha-5,praha-6,praha-7,praha-10,praha-8?velikost=2%2Bkk,2%2B1,3%2Bkk,3%2B1,4%2Bkk,4%2B1&cena-od=0&cena-do=5000000"
  # 
  # try<-read_html(url)
  # 
  # #http://www.computerworld.com/article/2971265/application-development/how-to-drive-a-web-browser-with-r-and-rselenium.html
  # #https://www.r-bloggers.com/scraping-with-selenium/
  # #https://scrapinghub.com/scrapy-cloud/
  # 
  # 
  # library('RSelenium')
  # checkForServer() # search for and download Selenium Server java binary.  Only need to run once.
  # startServer() # run Selenium Server binary
  # remDr <- remoteDriver(browserName="chrome", port=4444) # instantiate remote driver to connect to Selenium Server
  # remDr$open(silent=T) # open web browser
  # remDr$navigate(url) # navigates to webpage

  