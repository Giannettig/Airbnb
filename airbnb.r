
#Libraries
library(caret)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
require(plotly)
library(data.table)

#========================================================INPUT DATA================================================================#

##Configuration data - PRIVATE

#Map Quest Api Key

key<-"wuXUGczxCx1lvGxE23CdkR8lsAy4L0mU"

##Get Dump of AirBnB data - courtesy of http://tomslee.net/airbnb-data

rentals<-read.csv("Airbnb listings in Prague (March 2016).csv")[-1]%>%
  mutate(priceCZK=price*25.942)%>%
  mutate(pricePerPersonCZK=price/accommodates,
         pricePerRoomCZK=price/bedrooms)

##Enrichment process make sure that the source has Location an Latitude columns

#Load the required functions
source("functions.R")

#It takes a While so you can use a dump that I already made
enchancedRentals<-read.csv("AirbnbAugmented.csv")
##for a new run uncomment
#enchancedRentals<-enhanceGeodata(rentals,key)
#==========================================================ANALYSIS================================================================#



#some basic histograms
hist(rentals$price[rentals$price<200], breaks=100)
hist(rentals$pricePerPerson[rentals$pricePerPerson<200], breaks=100)

#get only relevant neighbouthoods
filteredRentals<-rentals %>%
  group_by(neighborhood,room_type)%>%
  summarize(count=n(), AVGpricePerPersonCZK=mean(pricePerPersonCZK, na.rm=TRUE),AVGpriceCZK=mean(priceCZK, na.rm=TRUE), accommodates=mean(accommodates, na.rm=TRUE))%>%
  filter(count>30)%>%
  arrange(AVGpricePerPersonCZK)



