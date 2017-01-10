
#Libraries

library(caret)
library(dplyr)
library(ggplot2)
library(tidyr)
require(plotly)

#Load the data
rentals<-read.csv("Airbnb listings in Prague (March 2016).csv")[-1]%>%
  mutate(priceCZK=price*25.942)%>%
  mutate(pricePerPersonCZK=price/accommodates,
         pricePerRoomCZK=price/bedrooms)

#some basic histograms
hist(rentals$price[rentals$price<200], breaks=100)
hist(rentals$pricePerPerson[rentals$pricePerPerson<200], breaks=100)

#get only relevant neighbouthoods
filteredRentals<-rentals %>%
  group_by(neighborhood,room_type)%>%
  summarize(count=n(), AVGpricePerPersonCZK=mean(pricePerPersonCZK, na.rm=TRUE),AVGpriceCZK=mean(priceCZK, na.rm=TRUE), accommodates=mean(accommodates, na.rm=TRUE))%>%
  filter(count>30)%>%
  arrange(AVGpricePerPersonCZK)



# Notched Boxplot of Rentals Against 2 Crossed Factors
# boxes colored for ease of interpretation 
boxplot(price~room_type, data=rentals,main="Rentals by neighborhood and room type", xlab="toom type")

p <- ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity))) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)

ggplotly(p)

pts = read.table("file.csv",......)
b. Convert the data frame to a SpatialPointsDataFrame using the sp package and something like:
  
  library(sp)
library(rgdal)
coordinates(pts)=~x+y
c. Convert to your regular km system by first telling it what CRS it is, and then spTransform to the destination.

proj4string(pts)=CRS("+init=epsg:4326") # set it to lat-long
pts = spTransform(pts,CRS("insert your proj4 string here"))
d. Tell R that this is gridded:
  
  gridded(pts) = TRUE
At this point you'll get an error if your coordinates don't lie on a nice regular grid.
Now use the raster package to convert to a raster and set its CRS:
  r = raster(pts)
projection(r) = CRS("insert your proj4 string here")
Now have a look:
  plot(r)
Now write it as a geoTIFF file using the raster package:
  writeRaster(r,"pts.tif")