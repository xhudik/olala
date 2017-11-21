library(rworldmap)
library(dplyr)
library(tibble)

#select countries and "density"
officeDF <- data.frame(country = c("CZE", "PHL", "PAK","IND", "USA"),
                    GDC = c(10, 10, 3, 4, 50))
officeMap <- joinCountryData2Map(officeDF, joinCode = "ISO3",
                              nameJoinColumn = "country")

#plot the map
mapCountryData(officeMap, nameColumnToPlot="GDC", catMethod = "pretty",#"categorical",
               missingCountryCol = gray(.8), addLegend=FALSE,lwd = 1.9)


#filter out names we are interested in
country_coord<-data.frame(coordinates(officeMap),stringsAsFactors=F) %>% rownames_to_column("Country") %>% 
  filter(Country %in% c("Czech Republic", "Pakistan", "India", "Philippines","United States of America" )) %>% 
  column_to_rownames("Country")

#put country names into the map
text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord))



#2.nd approach
#install.packages(c("maps", "mapdata"))
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(dplyr)

world <- map_data("world")
GDC <- world %>% filter(region %in% c("Philippines","Pakistan","India", "Czech Republic"))
USA <- subset(world,region=="USA")

prague <- geocode("prague")
prague$city <- "Prague"

pune <- geocode("pune")
pune$city <- "Pune"

mumbai <- geocode("mumbai")
mumbai$city <- "Mumbai"

salt <- geocode("salt lake city")
salt$city <- "Salt Lake City" 

manila <- geocode("manila")
manila$city <- "Manila" 

islamabad <- geocode("islamabad")
islamabad$city <- "Islamabad" 

lahore <- geocode("lahore")
lahore$city <- "Lahore" 


GDC_city <- bind_rows(prague, mumbai, salt, manila, islamabad)
#lets create 2nd city in rder not to overlap on the picture
GDC_2ndcity <- bind_rows(pune, lahore)

ggplot(data=world, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")+
  geom_polygon(data=GDC, color="black", fill="#f9e0c5")+
  geom_polygon(data=USA, color="black", fill="#f9e0c5")+
  geom_point(data=GDC_city, aes(x=lon,y=lat,group=city), size=2, colour="red") +
  geom_point(data=GDC_2ndcity, aes(x=lon,y=lat,group=city), size=2, colour="red") +
  #geom_text(data = GDC_city, aes(x=lon,y=lat,label=city,group=group),hjust = 0, nudge_x = 0.9, size=10, check_overlap = TRUE)  
  geom_label(data = GDC_2ndcity, aes(x=lon,y=lat,label=city,group=city), fill="#ec871b",colour = "white", fontface = "bold", hjust = 0, 
             nudge_x = -7, nudge_y = -1.8, size=5)  +
  geom_label(data = GDC_city, aes(x=lon,y=lat,label=city,group=city), fill="#ec871b",colour = "white", fontface = "bold",hjust = 0, 
             nudge_x = 0.9, size=7) 


  


#3. approach ggmap

library(ggmap)

ggworld <- get_map(location = "World", zoom = 3)
ggmap(ggworld)

#get lat and longitute of city
#geocode("SEATTLE")

library(OpenStreetMap)
library(ggplot2)
map <- openmap(c(70,-179),
               c(-70,179),zoom=1)
map <- openproj(map)
ggplot(map)
reclat <- c(50,20,30,40)
reclong <- c(30,40,30,50)         
autoplot(map) + geom_point(aes(x=reclong,y=reclat))