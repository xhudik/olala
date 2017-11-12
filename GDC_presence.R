library(rworldmap)
library(dplyr)
library(tibble)

#select countries and "density"
officeDF <- data.frame(country = c("CZE", "PHL", "PAK","IND", "USA"),
                    GDC = c(1, 1, 1, 1, 0.9))
officeMap <- joinCountryData2Map(officeDF, joinCode = "ISO3",
                              nameJoinColumn = "country")

#plot the map
mapCountryData(officeMap, nameColumnToPlot="GDC", catMethod = "categorical",
               missingCountryCol = gray(.8), addLegend=FALSE,lwd = 1.2)

#filter out names we are interested in
country_coord<-data.frame(coordinates(officeMap),stringsAsFactors=F) %>% rownames_to_column("Country") %>% 
  filter(Country %in% c("Czech Republic", "Pakistan", "India", "Philippines","United States of America" )) %>% 
  column_to_rownames("Country")

#put country names into the map
text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord))

