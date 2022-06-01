library(tidyverse)
library(tmap)
library(sf)

#Script which formats relevant shapefiles and datasets, and creates map

#STEP 1: Find shapefiles
##England/Wales/NI shapefile from here: https://geoportal.statistics.gov.uk/datasets/ons::nuts-level-1-january-2018-full-clipped-boundaries-in-the-uanited-kingdom/explore?location=54.484119%2C-3.250000%2C5.99
England_Wales_NI <- read_sf("C:/Users/u449921/Documents/Drugsmap/England_wales_NI/NUTS_Level_1_(January_2018)_Boundaries.shp") %>% 
  mutate(local_auth = str_remove(nuts118nm, " \\s*\\([^\\)]+\\)"),
         code = nuts118cd,
         hectares = st_areasha) %>%
  select(local_auth, code, hectares, geometry) %>%
  filter(local_auth != "Scotland")
  
##Scotland shapefile from here: https://www.spatialdata.gov.scot/geonetwork/srv/api/records/f12c3826-4b4b-40e6-bf4f-77b9ed01dc14
Scotlandmap <- read_sf("C:/Users/u449921/Documents/Workforce/Survey work/NHS_healthboards_2019/SG_NHS_HealthBoards_2019.shp") %>% 
  mutate(local_auth = HBName,
         code = HBCode,
         hectares = Shape_Area) %>% 
  select(local_auth, code, hectares, geometry)
  
  
#STEP 2: Transform coordinates of both maps to British National Grid format
England_Wales_NI2 <-  st_transform(England_Wales_NI, "EPSG:27700")
Scotlandmap2 <- st_transform(Scotlandmap, "EPSG:27700")

#STEP 3: Bringing in crude deaths
##From previous work done in this area, located on eRDM: https://erdm.scotland.gov.uk:8443/documents/A37912852/details
tomerge <- read_csv("C:/Users/u449921/Documents/Drugsmap/crudedeaths.csv")

####################################################################################
#VIS
##UNITED KINGDOM
output <- left_join(FINALSHAPEFILE, tomerge, by="local_auth") %>% 
  tm_shape()+
  tm_polygons("crude_deaths", 
              title = "Crude drug death rates",
              style = 'cont',
              palette = 'YlOrRd')+
  tm_layout(legend.outside=T,
            legend.position = c("left", "bottom"),
            legend.width = 4,
            main.title = "UK drug death rates per 1,000,000 \n(crude rate), 2020", 
            main.title.size=1.5,
            attr.outside =  T) +
  tm_credits("Sources: National Records of Scotland, \nOffice of National Statistics, \nNorthern Ireland Statistics and Research Agency",
             position = c("LEFT", "TOP")) 
             
#########################################################################################
#VIS
##rUK (i.e. without Scotland)
output_rUK<- left_join(England_Wales_NI2, tomerge, by='local_auth') %>% 
  tm_shape()+
  tm_polygons("crude_deaths", 
              title = "Drug deaths per 1m",
              style = 'cont',
              palette = 'YlOrRd')+
  tm_layout(legend.outside=T,
            legend.position = c("left", "bottom"),
            legend.width = 4,
            main.title = "Drug death rates per 1,000,000 (crude rate), \nEngland, Wales and Northern Ireland, 2020", 
            main.title.size=1.5,
            attr.outside =  T) +
  tm_credits("Sources: Office of National Statistics, \nNorthern Ireland Statistics and Research Agency",
             position = c("LEFT", "TOP"))

#########################################################################################
#SAVING FUNCTION
tmap_save(output_rUK, 
          units = 'in',
          height = 7,
          width =7,
          filename = "C:/Users/u449921/Documents/Drugsmap/rUK.png")
