library(tidyverse)
library(tmap)
library(sf)


#Juxtaposing DRDs and SIMD distribution by Local Authority

#STEP 1:
##reading in LA shapefile from here: https://data.gov.uk/dataset/8e3a4564-8081-42ec-8772-03ade11d4acf/local-authority-boundaries-scotland
LA_map <- read_sf("C:/Users/u449921/Documents/Drugsmap/Local_Authority_Boundaries_-_Scotland (1)/pub_las.shp") %>% 
  st_make_valid()
  
###########################
###########################
###########################
###########################
###########################
###########################

#STEP 2: READING IN DATA

#Step 2a: Drug deaths
##reading in mid-2020 population estimates, from Table 2 https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2020
LApopulation2020 <- read_csv("C:/Users/u449921/Documents/Drugsmap/Scotland_pop2020.csv")

##reading in 2020 DRDs by LA, from C1 - summary: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/drug-related-deaths-in-scotland/2020
DRDs2020 <- read_csv("C:/Users/u449921/Documents/Drugsmap/Scotland_drds.csv") %>% 
  mutate(local_authority = str_replace(local_authority, "&", "and"))
  
##calculating crude rates
crude_rates <- left_join(DRDs2020, LApopulation2020, by="local_authority") %>% 
  mutate(crudedeaths = ((DRDs*1000000)/population),
         local_auth = case_when(local_authority == "Na h-Eileanan Siar"~"Eilean Siar",
                                T~local_authority))
                                
#Step 2b: SIMD
##Reading in all the SIMD data + calculations
##From table 2: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/population-estimates-by-simd-2016

SIMD_byLA <- read_csv("C:/Users/u449921/Documents/Drugsmap/SIMD/SIMD_byLA.csv") %>% 
  select(1,2, 4) %>% 
  slice(6:325) %>% 
  mutate(SIMD20 = case_when(X2 %in% c("1","2") ~1,
                            T~0),
         X4 = as.numeric(str_replace(X4, "\\,", ""))) %>% 
  group_by(`Population estimates by Scottish Index of Multiple Deprivation (SIMD) 2020 decile by council area, sex and single year of age, June 2020`,
           SIMD20) %>% 
  summarise(totalpop = sum(X4)) %>% 
  pivot_wider(names_from = SIMD20, values_from = totalpop) %>% 
  mutate(population = `0`+`1`,
         simdprop = round(`1`/population*100,1),
         local_auth = case_when(`Population estimates by Scottish Index of Multiple Deprivation (SIMD) 2020 decile by council area, sex and single year of age, June 2020`=="Na h-Eileanan Siar"~"Eilean Siar",
                                T~`Population estimates by Scottish Index of Multiple Deprivation (SIMD) 2020 decile by council area, sex and single year of age, June 2020`)) %>% 
  select(local_auth, simdprop)
  
###########################
###########################
###########################
###########################
###########################
###########################

#STEP 3: MAPPIN'

##mapping DRDs
DRDs<- left_join(LA_map, crude_rates, by="local_auth") %>% 
  tm_shape()+
  tm_polygons("crudedeaths",
              title = "Crude DRDs \nper 1m",
              palette = "YlOrRd",
              style = "cont") +
  tm_layout(legend.outside=T,
            legend.position = c("left", "bottom"),
            legend.width = 3,
            main.title = "Drug-related deaths per 1,000,000 \n(crude rate) by Local Authority, 2020",
            main.title.size=1,
            attr.outside = T)+
tm_credits("Source: National Records of Scotland              \n                           ", ##Hacky fix to get spacing right
           position = c("LEFT", "TOP"),
           size = 1) 
# legend.position = c("LEFT", "bottom")

##mapping SIMD
SIMD<- left_join(LA_map, SIMD_byLA, by="local_auth") %>% 
  tm_shape()+
  tm_polygons("simdprop",
              title = "% in SIMD20",
              palette = "Blues",
              style = "cont") +
  tm_layout(legend.outside=T,
            legend.position = c("left", "bottom"),
            legend.width = 3,
            main.title = "Percent of population in most deprived \nSIMD quintile by Local Authority, 2020*",
            main.title.size=1,
            attr.outside = T) +
  tm_credits("*Note: Western Islands, Shetland Islands and Orkney \nIslands have no SIMD20 postcodes",  
             position = c("LEFT", "TOP"),
             size = 1) 
             
###########################
###########################
###########################
###########################
###########################
###########################
##FINAL STEP PUTTING TOGETHER
combined <- tmap_arrange(DRDs, SIMD)

tmap_save(combined,
          units = 'in',
          height = 5.5,
          width =8,
          filename = "C:/Users/u449921/Documents/Drugsmap/combined.png")
