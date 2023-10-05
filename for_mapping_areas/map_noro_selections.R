
library(leaflet)
library(tidycensus)
library(sf)
library(tigris)
library(tidyverse)
library(htmlwidgets)    # for saving html files
library(htmltools) 
library(leaflet.extras)

census_api_key("bdadb17ac3cdebcc23518a2367b622f8bcc24084")
county<-counties("MI",2010,cb=T,resolution="500k")

county <- county %>% mutate(color_in = case_when(NAME %in% c("Sanilac", "Lapeer", 
                                                             "St. Clair", "Macomb", "Oakland", 
                                                             "Washtenaw", "Wayne", 
                                                             "Monroe", "Livingston") ~ 1, 
                                                 T ~ 0))

# read in a location
location1 <- "C:/Users/juliegil/Dropbox (University of Michigan)/SPH-COVID Response/Jules/Cases_WWregions_forKevin/shape_file_folders/ann_arbor2/annarborcatch.shp"

# convert it to shape file system (R)
tx <- st_read(location1, stringsAsFactors=FALSE)
aa <- st_transform(tx, "+proj=longlat +ellps=WGS84 +datum=WGS84")
aa <- st_as_sf(aa)
aa <- st_cast(aa, to = c("POLYGON"))

# read in a location
location1 <- "C:/Users/juliegil/Documents/ArcGIS/Projects/CreateShapeFiles3/city_of_flint_wpcf_service_area.shp"

# convert it to shape file system (R)
tx <- st_read(location1, stringsAsFactors=FALSE)
fl <- st_transform(tx, "+proj=longlat +ellps=WGS84 +datum=WGS84")
fl <- st_as_sf(fl)
fl <- st_cast(fl, to = c("POLYGON"))

# read in a location
location1 <- "C:/Users/juliegil/Documents/ArcGIS/Projects/CreateShapeFiles2/city_of_jackson_sanitary_sewer.shp"

# convert it to shape file system (R)
tx <- st_read(location1, stringsAsFactors=FALSE)
js <- st_transform(tx, "+proj=longlat +ellps=WGS84 +datum=WGS84")
js <- st_as_sf(js)
js <- st_cast(js, to = c("POLYGON"))

# read in a location
location1 <- "C:/Users/juliegil/Dropbox (University of Michigan)/SPH-COVID Response/Jules/Cases_WWregions_forKevin/shape_file_folders/tecumseh2/tecumseh_ww.shp"

# convert it to shape file system (R)
tx <- st_read(location1, stringsAsFactors=FALSE)
tm <- st_transform(tx, "+proj=longlat +ellps=WGS84 +datum=WGS84")
tm <- st_as_sf(tm)
tm <- st_cast(tm, to = c("POLYGON"))

# read in a location
location1 <- "C:/Users/juliegil/Dropbox (University of Michigan)/SPH-COVID Response/Jules/Cases_WWregions_forKevin/shape_file_folders/ycua_wastewater_service_area/ycua_outline.shp"

# convert it to shape file system (R)
tx <- st_read(location1, stringsAsFactors=FALSE)
yc <- st_transform(tx, "+proj=longlat +ellps=WGS84 +datum=WGS84")


pall2<-colorBin(c("#FFFFFF","#754F5B"), county$color_in,2, pretty = T) 


noro_waste <- leaflet() %>% addPolygons(data = county$geometry,
                                        fillColor = pall2(county$color_in),
                                            color = "#cdc8b7", # you need to use hex colors
                                            fillOpacity = 0.5, 
                                            weight = 1, 
                                            smoothFactor = 0.2) %>%
                         addPolygons(data = aa$geometry,
                                        fillColor = "#C2C2C2",
                                        color = "#000000", # you need to use hex colors
                                        fillOpacity = 0.5, 
                                        weight = 1, 
                                        smoothFactor = 0.2) %>%
                         addPolygons(data = fl$geometry,
                                        fillColor = "#C2C2C2",
                                        color = "#000000", # you need to use hex colors
                                        fillOpacity = 0.5, 
                                        weight = 1, 
                                        smoothFactor = 0.2) %>%
                          addPolygons(data = js$geometry,
                                      fillColor = "#C2C2C2",
                                      color = "#000000", # you need to use hex colors
                                      fillOpacity = 0.5, 
                                      weight = 1, 
                                      smoothFactor = 0.2)  %>%
                          addPolygons(data = tm$geometry,
                                      fillColor = "#C2C2C2",
                                      color = "#000000", # you need to use hex colors
                                      fillOpacity = 0.5, 
                                      weight = 1, 
                                      smoothFactor = 0.2)  %>%
                          addPolygons(data = yc$geometry,
                                      fillColor = "#C2C2C2",
                                      color = "#000000", # you need to use hex colors
                                      fillOpacity = 0.5, 
                                      weight = 1, 
                                      smoothFactor = 0.2)

saveWidget(noro_waste,"C:/Users/juliegil/Dropbox (University of Michigan)/SPH-COVID Response/Jules/norovirus_ww_crosscorr/map_locations.html",selfcontained=T)

