#Help with mapping
library(tidyverse)
library(sf)
#Make Spatial Grid for iCAR Analysis
pot_est<-trnd <- read.csv("PosteriorSummary.csv") #pull in data to map
pot<-pot_est %>% select(alph, tau, id, taxa_code)

events<-read.csv("QCOWLSEvents.csv")

#remove COVID data
events<-events %>% filter(survey_year!=2020)
events<-events %>% filter(survey_year!=2021)
events<-events %>% select(latitude, longitude) %>% distinct()

xy<-st_as_sf(events, coords = c("longitude", "latitude"))
st_crs(xy)<-"+proj=longlat +datum=NAD83"

#all grid for North American
poly<- read_sf("nos_na_grid.shp") #pull in the grid data

#sf point
newCRS<-st_crs(poly) #get the CRS of the ply data
xy<-st_transform(xy, newCRS) #transform 
Grid <- poly %>% st_filter(xy, .pred = st_intersects)

qq <- rnaturalearth::ne_states(country = "canada", returnclass = "sf") %>% st_transform(newCRS) #pull in the background map
qq<-qq %>% filter(name=="Québec")


#Prepare study area map

ggplot()+ 
  geom_sf(data = qq, fill = NA) +
  geom_sf(data=Grid, aes(), size=0.3) + #this polygone layer is not overlapping the point like it should. 
  geom_sf(data=xy, aes(), size=1)+
  theme_map() + theme(panel.grid.major=element_line(colour="transparent"))



