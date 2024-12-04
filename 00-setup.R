#Setup for NOS Analysis

#Install packages 
#install.packages("naturecounts", 
#                 repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
#                           CRAN = 'https://cloud.r-project.org'))

#install.packages("INLA", repos=c(getOption("repos"), 
#        INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

#library(devtools)
#remotes::install_github("inbo/inlatools")

#Load libraries
library(tidyverse)
library(stringr)
library(INLA)
library(inlabru)
library(VGAM)
library(reshape)
library(sp)
library(sf)
library(spdep)
library(naturecounts)
library(inlatools)
library(maps)
library(ggplot2)
library(terra)
library(tidyterra) # raster plotting
library(tidyr)
library(scales)
library(dplyr)
library(prettymapr)
library(fmesher)
library(MatrixModels)
library(Matrix)
library(ggspatial)
library(viridis)
#library(iemisc)
library(oce)
library(mapview)
library(ggmap) 
library(mapproj)
register_stadiamaps("1cdeb586-3f5a-4ef9-813b-02375efa9f21") 

# Create folders as necessary
if(!dir.exists("data")) dir.create("data")
if(!dir.exists("output")) dir.create("output")
if(!dir.exists("plots")) dir.create("plots")

out.dir <- paste("output/")
dat.dir <- paste("data/")
plot.dir<-paste("plots/")

## Source scripts
source("./functions/LOESS.R")

#These tables are available on Github 

#Main Analysis Parameters file 
anal.param<-read.csv("Analysis Parameters.csv")
#remove sites that we don't want for the national analysis
anal.param<-anal.param %>% filter(!is.na(protocol_id)) %>% filter(protocol_id!=29)

epsg6703km <- paste(
  "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5",
  "+lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83",
  "+units=km +no_defs"
)

#load species names list
sp.names<-meta_species_taxonomy()
sp.names<-sp.names %>% dplyr::select(species_id, english_name, scientific_name)

##-----------------------------------------------------------
#Create plot functions

make_plot_field <- function(data_stk, scale_label) {
  ggplot() +
    geom_sf(data = qq, fill = NA) +
    #annotation_map_tile(type = "osm", zoomin = 0) +
    geom_sf(fill = NA) +
    coord_sf(datum = NA) +
    geom_spatraster(data = data_stk) +
    labs(x = "", y = "") +
    #scale_fill_gradient2(low = ("red4"),
    #                     mid = "white",
    #                     high = ("royalblue4"), midpoint = 0, space = "Lab",
    #                     guide = "colourbar") +
    scale_fill_viridis(option="magma", scale_label, na.value="transparent")+
    #scale_fill_distiller(scale_label, palette = "Blue-Red", na.value = "transparent") +
    theme_bw() +
    geom_sf(fill = NA)
}

make_plot_site <- function(data, scale_label) {
  ggplot() +
    geom_sf(data = qq, fill = NA) +
    #annotation_map_tile(type = "osm", zoomin = 0) +
    geom_sf() +
    coord_sf(datum = NA) +
    geom_sf(data = data, size = 5, mapping = aes(colour = value, geometry=geometry)) +
    labs(x = "", y = "") +
    #scale_fill_gradient2(low = ("red4"),
    #                     mid = "white",
    #                     high = ("royalblue4"), midpoint = 0, space = "Lab",
    #                     guide = "colourbar") +
    scale_colour_viridis(option="magma", scale_label, na.value="transparent")+
    #scale_colour_distiller(scale_label, palette = "Blue-Red", na.value = "transparent") +
    theme_bw() +
    geom_sf(fill = NA)
}

