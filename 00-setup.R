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
library(iemisc)
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

## Source scripts
source("./functions/LOESS.R")

#These tables are available on Github 

#Main Analysis Parameters file 
anal.param<-read.csv("Analysis Parameters.csv")
#remove sites that we don't want for the national analysis
anal.param<-anal.param %>% filter(!is.na(protocol_id)) %>% filter(protocol_id!=29)

#BC specific
BCregion<-read.csv("Regions_BCY.csv")

epsg6703km <- paste(
  "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5",
  "+lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83",
  "+units=km +no_defs"
)

#load species names list
sp.names<-meta_species_taxonomy()
sp.names<-sp.names %>% dplyr::select(species_id, english_name, scientific_name)
