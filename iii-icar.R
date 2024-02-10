#iCAR analysis

dat<-read.csv(paste(out.dir, collection, "OwlDataClean.csv", sep=""))
events<-read.csv(paste(out.dir, collection, "Events.csv", sep=""))
loc.dat<-read.csv(paste(out.dir, "Map", collection, ".csv", sep=""))
loc.dat<-loc.dat %>% na.omit()

#Make Spatial Grid for iCAR Analysis

#all grid for North American
poly<- read_sf(dsn="C:/Users/dethier/Documents/ethier-scripts/National-NOS/data", layer="nos_na_grid")

#sf point
xy<-st_as_sf(loc.dat, coords = c("longitude", "latitude"))
st_crs(xy)<-"+proj=longlat +datum=NAD83"

newCRS<-st_crs(poly)
xy<-st_transform(xy, newCRS)
test<-st_intersects(xy, poly)

###############
#Add UTM coords
UTM<-lonlat2utm( 
  loc.dat$latitude,
  loc.dat$longitude
)

loc.dat$Easting<-UTM$easting
loc.dat$Northing<-UTM$northing

coordinates(loc.dat)<-data.frame(easting=loc.dat$Easting, northing=loc.dat$Northing, proj4string = CRS("+proj=longlat +datum=NAD83"))
loc.sp <- SpatialPointsDataFrame(c(loc.dat[,c('longitude','latitude')]), data = loc.dat, proj4string = CRS("+proj=longlat +datum=NAD83"))

#SP points
xy <- data.frame(long=loc.dat$longitude, lat=loc.dat$latitude)
coordinates(xy) <- ~long+lat
proj4string(xy) <- CRS("+proj=longlat +datum=NAD83")
################



#grid data are only those cells containing data. This layers is created in ArcGIS. 
#Grid <- st_read(dsn="C:/Users/dethier/Documents/ethier-scripts/National-NOS/data", layer="QC_Grid_New")
nb1 <- spdep::poly2nb(Grid, row.names=Grid$data); nb1
is.symmetric.nb(nb1, verbose = FALSE, force = TRUE)
nb2INLA("nb1.graph", nb1)
nb1.adj <- paste(getwd(),"/nb1.graph", sep="")
g1 <- inla.read.graph("nb1.graph")

#This needed 'Projected' in Arc GIS from Albers to Nad83
Grid_proj<-st_read(dsn="C:/Users/dethier/Documents/ethier-scripts/National-NOS/data", layer="QC_Grid_proj")

#Routes .shp made in ArcGIS
Route <- st_read(dsn="C:/Users/dethier/Documents/ethier-scripts/National-NOS/data", layer="QC_Route_new")

#spatial join
grid<-st_join(Route, left=TRUE, Grid_proj)
grid<-grid %>% select(RouteIdent, latitude, longitude, id, bcr_number, bcr_name)