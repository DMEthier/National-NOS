##Combined 

out.dir<-"output/QC/"

#load events data
events<-read.csv("output/QCOWLSEvents.csv")

#remove COVID data
events<-events %>% filter(survey_year!=2020)
events<-events %>% filter(survey_year!=2021)

#load cell outputs
pot_est<-read.csv("output/QC/PosteriorSummary.csv")

#load route outputs
plot_route<-read.csv("output/QC/PosteriorSummaryRoute.csv")

#plot theme
#map theme
theme_map <- function(base_size = 9, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank(),
          legend.background=element_rect(fill=NA, colour=NA),
          legend.direction="vertical",
          legend.key=element_rect(fill=NA, colour="white"),
          legend.text.align=1,
          legend.text = element_text(size=9),
          legend.title=element_text(hjust=0, size=11),
          legend.justification=c(0, 0.5),
          plot.title = element_text(size=14, hjust = 0.7))
}


#Make Spatial Grid for iCAR Analysis
test<-events %>% select(RouteIdentifier, latitude, longitude) %>% distinct()

xy<-st_as_sf(test, coords = c("longitude", "latitude"))
st_crs(xy)<-"+proj=longlat +datum=NAD83"

#all grid for North American
poly<- read_sf(dsn="C:/Users/dethier/Documents/ethier-scripts/National-NOS/data", layer="nos_na_grid")

#sf point
newCRS<-st_crs(poly)
xy<-st_transform(xy, newCRS)

xy<-st_transform(xy, newCRS) #transform 
Grid <- poly %>% st_filter(xy, .pred = st_intersects)

# make cell level maps ---------------------------------------------------------
pot<-pot_est %>% select(alph, tau, id, taxa_code)
pot<-pot %>% drop_na() %>% dplyr::rename("CommonName"="taxa_code")
plot_grid<-pot

plot_route<-plot_route %>% drop_na()
plot_route<-plot_route %>% select(RouteIdentifier, alph, tau, CommonName)

results_cells <- merge(Grid, pot, by="id", all=F)
res_sf <- as(results_cells, "sf") 

route_10<-read.csv("output/QC/Route10yrList.csv")

#load outside map
qq <- rnaturalearth::ne_states(country = "canada", returnclass = "sf") %>% st_transform(newCRS) #pull in the background map
qq<-qq %>% filter(name=="Québec")

# make route level maps ---------------------------------------------------------

sp.list<-unique(plot_route$CommonName)

for(m in 1:length(sp.list)) {
  #m<-1 #for testing each species
  
  route.data<-NULL
  route.data<-route_10 %>% filter(CommonName %in% sp.list[m])
  route.list<-unique(route.data$RouteIdentifier)
  
  plot.r <-NULL 
  plot.r <- plot_route %>% filter(CommonName %in% sp.list[m])
  plot.r<-left_join(plot.r, xy, by="RouteIdentifier")
  plot.r<-plot.r %>% filter(RouteIdentifier %in% route.list)
  
  aplh.r<-range(plot.r$alph, plot.g$alph)
  tau.r<-range(plot.r$tau, plot.g$tau)
  
  plot.g <-NULL 
  plot.g <- res_sf %>% filter(CommonName %in% sp.list[m])
    
    # map tau grid
    tau_p1 <- ggplot() +
      
      geom_sf(data=Grid, aes(), size=0.3) +
      geom_sf(data=plot.g, aes(fill=tau), col="gray40", size=0.3) +
      scale_fill_gradient2("Tau\n(% per year)", low = ("red4"),
                           mid = "white",
                           high = ("royalblue4"), midpoint = 0, space = "Lab",
                           na.value = "grey40", guide = "colourbar", limits =tau.r) +
      geom_sf() +
      coord_sf(datum = NA) +  
      geom_sf(data = plot.r, size = 4, aes(geometry=geometry, colour=tau)) +
      scale_colour_gradient2("Tau\n(% per year)", low = ("red4"),
                             mid = "white",
                             high = ("royalblue4"), midpoint = 0, space = "Lab",
                             guide = "colourbar", limits=tau.r) +
      geom_sf(data = qq, fill = NA) +
      theme_map() + theme(panel.grid.major=element_line(colour="transparent"))
    
    # print cell maps
    ggsave(paste(out.dir, sp.list[m], "TauPlot.jpeg"), plot=tau_p1)
    
    # map alpha grid
    alph_p1 <- ggplot() +
      geom_sf(data=Grid, aes(), size=0.3) +
      # annotation_map_tile(type = "osm", zoomin = 0) +
      geom_sf(data=plot.g, aes(fill=alph), col="gray40", size=0.3) +
      scale_fill_gradient2("Alpha", low = "orange", mid = "white",
                           high = "green4", midpoint = 0, space = "Lab",
                           na.value = "grey40", guide = "colourbar", limits=aplh.r) +
      geom_sf() +
      coord_sf(datum = NA) +  
      geom_sf(data = plot.r, size = 4, aes(geometry=geometry, colour=alph)) +
      scale_colour_gradient2("Alpha", low = "orange", mid = "white",
                           high = "green4", midpoint = 0, space = "Lab",
                           na.value = "grey40", guide = "colourbar", limits=aplh.r) +
      geom_sf(data = qq, fill = NA) +
      theme_map() + theme(panel.grid.major=element_line(colour="transparent"))    
  
  # print cell maps
  ggsave(paste(out.dir, sp.list[m], "AlphaPlot.jpeg"), plot=alph_p1)
  
  
} #end loop

##Overlap grid and routes with significant negative trends. 
#load route outputs

route.data<-read.csv("output/QC/Route10yrList.csv")
route.list<-unique(route.data$RouteIdentifier)
plot.r<-read.csv("output/QC/PosteriorSummaryRoute.csv")
plot.r<-plot.r %>% drop_na()
plot.r<-plot.r %>% select(RouteIdentifier, alph, tau, lower_ci, upper_ci, CommonName)
plot.r<-plot.r %>% filter(RouteIdentifier %in% route.list)
plot.r<-plot.r %>% filter(CommonName !="Boreal Owl")

plot.r$tau_sig <- ifelse((plot.r$lower_ci < 0 & plot.r$upper_ci > 0),
                                                      plot.r$tau_sig <- 0,
                                                      plot.r$tau_sig <- 1)

plot.r<-plot.r %>% select(RouteIdentifier, tau, tau_sig, CommonName) 

#load cell outputs
pot<-read.csv("output/QC/PosteriorSummary.csv")
pot<-pot %>% select(alph, tau, id, tau_sig, taxa_code)
pot<-pot %>% drop_na() %>% dplyr::rename("CommonName"="taxa_code")
plot.g<-pot %>% mutate(tau_sig = ifelse(tau_sig==0, 0, 1))

plot.g <- merge(Grid, plot.g, by="id", all=F)
plot.g <- as(plot.g, "sf") 

# make route level maps ---------------------------------------------------------

sp.list<-unique(plot.r$CommonName)

for(m in 1:length(sp.list)) {
  #m<-1 #for testing each species
  
  plot.r2 <-NULL 
  plot.r2 <- plot.r %>% filter(CommonName %in% sp.list[m])
  plot.r2<-left_join(plot.r2, xy, by="RouteIdentifier")
  plot.r2<-plot.r2 %>% filter(tau_sig>0)
 
  plot.g2 <-NULL 
  plot.g2 <- plot.g %>% filter(CommonName %in% sp.list[m])
  plot.g2<-plot.g2 %>% filter(tau_sig>0)
  
  # map tau grid
  tau_p1 <- ggplot() +
    geom_sf(data=Grid, aes(), size=0.3) +
    geom_sf(data=plot.g2, aes(fill="red4"), col="gray40", size=0.3) +
    geom_sf() +
    coord_sf(datum = NA) +  
    geom_sf(data = plot.r2, size = 4, aes(geometry=geometry), colour="black") +
    geom_sf(data = qq, fill = NA) +
    theme_map() + theme(panel.grid.major=element_line(colour="transparent"))
  
  # print cell maps
  ggsave(paste(out.dir, sp.list[m], "TauSig_GridRoute.jpeg"), plot=tau_p1)
  
 
} #end loop


