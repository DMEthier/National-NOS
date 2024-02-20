#v_plots

##Combine data outputs into one table for Catherine's uploads

Trends <- list.files(path = "C:/Users/dethier/Documents/ethier-scripts/National-NOS/output/",  # Identify all CSV files
                     pattern = "*TrendsSlope.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data_all 

#remove row with only NA
n<-nrow(Trends)
Trends<-Trends[2:n,]

write.csv(Trends, "output/AllTrendsNOS.csv")


Indices <- list.files(path = "C:/Users/dethier/Documents/ethier-scripts/National-NOS/output/",  # Identify all CSV files
                      pattern = "*AnnualIndices.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data_all

m<-nrow(Indices)
Indices<-Indices[2:m,]

write.csv(Indices, "output/AllIndicesNOS.csv")


##----------------------------------------------------------
# map it
ggplot() +
  geom_sf(data = sp.data, aes(col = count)) +
  geom_sf(data = qq, fill = NA) +
  coord_sf(datum = NA) +
  facet_wrap(~survey_year) +
  scale_color_distiller(palette = "Spectral") +
  theme_bw()  

##----------------------------------------------------------

# plot it 2
#meshmap2<-ggplot() +
#  gg(data = mesh2) +
#  geom_sf(data = site_map, col = "darkgreen", size = 1) +
#  geom_sf(data = qc, fill = NA) +
#  theme_bw() +
#  labs(x = "", y = "")  