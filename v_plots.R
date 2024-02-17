#v_plots

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
