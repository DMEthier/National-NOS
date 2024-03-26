#v_plots

# read in trend output

#add species English and French name
sp.name<-meta_species_taxonomy()
sp.name<-sp.name %>% select(species_id, english_name, french_name)

trnd <- read.csv("output/NOS_TrendsSlope.csv")
trnd<- left_join(trnd, sp.name, by="species_id")

trnd <- trnd %>% drop_na(results_code)

trnd <- trnd %>% select(species_id, area_code, english_name, french_name, trnd, lower_ci, upper_ci) %>%
  mutate(sp.trnd = paste(english_name, "/", " \n", french_name, "\n ", round(trnd, digits = 2),  
                                 " (", round(lower_ci, digits = 2), ", ",
                                 round(upper_ci, digits = 2), ")"))
trnd<-trnd %>% select(-trnd, -lower_ci, -upper_ci)

# read in annual index output

index <- read.csv(paste("output/NOS_AnnualIndices.csv"))
index<- left_join(index, sp.name, by="species_id")

index <- index %>%
  filter(!is.na(results_code)) %>% dplyr::select(index, lower_ci, upper_ci, LOESS_index, trend_index, 
                species_code, year, area_code, species_id,
                english_name, french_name)

plot.dat<-NULL
plot.dat <- full_join(index, trnd, by = c("area_code", "species_id", "english_name", "french_name"), multiple="all")

plot.dat <-plot.dat %>% filter(area_code %in% c("Canada")) %>% filter(species_id != 7610)

ggplot(data = plot.dat, aes(x = as.numeric(year), y = index)) +
    facet_wrap(~ sp.trnd, ncol = 2, scales = "free", as.table = TRUE) +
    geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci)) +
    geom_smooth(aes(ymin = lower_ci, ymax = upper_ci), method = "loess", alpha = 0.1) + 
    xlab("Year") +
    ylab("Annual Index") +
    scale_shape_manual(values = c(1,2)) +
    #scale_y_continuous(trans='log10') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position = "none")+
    theme(text=element_text(size=20))+
    theme_classic()
  
