#v_plots

# read in trend output

#add species English and French name
sp.name<-meta_species_taxonomy()
sp.name<-sp.name %>% select(species_id, english_name, french_name)

trnd <- read.csv("output/NOS_TrendsSlope.csv")
trnd<- left_join(trnd, sp.name, by="species_id")

trnd <- trnd %>% drop_na(results_code)

trnd <- trnd %>% select(species_id, area_code, english_name, french_name, trnd, lower_ci, upper_ci) %>%
  mutate(sp.trnd = paste(english_name, "/", " \n", french_name, "\n ", ": ", round(trnd, digits = 2),  
                                 " (", round(lower_ci, digits = 2), ", ",
                                 round(upper_ci, digits = 2), ")"))
trnd<-trnd %>% select(-trnd, -lower_ci, -upper_ci)

# read in annual index output

index <- read.csv(paste("output/NOS_AnnualIndices.csv"))
index<- left_join(index, sp.name, by="species_id")

index <- index %>%
  filter(!is.na(results_code)) %>% dplyr::select(index, lower_ci, upper_ci, LOESS_index,
                species_code, year, area_code, species_id,  
                english_name, french_name)

plot.dat <- full_join(index, trnd, by = c("area_code", "species_id", "english_name", "french_name"), multiple="all")
plot.dat <-plot.dat %>% filter(area_code %in% c("National", "BRITISH COLUMBIA/YUKON", "ALBERTA", "SASKATCHEWAN", "MANITOBA", "ONTARIO", "QUEBEC", "NOVA SCOTIA", "NEW BRUNSWICK", "PRINCE EDWARD ISLAND"))


  out.plot[[m]] <- ggplot(data = subset(plot.dat, species_code %in% sp.list[i:j]), aes(x = as.numeric(year), y = index, colour = season, shape = season)) +
    facet_wrap(~ sp.trnd, ncol = 2, scales = "free", as.table = TRUE) +
    geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci, group = season, shape = season, width =1), size = 0.4) +
    geom_smooth(aes(ymin = lower_ci, ymax = upper_ci, group = season, colour = season, fill = season, linetype = season), method = "loess",	size = 0.5, alpha = 0.1) + 
    xlab("Year") +
    ylab("Annual Index") +
    #  scale_x_continuous(breaks = seq(from = min.yr.filt, to = max.yr.filt, by = 4)) +
    scale_shape_manual(values = c(1,2)) +
    scale_y_continuous(trans='log10') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position = "none")+
    theme(text=element_text(size=20))+
    theme_classic()
  
  #To plot bt indiex values
  #out.plot[[t]] <- ggplot(data = subset(plot.dat, species_code %in% sp.list[i:j]), aes(x = as.numeric(year), y = index.bt, colour = season, shape = season)) +
  #facet_wrap(~ sp.trnd, ncol = 2, scales = "free", as.table = TRUE) +
  #geom_pointrange(aes(ymin = lower_ci.bt, ymax = upper_ci.bt, group = season, shape = season, width =1), size = 0.4) +
  #geom_smooth(aes(ymin = lower_ci.bt, ymax = upper_ci.bt, group = season, colour = season, fill = season, linetype = season), method = "loess",
  #size = 0.5, alpha = 0.1) + #linetype = "blank", 
  #theme_bw() +
  #xlab("Year") +
  #ylab("Annual Index") +
  #scale_y_continuous(trans="log10") +
  #scale_x_continuous(breaks = seq(from = min.yr.filt, to = max.yr.filt, by = 4)) +
  #scale_shape_manual(values = c(1,2)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #theme(legend.position = "none")
  
  i <- i + 6
  j <- j + 6
}

length(out.plot)
# Plot to PDF file
pdf(paste(out.dir, site, ".IndexPlot.pdf", sep=""),
    height = 10, width = 8, paper = "letter")
try(print(out.plot[[1]], silent=T))
try(print(out.plot[[2]], silent=T))
try(print(out.plot[[3]], silent=T))
try(print(out.plot[[4]], silent=T))
try(print(out.plot[[5]], silent=T))
try(print(out.plot[[6]], silent=T))
try(print(out.plot[[7]], silent=T))
try(print(out.plot[[8]], silent=T))
try(print(out.plot[[9]], silent=T))
try(print(out.plot[[10]], silent=T))
try(print(out.plot[[11]], silent=T))
try(print(out.plot[[12]], silent=T))
try(print(out.plot[[13]], silent=T))
try(print(out.plot[[14]], silent=T))
try(print(out.plot[[15]], silent=T))
try(print(out.plot[[16]], silent=T))
try(print(out.plot[[17]], silent=T))
try(print(out.plot[[18]], silent=T))
try(print(out.plot[[19]], silent=T))
try(print(out.plot[[20]], silent=T))
try(print(out.plot[[21]], silent=T))
try(print(out.plot[[22]], silent=T))
try(print(out.plot[[23]], silent=T))
try(print(out.plot[[24]], silent=T))
try(print(out.plot[[25]], silent=T))
try(print(out.plot[[26]], silent=T))
try(print(out.plot[[27]], silent=T))
try(print(out.plot[[28]], silent=T))
try(print(out.plot[[29]], silent=T))


while(!is.null(dev.list())) dev.off()

