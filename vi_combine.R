##Combine data outputs into one table for Catherine's uploads

Trends <- list.files(path = "C:/Users/dethier/Documents/ethier-scripts/National-NOS/output/",  # Identify all CSV files
                     pattern = "*TrendsSlope.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
 

#remove row with only NA
n<-nrow(Trends)
Trends<-Trends[2:n,]

write.csv(Trends, "output/AllTrendsNOS.csv")


Indices <- list.files(path = "C:/Users/dethier/Documents/ethier-scripts/National-NOS/output/",  # Identify all CSV files
                      pattern = "*AnnualIndices.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 


m<-nrow(Indices)
Indices<-Indices[2:m,]

write.csv(Indices, "output/AllIndicesNOS.csv")
