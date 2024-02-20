#Write output tables suitable for SoCB

#Posterior Summary
post_sum2<- as.data.frame(matrix(data = NA, nrow = 1, ncol = 12, byrow = FALSE, dimnames = NULL))
names(post_sum2) <- c("alpha_i", "alph", "alph_ll", "alph_ul", "alph_iw", "tau", "tau_ll", "tau_ul", "tau_iw", "tau_sig", "id", "taxa_code")
write.table(post_sum2, file = paste(out.dir, collection, "PosteriorSummary2.csv", sep=""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

#Output for SoBC import
indices2.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 16, byrow = FALSE,
                                    dimnames = NULL))
names(indices2.csv) <- c("results_code",	"version",	"area_code", "year",	"season",	"period",	 "species_code",	"species_id",	"index",	"stderr",	"stdev",	"upper_ci",	"lower_ci",	"LOESS_index",	"species_name",	"species_sci_name") 
write.table(indices2.csv, file = paste(out.dir, collection,
                                      "NOS_AnnualIndices2",".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


## Create text file for trends 
trends2.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 38, 
                                   byrow = FALSE, dimnames = NULL))
names(trends2.csv) <- c("results_code",	"version",	"area_code",	"species_code",	"species_id",	"season",	"period",	"years",	"year_start",	"year_end",	"trnd",	"index_type",	"upper_ci", "lower_ci", "stderr",	"model_type",	"model_fit",	"percent_change",	"percent_change_low",	"percent_change_high",	"prob_decrease_0",	"prob_decrease_25",	"prob_decrease_30",	"prob_decrease_50",	"prob_increase_0",	"prob_increase_33",	"prob_increase_100",	"confidence",	"precision_num",	"precision_cat",	"coverage_num",	"coverage_cat",	"sample_size",	"prob_LD", "prob_MD", "prob_LC", "prob_MI", "prob_LI")
#Slope Trends
write.table(trends2.csv, file = paste(out.dir, collection, 
                                     "NOS_TrendsSlope2", ".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")
