# These are collapsed rapid light curves where each row is a unique sample and 
# the information is the output from Phytotools package
# The ek_est column is what we use to determine saturation

library(dplyr)
library(ggplot2)
library(ggpubr)

ek = read.csv("./data_ek/hyp_ulva_all_runs_ek_alpha_normalized.csv")
# Make  sure the date is loaded as date
ek$posix_date <- as.POSIXct(ek$Date, format = "%Y-%m-%d")
ek$RLC.Order <- as.factor(ek$RLC.Order)
ek$RLC.Day <- as.factor(ek$RLC.Day)
ek$Treatment <- as.character(ek$Treatment)
ek$Plant.ID <- as.factor(ek$Plant.ID)
ek$Run <- as.character(ek$Run)

# These are time series of the irradiance measurements
# The 3rd column is the value of interest (irradiance)
ewa_irradiance_files <- dir("./data_irradiance/ewa", full.names = TRUE)
ewa_irradiance <- do.call(rbind, lapply(ewa_irradiance_files, read.csv))
ewa_irradiance$Lanai.Side <- as.factor("ewa")

dia_irradiance_files <- dir("./data_irradiance/dia", full.names = TRUE)
dia_irradiance <- do.call(rbind, lapply(dia_irradiance_files, read.csv))
dia_irradiance$Lanai.Side <- as.factor("diamond")

irradiance <- rbind(ewa_irradiance, dia_irradiance)
rm(dia_irradiance, ewa_irradiance, dia_irradiance_files, ewa_irradiance_files)

# Loads all the files in the same data frame

# Make sure the date is loaded as date
irradiance$posix_date <- as.POSIXct(irradiance$Date, format = "%m/%d/%y", tz = "")
irradiance$date_time <- as.POSIXct(paste(irradiance$Date, irradiance$Time, sep = " "), format = "%m/%d/%y %H:%M:%S")
# -------------

# algo

minutes_over_epar <- function(obs_posix_date, obs_lanai_side, e_sub_k) {
  # look for irradiance rows when date and Lanai side match
  irrad_on_obs_date <- subset(irradiance, posix_date == obs_posix_date & Lanai.Side == obs_lanai_side)
  # count rows (minutes) when Epar is above 1
  total_minutes <- sum(with(irrad_on_obs_date, Epar >= 1))
  # count rows (minutes) when Epar  is greater then ek_est
  minutes_above_ek <- sum(with(irrad_on_obs_date, Epar >= e_sub_k))
  perc_over_ek = round(minutes_above_ek / total_minutes, 3)
  
  # irradiance rows where Epar is above e_sub_k
  epar_above_ek <- subset(irrad_on_obs_date, Epar >= e_sub_k)
  if (nrow(epar_above_ek) == 0) {
    first_cross_above_ek <- "MISSING"
    last_cross_below_ek <- "MISSING"
  } else {
    first_cross_above_ek <- epar_above_ek[1, "Time"]
    last_cross_below_ek <- epar_above_ek[nrow(epar_above_ek), "Time"]
  }
  # desired output
  # minutes_over_ek, perc_time_over_ek, first_cross_above_ek, last_cross_below_ek
  answer <- list(minutes_above_ek, perc_over_ek, first_cross_above_ek, last_cross_below_ek)
  return(answer)
}

r <- mapply(minutes_over_epar, ek$posix_date, ek$Lanai.Side, ek$ek.1)
ek$irradiance_over_ek <- unlist(r[1, ])
ek$irradiance_over_ek_perc <- unlist(r[2, ])
ek$first_time_over_ek <- unlist(r[3, ])
ek$last_time_over_ek <- unlist(r[4, ])

#dir.create("./output", showWarnings = FALSE)
write.csv(ek, "./output/all_runs_irrad_ek.csv")

# 
# Calculate delta Ek over 9 days
#
columns <- c("specimen_id",
            "run",
            "lanai_side",
            "ek_day1",
            "ek_day5",
            "ek_day9",
            "ek_delta",
            "treatment",
            "temp",
            "rlc_order",
            "plant_id",
            "species"
)
delta_esubk_result <- data.frame(matrix(nrow = 0, ncol = length(columns)) )
colnames(delta_esubk_result) <- columns

unique_grouping <- ek[ , c("Specimen.ID", "Lanai.Side", "Run")] %>% group_by(Specimen.ID, Lanai.Side, Run) %>% distinct()
for (i in 1:nrow(unique_grouping)) {
  specimen_id = unique_grouping[i, ]$Specimen.ID
  run = unique_grouping[i, ]$Run
  lanai_side = unique_grouping[i, ]$Lanai.Side
  working_set <- subset(ek, Specimen.ID == specimen_id & Run == run & Lanai.Side == lanai_side)
  if (nrow(working_set) == 0) {
    print(c("not found", specimen_id, run, lanai_side))
    break
  } 
  d1 <- working_set[1,]
  e_subk_d1 <- working_set[working_set$RLC.Day == 1,]$ek.1
  if (length(e_subk_d1) == 0) {
    e_subk_d1 <- c(NA)
  }
  e_subk_d5 <- working_set[working_set$RLC.Day == 5,]$ek.1
  if (length(e_subk_d5) == 0) {
    e_subk_d5 <- c(NA)
  }
  e_subk_d9 <- working_set[working_set$RLC.Day == 9,]$ek.1
  if (length(e_subk_d9) == 0) {
    e_subk_d9 <- c(NA)
  }
  e_subk_delta = (e_subk_d9 - e_subk_d1) / e_subk_d1
  delta_esubk_result[nrow(delta_esubk_result) + 1, ] <- list(specimen_id,
                                                             run,
                                                             lanai_side,
                                                             e_subk_d1,
                                                             e_subk_d5,
                                                             e_subk_d9,
                                                             e_subk_delta,
                                                             d1$Treatment,
                                                             d1$Temp...C.,
                                                             d1$RLC.Order,
                                                             d1$Plant.ID,
                                                             d1$Species
                                                             )
}

delta_esubk_result$perc_delta <- round(as.numeric(delta_esubk_result$ek_delta * 100), 3) 
write.csv(delta_esubk_result, "./output/delta_ek.csv")

#make subsets for each species of the new dataframe. Remove the individual from Hypnea data that
#had such a low Ft that it was impossible to get a day 9 RLC
hypnea <- subset(delta_esubk_result, species == "hm" & ! is.na(perc_delta))
hypnea$treatment_graph[hypnea$treatment == 0] <- "1) 35ppt/0.5umol"
hypnea$treatment_graph[hypnea$treatment == 1] <- "2) 35ppt/14umol" 
hypnea$treatment_graph[hypnea$treatment == 2] <- "3) 28ppt/27umol" 
hypnea$treatment_graph[hypnea$treatment == 3] <- "5) 18ppt/53umol" 
hypnea$treatment_graph[hypnea$treatment == 4] <- "6) 11ppt/80umol"
hypnea$treatment_graph[hypnea$treatment == 2.5] <- "4) 28ppt/53umol"

ulva <- subset(delta_esubk_result, species == "ul")
ulva$treatment_graph[ulva$treatment == 0] <- "1) 35ppt/0.5umol"
ulva$treatment_graph[ulva$treatment == 1] <- "2) 35ppt/14umol" 
ulva$treatment_graph[ulva$treatment == 2] <- "3) 28ppt/27umol" 
ulva$treatment_graph[ulva$treatment == 3] <- "5) 18ppt/53umol" 
ulva$treatment_graph[ulva$treatment == 4] <- "6) 11ppt/80umol"
ulva$treatment_graph[ulva$treatment == 2.5] <- "4) 28ppt/53umol"

#visualize data
ulva %>% group_by(treatment) %>% summarise_at(vars(perc_delta), list(mean = mean))
ulva %>% group_by(run) %>% summarise_at(vars(perc_delta), list(mean = mean))

ulva %>% ggplot(aes(treatment_graph, perc_delta)) + 
  geom_boxplot(size=0.5) + 
  geom_point(alpha = 0.5, size = 3, aes(color = run), show.legend = FALSE) + 
  labs(x="salinity/nitrate", y= "Ek 8-day change (%)", title= "A", subtitle = "Ulva lactuca") + 
  scale_x_discrete(labels = c("35ppt/0.5umolN", "35ppt/14umolN", "28ppt/27umolN", "28ppt/53umolN", "18ppt/53umolN", "11ppt/80umolN")) + 
  ylim(-100, 120) + stat_mean() + 
  geom_hline(yintercept=0, color = "purple", size = 0.5, alpha = 0.5) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", vjust = -15, hjust = 0.05), plot.subtitle = element_text(face = "italic", vjust = -20, hjust = 0.05))
hypnea %>% group_by(treatment) %>% summarise_at(vars(perc_delta), list(mean = mean))
hypnea %>% group_by(run) %>% summarise_at(vars(perc_delta), list(mean = mean))

hypnea %>% ggplot(aes(treatment_graph, perc_delta)) + 
  geom_boxplot(size=0.5) + 
  geom_point(alpha = 0.5, size = 3, aes(color = run), show.legend = FALSE) + 
  labs(x="salinity/nitrate", y= "Ek 8-day change (%)", title= "B", subtitle = "Hypnea musciformis") + 
  scale_x_discrete(labels = c("35ppt/0.5umolN", "35ppt/14umolN", "28ppt/27umolN", "28ppt/53umolN", "18ppt/53umolN", "11ppt/80umolN")) + 
  ylim(-100, 120) + stat_mean() + 
  geom_hline(yintercept=0, color = "purple", size = 0.5, alpha = 0.5) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", vjust = -15, hjust = 0.05), plot.subtitle = element_text(face = "italic", vjust = -20, hjust = 0.05))
