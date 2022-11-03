# These are collapsed rapid light curves where each row is a unique sample and 
# the information is the output from Phytotools package
# The ek_est column is what we use to determine saturation

library(dplyr)
library(ggplot2)

ek = read.csv("./data_ek/hyp_ulva_all_runs_ek_alpha_normalized.csv")
# Make  sure the date is loaded as date
ek$posix_date <- as.POSIXct(ek$Date, format = "%Y-%m-%d")
ek$Run <- as.factor(ek$Run)
ek$RLC.Order <- as.factor(ek$RLC.Order)
ek$RLC.Day <- as.factor(ek$RLC.Day)
ek$Treatment <- as.factor(ek$Treatment)
ek$Plant.ID <- as.factor(ek$Plant.ID)
ek$Lanai.Side <- as.factor(ek$Lanai.Side)
ek$Specimen.ID <- as.factor(ek$Specimen.ID)
ek$Species <- as.factor(ek$Species)

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

# - we want something like this
# Specimen.ID Run Lanai.Side ek.day1 ek.1.day9 ek.delta    Treatment Temp...C. RLC.Order Plant.ID
# hm1-1       2   ewa        133.1   73.2      -0.4500376

# working_set <- subset(ek, Specimen.ID == 'Ul2-1' & Run == 6 & Lanai.Side == 'ewa')

specimen_ids <- unique(ek$Specimen.ID)
runs <- unique(ek$Run)
lanai_sides <- unique(ek$Lanai.Side)
species <- unique(ek$Species)


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

for (specimen_id in specimen_ids) {
  for (run in runs) {
    for (lanai_side in lanai_sides) {
      for (species in species) {
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
    }
  }
}
print('done')
write.csv(delta_esubk_result, "./output/delta_ek.csv")


