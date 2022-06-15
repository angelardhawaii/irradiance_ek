# These are collapsed rapid light curves where each row is a unique sample and 
# the information is the output from Phytotools package
# The ek_est column is what we use to determine saturation
ek = read.csv("./data_ek/run5-6_ek_alpha.csv")
# Make  sure the date is loaded as date
ek$posix_date <- as.POSIXct(ek$Date, format = "%Y-%m-%d")


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
write.csv(ek, "./output/irrad_ek.csv")
