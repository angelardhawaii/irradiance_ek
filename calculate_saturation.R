

# These are collapsed rapid light curves where each row is a unique observation
# The ek_est column is what we use to determine saturation
ek = read.csv("./data_ek/run5_hyp_ulv_final_alpha_ek_rounded.csv")
# Make  sure the date is loaded as date
ek$posix_date <- as.POSIXct(ek$Date, format = "%Y-%m-%d")


# These are time series of the irradiance measurements
# The 3rd column is the value of interest (irradiance)
ewa_irradiance_files <- dir("./data_irradiance/ewa", full.names = TRUE)
ewa_irradiance <- do.call(rbind, lapply(ewa_irradiance_files, read.csv))
ewa_irradiance$Lanai.Side <- as.factor("E")

dia_irradiance_files <- dir("./data_irradiance/dia", full.names = TRUE)
dia_irradiance <- do.call(rbind, lapply(dia_irradiance_files, read.csv))
dia_irradiance$Lanai.Side <- as.factor("D")

irradiance <- rbind(ewa_irradiance, dia_irradiance)
rm(dia_irradiance, ewa_irradiance, dia_irradiance_files, ewa_irradiance_files)

# Loads all the files in the same data frame

# Make sure the date is loaded as date
irradiance$posix_date <- as.POSIXct(irradiance$Date, format = "%m/%d/%y", tz = "")
irradiance$date_time <- as.POSIXct(paste(irradiance$Date, irradiance$Time, sep = " "), format = "%m/%d/%y %H:%M:%S")
# -------------

# algo

minutes_over_epar <- function(obs_posix_date, obs_lanai_side, obs_ek_est) {
  # look for irradiance rows when date and Lanai side match
  irrad_on_obs_date <- subset(irradiance, posix_date == obs_posix_date & Lanai.Side == obs_lanai_side)
  # count rows (minutes) when Epar is above 1
  total_minutes <- sum(with(irrad_on_obs_date, Epar >= 1))
  # count rows (minutes) when Epar  is greater then ek_est
  minutes_above_epar <- sum(with(irrad_on_obs_date, Epar >= obs_ek_est))
  perc_over_epar = minutes_above_epar / total_minutes
  return(perc_over_epar)
}

# observation under consideration
ek[1,]
# use the observation date
minutes_over_epar(ek[1, "posix_date"], ek[1, "Lanai.Side"], ek[1, "ek_est"])





