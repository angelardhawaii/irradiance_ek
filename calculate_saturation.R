

# These are collapsed rapid light curves where each row is a unique observation
# The ek_est column is what we use to determine saturation
ek = read.csv("./data_ek/run5_hyp_ulv_final_alpha_ek_rounded.csv")
# Make  sure the date is loaded as date
ek$Date2 <- strptime(ek[,"Date"], "%Y-%m-%d")


# These are time series of the irradiance measurements
# The 3rd column is the value of interest (irradiance)
ewa_irradiance_files <- dir("./data_irradiance/ewa", full.names = TRUE)
ewa_irradiance <- do.call(rbind, lapply(ewa_irradiance_files, read.csv))
ewa_irradiance$Lanai.Side <- as.factor("E")

dia_irradiance_files <- dir("./data_irradiance/dia", full.names = TRUE)
dia_irradiance <- do.call(rbind, lapply(dia_irradiance_files, read.csv))
dia_irradiance$Lanai.Side <- as.factor("D")

irradiance <- rbind(ewa_irradiance, dia_irradiance)


# Loads all the files in the same data frame

# Make sure the date is loaded as date
irradiance$Date2 <- strptime(irradiance[, "Date"], "%m/%d/%y")
irradiance$Time2 <- strptime(irradiance[, "Time"], "%H:%M:%S")
# -------------

# algo
# observation under consideration
ek[1,]
# use the observation date
observation_date <- ek[1, "Date2"]
observation_lanai_side = ek[1, "Lanai.Side"]
observation_ek_est = ek[1, "ek_est"]
# look for irradiance rows when date and Lanai side match and irradiance is greater then ek_est
irradiance_when_saturated <- subset(irradiance, Date2 == observation_date & Lanai.Side == observation_lanai_side & Epar >= observation_ek_est)
# find the min time, which is, when Epar went over est_ek
cross_above <- irradiance_when_saturated[irradiance_when_saturated$Time2 == min(irradiance_when_saturated$Time2), "Time2"]
# find the max time, which is, when Epar went below est_ek
cross_below <- irradiance_when_saturated[irradiance_when_saturated$Time2 == max(irradiance_when_saturated$Time2), "Time2"]
# calculate for how many minutes of the day it was saturated
saturated_minutes = as.numeric(difftime(cross_below, cross_above, units = "mins"))

