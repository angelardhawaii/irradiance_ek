# Calculate mean supersaturation time by period 

# S
# For runs 1-7
# Period 1 is days 1-3 and uses Ek of day 1 as threshold
# Period 2 is days 4-5 and uses Ek of day 5 as threshold
# Period 3 is days 6-8 and uses Ek of day 9 as threshold

# For runs 8-9 which don't have a day 5 measurement
# Period 1 is days 1-4 and uses Ek of day 1 as threshold
# Period 2 is days 5-8 and uses Ek of day 9 as threshold

# Desired output columns
# specimen_id run lanai_side treatment temp rlc_order plant_id  species supersat_time_p1 supersat_time_p2 supersat_time_p3 supersat_time_total

# execute the R program that populates the ek data frame
source("./populate_ek.R")

# execute the R program that populates the irradiance data frame
source("./populate_irradiance.R")

# Consider the first day of each run
supersat <- ek[ek$RLC.Day == 1, c(
  "Specimen.ID", "Run", "Lanai.Side", "Treatment", "Temp...C.",
  "RLC.Order", "Plant.ID", "Species", "ek.1", "posix_date", "rlc_end_time")]
names(supersat)[11] <- "day1_rlc_time" 

# Populate the column with the hour when the RLC was done on day 9
supersat9 <- ek[ek$RLC.Day == 9, c("Specimen.ID", "Run", "Lanai.Side", "rlc_end_time")]
r <- mapply(function(specimen_id, run, lanai_side) {
  return(supersat9[supersat9$Specimen.ID == specimen_id 
                   & supersat9$Run == run 
                   & supersat9$Lanai.Side == lanai_side, "rlc_end_time"])
}, supersat$Specimen.ID, supersat$Run, supersat$Lanai.Side)
supersat$day9_rlc_time <- unlist(r)


calc_supersat_by_day <- function(day1_date, days_to_consider, day1_rlc_time, day9_rlc_time, Ek, lanai_side) {
  supersat_by_day <- rep(NA, length(days_to_consider))
  n = 0
  for (day in days_to_consider) {
    start_time <- if(day == 0) day1_rlc_time else "00:00:01"
    start <- as.POSIXct(paste(day1_date + 86400 * day, start_time, sep = " "))
    end_time <- if(day == 8) day9_rlc_time else "23:59:59"
    end <- as.POSIXct(paste(day1_date + 86400 * day, end_time, sep = " "))
    n = n + 1
    supersat_by_day[n] = nrow(irradiance[irradiance$date_time > start 
                                        & irradiance$date_time < end 
                                        & irradiance$Lanai.Side == lanai_side
                                        & irradiance$Epar > Ek, ])
    
  }
  return(supersat_by_day)
}
# Test the function above
#calc_supersat_by_day(supersat[1, "posix_date"], 0:2, supersat[1, "day1_rlc_time"], supersat[1, "day9_rlc_time"], supersat[1, "ek.1"], "ewa")


calculate_supersat <- function(posix_date, day1_rlc_time, day9_rlc_time, Ek, run, lanai_side) {
  if (run == 8 | run == 9) {
    supersat_p1 = mean(calc_supersat_by_day(posix_date, 0:3, day1_rlc_time, day9_rlc_time, Ek, lanai_side))
    supersat_p2 = mean(calc_supersat_by_day(posix_date, 4:7, day1_rlc_time, day9_rlc_time, Ek, lanai_side))
    supersat_p3 = NA
    supersat_total = mean(calc_supersat_by_day(posix_date, 0:7, day1_rlc_time, day9_rlc_time, Ek, lanai_side))
  } else {
    supersat_p1 = mean(calc_supersat_by_day(posix_date, 0:2, day1_rlc_time, day9_rlc_time, Ek, lanai_side))
    supersat_p2 = mean(calc_supersat_by_day(posix_date, 3:4, day1_rlc_time, day9_rlc_time, Ek, lanai_side))
    supersat_p3 = mean(calc_supersat_by_day(posix_date, 5:7, day1_rlc_time, day9_rlc_time, Ek, lanai_side))
    supersat_total = mean(calc_supersat_by_day(posix_date, 0:7, day1_rlc_time, day9_rlc_time, Ek, lanai_side))
  }
  return(list(supersat_p1, supersat_p2, supersat_p3, supersat_total))
}

r <- mapply(calculate_supersat, supersat$posix_date, supersat$day1_rlc_time, supersat$day9_rlc_time, supersat$ek.1, supersat$Run, supersat$Lanai.Side)
supersat$supersat_p1 <- unlist(r[1,])
supersat$supersat_p2 <- unlist(r[2,])
supersat$supersat_p3 <- unlist(r[3,])
supersat$supersat_total <- unlist(r[4,])


