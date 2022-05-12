
irrad_one_day = subset(irradiance, posix_date == ek[1, "posix_date"])
plot(irrad_one_day$date_time, irrad_one_day$Epar)
abline(h = 116)
