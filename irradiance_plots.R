#this script will import and plot the irradiance data

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(scales)
library(fpp2)
library(zoo)


# execute the R program that populates the irradiance data frame
source("./populate_irradiance.R")

#irradiance measurements similar on both sides of lanai, merging the two sides makes smaller dataframe
irradiance_2021_ewa <- irradiance[irradiance$posix_date >= "2021-09-20" 
                              & irradiance$posix_date <= "2021-11-12" 
                              & irradiance$Lanai.Side == "ewa", ]
irradiance_2021_dia <- irradiance[irradiance$posix_date >= "2021-10-21"
                                  & irradiance$posix_date <= "2021-10-29"
                                  & irradiance$Lanai.Side == "diamond", ]
irradiance_2021 <- rbind(irradiance_2021_ewa, irradiance_2021_dia)
irradiance_2022 <- irradiance[irradiance$posix_date > "2022-02-10"
                              & irradiance$posix_date < "2022-10-23", ]

#2021
#moving average to compress data into every 10 minutes rather than every minute
i2021 <- irradiance_2021 %>% 
  select(date_time, Epar) %>%
  mutate(epar_ma10 = rollmean(Epar, k = 10, fill = Epar))
#keep only the values for every 10 minutes
i2021 <- i2021[substr(i2021$date_time, 16, 16) == "0", ]

i2021$run <- NA
i2021$ulv_ek_mean <- NA
i2021$hyp_ek_mean <- NA

i2021[i2021$date_time >= "2021-09-20" & i2021$date_time <= "2021-09-29", ]$run <- "1"
i2021[i2021$date_time >= "2021-09-20" & i2021$date_time < "2021-09-26", ]$ulv_ek_mean <- 93.4
      i2021[i2021$date_time >= "2021-09-26" & i2021$date_time <= "2021-09-29", ]$ulv_ek_mean <- 82.4
i2021[i2021$date_time >= "2021-09-20" & i2021$date_time < "2021-09-26", ]$hyp_ek_mean <- 132
     i2021[i2021$date_time >= "2021-09-26" & i2021$date_time <= "2021-09-29", ]$hyp_ek_mean <- 106

i2021[i2021$date_time >= "2021-09-30" & i2021$date_time <= "2021-10-09", ]$run <- "2"
i2021[i2021$date_time >= "2021-09-30" & i2021$date_time <= "2021-10-04", ]$ulv_ek_mean <- 91.7
     i2021[i2021$date_time >= "2021-10-05" & i2021$date_time <= "2021-10-09", ]$ulv_ek_mean <- 74.4
i2021[i2021$date_time >= "2021-09-30" & i2021$date_time <= "2021-10-04", ]$hyp_ek_mean <- 115
     i2021[i2021$date_time >= "2021-10-05" & i2021$date_time <= "2021-10-09", ]$hyp_ek_mean <- 88.9

i2021[i2021$date_time >= "2021-10-11" & i2021$date_time <= "2021-10-20", ]$run <- "3"
i2021[i2021$date_time >= "2021-10-11" & i2021$date_time <= "2021-10-15", ]$ulv_ek_mean <- 89.6
     i2021[i2021$date_time >= "2021-10-16" & i2021$date_time <= "2021-10-20", ]$ulv_ek_mean <- 78.6
i2021[i2021$date_time >= "2021-10-11" & i2021$date_time <= "2021-10-15", ]$hyp_ek_mean <- 174
      i2021[i2021$date_time >= "2021-10-16" & i2021$date_time <= "2021-10-20", ]$hyp_ek_mean <- 75.5

i2021[i2021$date_time >= "2021-10-21" & i2021$date_time < "2021-10-30", ]$run <- "3.5"
i2021[i2021$date_time >= "2021-10-21" & i2021$date_time <= "2021-10-25", ]$ulv_ek_mean <- 75.7
     i2021[i2021$date_time >= "2021-10-26" & i2021$date_time < "2021-10-30", ]$ulv_ek_mean <- 81.1
i2021[i2021$date_time >= "2021-10-21" & i2021$date_time <= "2021-10-25", ]$hyp_ek_mean <- 124
     i2021[i2021$date_time >= "2021-10-26" & i2021$date_time < "2021-10-30", ]$hyp_ek_mean <-71.6
  
i2021[i2021$date_time >= "2021-11-01" & i2021$date_time <= "2021-11-12", ]$run <- "4"
i2021[i2021$date_time >= "2021-11-01" & i2021$date_time <= "2021-11-06", ]$ulv_ek_mean <- 92.4
     i2021[i2021$date_time >= "2021-11-07" & i2021$date_time <= "2021-11-12", ]$ulv_ek_mean <- 76.3
i2021[i2021$date_time >= "2021-11-01" & i2021$date_time <= "2021-11-06", ]$hyp_ek_mean <- 113
     i2021[i2021$date_time >= "2021-11-07" & i2021$date_time <= "2021-11-12", ]$hyp_ek_mean <- 77.3

i2021$run <- as.factor(i2021$run)

#2022
i_2022 <- irradiance_2022 %>% 
  select(date_time, Epar) %>%
  mutate(epar_ma10 = rollmean(Epar, k = 10, fill = Epar))

i_2022 <- i_2022[substr(i_2022$date_time, 16, 16) == "0", ]

i_2022$run <- NA
i_2022$ulv_ek_mean <- NA
i_2022$hyp_ek_mean <- NA
i_2022[i_2022$date_time >= "2022-02-11" & i_2022$date_time <= "2022-02-19", ]$run <- "6"
i_2022[i_2022$date_time >= "2022-02-11" & i_2022$date_time < "2022-02-16", ]$ulv_ek_mean <- 89.6
     i_2022[i_2022$date_time >= "2022-02-16" & i_2022$date_time <= "2022-02-19", ]$ulv_ek_mean <- 48.8

i_2022[i_2022$date_time >= "2022-02-21" & i_2022$date_time <= "2022-03-01", ]$run <- "6.5"
i_2022[i_2022$date_time >= "2022-02-21" & i_2022$date_time < "2022-02-26", ]$ulv_ek_mean <- 65.6
     i_2022[i_2022$date_time >= "2022-02-26" & i_2022$date_time <= "2022-03-01", ]$ulv_ek_mean <- 43.4

i_2022[i_2022$date_time >= "2022-04-21" & i_2022$date_time <= "2022-04-29", ]$run <- "7"
i_2022[i_2022$date_time >= "2022-04-21" & i_2022$date_time < "2022-04-26", ]$hyp_ek_mean <- 122
      i_2022[i_2022$date_time >= "2022-04-26" & i_2022$date_time <= "2022-04-29", ]$hyp_ek_mean <- 120

i_2022[i_2022$date_time >= "2022-10-04" & i_2022$date_time <= "2022-10-12", ]$run <- "8"
i_2022[i_2022$date_time >= "2022-10-04" & i_2022$date_time < "2022-10-08", ]$hyp_ek_mean <- 70.8
  i_2022[i_2022$date_time >= "2022-10-08" & i_2022$date_time <= "2022-10-12", ]$hyp_ek_mean <- 85.8

i_2022[i_2022$date_time >= "2022-10-13" & i_2022$date_time <= "2022-10-22", ]$run <- "9"  
i_2022[i_2022$date_time >= "2022-10-13" & i_2022$date_time < "2022-10-18", ]$ulv_ek_mean <- 96.4
    i_2022[i_2022$date_time >= "2022-10-18" & i_2022$date_time <= "2022-10-22", ]$ulv_ek_mean <- 86.2
i_2022[i_2022$date_time >= "2022-10-13" & i_2022$date_time < "2022-10-18", ]$hyp_ek_mean <- 66.8
    i_2022[i_2022$date_time >= "2022-10-18" & i_2022$date_time <= "2022-10-22", ]$hyp_ek_mean <- 53.8
i_2022$run <- as.factor(i_2022$run)

#plot that takes either the max irradiance for the day or the sum total as a continuous line for the period
#ggplot(aggregate(i2021$Epar, by=list(i2021$date_time), FUN=max), aes(x=Group.1, y=x)) + geom_line()

#one_day <- irradiance[irradiance$date_time = "2022-02-12", ]
#irrad_plot <- ggplot(one_day, aes(x=date_time, y=Epar)) +
  geom_line() + xlab("2021") 
irrad_plot

i2021 %>% ggplot() +
  geom_line(aes(x=date_time, y=epar_ma10), alpha = 1, color = "black", linetype = 1, size = 0.3) + 
  xlab("All Runs 2021") + 
  geom_line(aes(x=date_time, y=ulv_ek_mean), alpha= 1, color = "#196F3D", linetype = 1) +
  geom_line(aes(x=date_time, y=hyp_ek_mean), alpha=1, color = "#7B241C", linetype = 1) +
  ylim(0, 650) +
  ylab("Epar (μmols photons m-2 s-1)") +
  scale_x_datetime(date_breaks = "5 day") +
  theme_bw()

i_2022[i_2022$run == "6" | i_2022$run == "6.5", ] %>% ggplot() +
  geom_line(aes(x=date_time, y=epar_ma10), alpha = 1, color = "black", linetype = 1, size = 0.3) +
  xlab("Feb 2022") + 
  geom_line(aes(x=date_time, y=ulv_ek_mean), alpha= 1, color = "#196F3D", linetype = 1) +
  geom_line(aes(x=date_time, y=hyp_ek_mean), alpha=1, color = "#7B241C", linetype = 1) +
  ylim(0, 650) +
  ylab("Epar (μmols photons m-2 s-1)") +
  scale_x_datetime(date_breaks = "5 day") +
  theme_bw()

i_2022[i_2022$run == "7", ] %>% ggplot() +
  geom_line(aes(x=date_time, y=epar_ma10), alpha = 1, color = "black", linetype = 1, size = 0.3) +
  xlab("April 2022") + 
  geom_line(aes(x=date_time, y=ulv_ek_mean), alpha= 1, color = "#196F3D", linetype = 1) +
  geom_line(aes(x=date_time, y=hyp_ek_mean), alpha=1, color = "#7B241C", linetype = 1) +
  ylim(0, 650) +
  ylab("Epar (μmols photons m-2 s-1)") +
  scale_x_datetime(date_breaks = "5 day") +
  theme_bw()

i_2022[i_2022$run == "8" | i_2022$run == "9", ] %>% ggplot() +
  geom_line(aes(x=date_time, y=epar_ma10), alpha = 1, color = "black", linetype = 1, size = 0.3) +
  xlab("Oct 2022") + 
  geom_line(aes(x=date_time, y=ulv_ek_mean), alpha= 1, color = "#196F3D", linetype = 1) +
  geom_line(aes(x=date_time, y=hyp_ek_mean), alpha=1, color = "#7B241C", linetype = 1) +
  ylim(0, 650) +
  ylab("Epar (μmols photons m-2 s-1)") +
  scale_x_datetime(date_breaks = "5 day") +
  theme_bw()


irrad_plot <- ggplot(i2021, aes(x=date_time, y=Epar)) +
  geom_line() + xlab("2021") 
irrad_plot
