load("shifts_design_matrix.Rdata")
source("load_weather.R")
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(plyr)

# White backgrounds on our plots
theme_set(theme_minimal())

##########################################
## Add ymd col
##########################################
shifts_design_matrix <- shifts_design_matrix %>% mutate(ymd= as.Date(start))

########################################
## Join the two data frames:
########################################
shifts_design_matrix_weather <- left_join(shifts_design_matrix, weather, by="ymd")

########################################
# function to define high and low earning
########################################

mean_efficiency <- mean(shifts_design_matrix_weather$efficiency)
sd_efficiency <- sd(shifts_design_matrix_weather$efficiency)
high_earning <- mean_efficiency + sd_efficiency
low_earning <- mean_efficiency - sd_efficiency



high_low_efficiency <- function(efficiency){
   if (efficiency >= high_earning)
     "high"
  else if (efficiency <= low_earning)
    "low"
  else
    "med"
}

high_low_efficiency <- Vectorize(high_low_efficiency)


shifts_design_matrix_weather <- shifts_design_matrix_weather %>%  
mutate(efficiency_category= high_low_efficiency(shifts_design_matrix_weather$efficiency))

#Adding day and night shift period

shift_period = function(time)
{
  if(hour(time) >= 5 && hour(time) < 17)
    1 # for day shift
  else
    0 #for night shift
}
shift_period = Vectorize(shift_period)
shifts_design_matrix <- shifts_design_matrix %>% mutate(shift_type = shift_period(start))
