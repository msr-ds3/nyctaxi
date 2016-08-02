load("../Rdata/shifts_design_matrix.Rdata")
library(lubridate)
library(dplyr)
library(ggplot2)
library(glmnet)
library(lubridate)
library(broom)
library(tidyr)
library(ROCR)
library(stargazer)
library(caret)
source("model_prediction_functions.R")

# PRE-DEFINING OUR FORMULAS FOR CLASSIFICATION AND REGRESSION #
formula1_class = as.formula(efficiency_category ~
                              hack_license)
formula2_class = as.formula(efficiency_category ~
                              as.factor(start_hour)*as.factor(is_week_end))
formula3_class = as.formula(efficiency_category ~ 
                              hack_license + 
                              as.factor(start_hour)*as.factor(is_week_end))
formula4_class = as.formula(efficiency_category ~ 
                              hack_license + 
                              as.factor(start_hour)*as.factor(is_week_end) + 
                              avg_trip_time + 
                              avg_trip_distance)
formula5_class = as.formula(efficiency_category ~
                              hack_license + 
                              as.factor(start_hour)*as.factor(is_week_end) + 
                              prcp)
formula6_class = as.formula(efficiency_category ~ 
                              hack_license + 
                              as.factor(start_hour)*as.factor(is_week_end) + 
                              avg_trip_time + 
                              avg_trip_distance + 
                              prcp)

formula1_reg = as.formula(efficiency ~
                            hack_license)
formula2_reg = as.formula(efficiency ~
                            as.factor(start_hour)*as.factor(is_week_end))
formula3_reg = as.formula(efficiency ~ 
                            hack_license + 
                            as.factor(start_hour)*as.factor(is_week_end))
formula4_reg = as.formula(efficiency ~ 
                            hack_license + 
                            as.factor(start_hour)*as.factor(is_week_end) + 
                            avg_trip_time + 
                            avg_trip_distance)
formula5_reg = as.formula(efficiency ~
                            hack_license + 
                            as.factor(start_hour)*as.factor(is_week_end) + 
                            prcp)
formula6_reg = as.formula(efficiency ~ 
                            hack_license + 
                            as.factor(start_hour)*as.factor(is_week_end) + 
                            avg_trip_time + 
                            avg_trip_distance + 
                            prcp)
formula7_reg = as.formula(efficiency ~  hack_license + 
                            as.factor(start_hour)*as.factor(is_week_end) + dropoffs )

############################################
#Added  new columns for efficiency_category#
############################################
med = median(shifts_design_matrix$efficiency)
shifts_design_matrix <- shifts_design_matrix %>% mutate(t_ave = ((tmin+tmax)/2))

# 0 - LOW EFFICIENCY  |   1 - HIGH EFFICIENCY
shifts_design_matrix = shifts_design_matrix %>% 
  mutate(efficiency_category = ifelse(efficiency < med, 0, 1))

###############################################
#REMOVE DRIVERS WHO ARE OUT OF SHIFT THRESHOLD#
###############################################
min_num_shifts = 15
valid_drivers =
  shifts_design_matrix %>%
  group_by(hack_license) %>%
  summarize(num_shifts = n()) %>% 
  filter(num_shifts >= min_num_shifts) %>%
  select(hack_license)
# join data frames to only select shifts with drivers who meet threshold
valid_shifts =
  left_join(valid_drivers, shifts_design_matrix)


###############################
######### REGRESSION ##########
###############################

train = get_training_data(valid_shifts)
test = get_test_data(valid_shifts, train)


X = sparse.model.matrix(formula5_reg, train)
Y = train$efficiency
regression_model <- glmnet(X, Y, lambda = 0)

# PREDICTION
xtest = sparse.model.matrix(formula5_reg, test)
test$predicted <- predict(regression_model, newx = xtest, type = "response")

# Assessing prediction - RMSE
RMSE <- sqrt(mean((test$efficiency-test$predicted)^2))

# separating `hack_license` label from its value and plotting distribution of coef
plot_data <-regression_model %>% 
  tidy() %>% 
  extract(term, c("hack_license","rest"), "(hack_license)(.*)") %>% 
  filter(hack_license == "hack_license") 

ggplot(plot_data, aes(x = estimate)) + geom_histogram(binwidth = 0.1)
#ggsave("../figures/coef_distribution_of_model_without_hack_license.png")

sd(plot_data$estimate)

####Filtering weird drivers#####

weird_drivers <- plot_data %>% filter(estimate < -6.949| estimate > 6.949)

hack_license = "3DBF12F34B6BBE5843E2B25A18D78B76"

ggplot() +
  geom_point(data = valid_shifts, 
               aes(x = start, xend=end, 
                   y = as.factor(hack_license), yend = as.factor(hack_license)),
               size = 2) + 
scale_shape_manual(values = c(4, 1)) +
  scale_x_datetime(date_breaks = "2 hour",labels=date_format("%H"))
  
