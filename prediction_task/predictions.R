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


####################################
######### CLASSIFICATION ###########
####################################

train = get_training_data(valid_shifts)
test = get_test_data(valid_shifts, train)
formula <- 
  as.formula(as.factor(efficiency_category) ~ 
               hack_license + as.factor(is_week_end)*as.factor(start_hour))

classification_model = train_model_classification(train, formula)
test = test_model_classification(formula, test, classification_model)

# Classifying predictions with threshold of 0.5
test <- test %>% 
  mutate(efficiency_category_predicted = ifelse(predicted > 0.5, 1, 0))

#ASSESSING PERFORMANCE WITH ACCURACY, ROC CURVE, AUC, CALIBRATION
test_confusion_matrix = 
  confusionMatrix(test$efficiency_category_predicted, test$efficiency_category)
test_confusion_matrix$overall["Accuracy"]
plot_roc_auc(test)
calibration_plot <- plot_calibration(test)
calibration_plot
#savePlot(filename =  paste("calibration_for_hl_wkend_str_hr", type = "png"))

# AUC = 0.616 for features:as.factor(is_week_end)*as.factor(start_hour) , accuracy = 56.74%, sd = NA
# AUC = 0.778 for features:hack_license + as.factor(is_week_end)*as.factor(start_hour), accuracy = 70.67%, sd = 2.302
# AUC = 0.784 for features:hack_license + as.factor(is_week_end)*as.factor(start_hour) + avg_trip_time + avg_trip_distance
# AUC = 0.7849 for features:hack_license + as.factor(is_week_end)*as.factor(start_hour) + avg_trip_time
# AUC = 0.783 for features: hack_license + as.factor(is_week_end)*as.factor(start_hour) + avg_trip_distance

# separating `hack_license` label from its value to plot distribution of coef
plot_data <- classification_model %>% 
  tidy() %>% 
  extract(term, c("hack_license","rest"), "(hack_license)(.*)") %>% 
  filter(hack_license == "hack_license") 

ggplot(plot_data, aes(x = estimate)) + geom_histogram(binwidth = 0.1)
ggsave("../figures/distribution_of_hl_wkend_str_hr.png")
sd(plot_data$estimate)


#####################################################
### REPEAT CLASSIFICATION FOR SHUFFLED DATA FRAME ###
#####################################################

# Shuffle hack licenses
hack_licenses_shuffled <-sample(valid_shifts$hack_license, nrow(valid_shifts))
random_shifts = valid_shifts
random_shifts$hack_license = hack_licenses_shuffled

# train/test and model
train = get_training_data(random_shifts)
test = get_test_data(random_shifts, train)
formula <- 
  as.formula(as.factor(efficiency_category) ~ 
               hack_license + as.factor(is_week_end)*as.factor(start_hour) + avg_trip_time + avg_trip_distance) 

classification_model = train_model_classification(train, formula)
test = test_model_classification(formula, test, classification_model)

# Classifying predictions with threshold of 0.5
test <- test %>% 
  mutate(efficiency_category_predicted = ifelse(predicted > 0.5, 1, 0))


#ASSESSING PERFORMANCE WITH ACCURACY, ROC CURVE, AUC, CALIBRATION
test_confusion_matrix = 
  confusionMatrix(test$efficiency_category_predicted, test$efficiency_category)
test_confusion_matrix$overall["Accuracy"]
plot_roc_auc(test)
calibration_plot <- plot_calibration(test)
calibration_plot

# AUC = 0.566, for features hack_license + as.(is_week_end)*as.factor(start_hour) , accurarcy = 54.55%, 0.523
# AUC = 0.609 for features as.(is_week_end)*as.factor(start_hour), accuracy = 54.55%

# separating `hack_license` label from its value to plot distribution of coef
plot_data <- classification_model %>% tidy() %>% 
  extract(term, c("hack_license","rest"), "(hack_license)(.*)") %>% 
  filter(hack_license == "hack_license") 
ggplot(plot_data, aes(x = estimate)) + geom_histogram(binwidth = 0.1)
sd(plot_data$estimate)
ggsave("../figures/coef_distribution_of_hack_licenses_shuffled.png")


###############################
######### REGRESSION ##########
###############################

train = get_training_data(valid_shifts)
test = get_test_data(valid_shifts, train)

formula <- as.formula(efficiency ~ 
                        hack_license + 
                        as.factor(is_week_end)*as.factor(start_hour)+ 
                        avg_trip_time + 
                        avg_trip_distance)
X = sparse.model.matrix(formula, train)
Y = train$efficiency
regression_model <- glmnet(X, Y, lambda = 0)

# PREDICTION
xtest = sparse.model.matrix(formula, test)
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

# RMSE = 5.60 for features as.factor(is_week_end)*as.factor(start_hour)
# RMSE = 4.76 for features hack_license + as.factor(is_week_end)*as.factor(start_hour)


#################################################
### REPEAT REGRESSION FOR SHUFFLED DATA FRAME ###
#################################################

#Shuffling the hack_licenses
hack_licenses_shuffled <-sample(valid_shifts$hack_license, nrow(valid_shifts))
random_shifts = valid_shifts
random_shifts$hack_license = hack_licenses_shuffled


train = get_training_data(random_shifts)
test = get_test_data(random_shifts, train)
formula <- as.formula(efficiency ~ 
                        hack_license + 
                        as.factor(is_week_end)*as.factor(start_hour)+ 
                        avg_trip_time + 
                        avg_trip_distance)
X = sparse.model.matrix(formula, train)
Y = train$efficiency
regression_model <- glmnet(X, Y, lambda = 0)


# PREDICTION
xtest = sparse.model.matrix(formula, test)
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





