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

formula_class = formula2_class

train = get_training_data(valid_shifts)
test = get_test_data(valid_shifts, train)

classification_model = train_model_classification(train, formula_class)
test = test_model_classification(formula_class, test, classification_model)

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
hack_licenses_coef <- extract_hack_licenses_coef(classification_model)
  

ggplot(hack_licenses_coef, aes(x = estimate)) + geom_histogram(binwidth = 0.1)
ggsave("../figures/distribution_of_hl_wkend_str_hr.png")
sd(hack_licenses_coef$estimate)


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

classification_model = train_model_classification(train, formula_class)
test = test_model_classification(formula_class, test, classification_model)

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
hack_licenses_coef <- extract_hack_licenses_coef(classification_model)
ggplot(hack_licenses_coef, aes(x = estimate)) + geom_histogram(binwidth = 0.1)
sd(hack_licenses_coef$estimate)
ggsave("../figures/coef_distribution_of_hack_licenses_shuffled.png")


###############################
######### REGRESSION ##########
###############################

formula_reg = formula5_reg

train = get_training_data(valid_shifts)
test = get_test_data(valid_shifts, train)


X = sparse.model.matrix(formula_reg, train)
Y = train$efficiency
regression_model <- glmnet(X, Y, lambda = 0)
#save(regression_model, file = "../Rdata/regression_model.Rdata")

# PREDICTION
xtest = sparse.model.matrix(formula_reg, test)
test$predicted <- predict(regression_model, newx = xtest, type = "response")

# Assessing prediction - RMSE
RMSE <- sqrt(mean((test$efficiency-test$predicted)^2))

# separating `hack_license` label from its value and plotting distribution of coef
hack_licenses_coef <- extract_hack_licenses_coef(regression_model) 
  

ggplot(hack_licenses_coef, aes(x = estimate)) + geom_histogram(binwidth = 0.1)
#ggsave("../figures/coef_distribution_of_model_without_hack_license.png")

sd(hack_licenses_coef$estimate)

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

X = sparse.model.matrix(formula_reg, train)
Y = train$efficiency
regression_model <- glmnet(X, Y, lambda = 0)


# PREDICTION
xtest = sparse.model.matrix(formula_reg, test)
test$predicted <- predict(regression_model, newx = xtest, type = "response")

# Assessing prediction - RMSE
RMSE <- sqrt(mean((test$efficiency-test$predicted)^2))


# separating `hack_license` label from its value and plotting distribution of coef
hack_licenses_coef <- extract_hack_licenses_coef(regression_model)
ggplot(hack_licenses_coef, aes(x = estimate)) + geom_histogram(binwidth = 0.1)
#ggsave("../figures/coef_distribution_of_model_without_hack_license.png")

sd(hack_licenses_coef$estimate)


################################################################
# REGRESSION FOR FORMULA WITH ALL PICKUP/DROPOFF NEIGHBORHOODS #
################################################################
rm(list = setdiff(ls(), lsf.str())) # removes all objects (not functions!)
load("../Rdata/shifts_design_matrix_nbhd.Rdata")

theme_set(theme_bw())

min_num_shifts = 15
valid_drivers =
  shifts_design_matrix_nbhd %>%
  group_by(hack_license) %>%
  summarize(num_shifts = n()) %>% 
  filter(num_shifts >= min_num_shifts) %>%
  select(hack_license)
# join data frames to only select shifts with drivers who meet threshold
valid_shifts =
  left_join(valid_drivers, shifts_design_matrix_nbhd)

# colnames(shifts_design_matrix_nbhd)
# we want: 1, 9, 12, 38, 42, 47, 48, 49:559
# we also include 3 (start) so we can arrange by start time and then randomize
nhbd_features_df  = valid_shifts[, c(1, 3, 9, 12, 38, 42, 47, 48, 49:559)]
rm(shifts_design_matrix_nbhd, valid_shifts)
backup = nhbd_features_df

names(nhbd_features_df) = gsub(" ", "_", names(nhbd_features_df))
names(nhbd_features_df) = gsub("-", "_", names(nhbd_features_df))
names(nhbd_features_df) = gsub(",", "_", names(nhbd_features_df))
names(nhbd_features_df) = gsub("'", "", names(nhbd_features_df))
names(nhbd_features_df) = gsub("\\.", "_", names(nhbd_features_df))

train = get_training_data(nhbd_features_df)
test = get_test_data(nhbd_features_df, train)

formula7_reg = as.formula(efficiency ~ as.factor(is_week_end)*as.factor(start_hour) + .)
formula_reg = formula7_reg

#remove start time b/c we dont need it in our model and row_num to keep the features the same
train = train[, -2] 
train = train[, -519] #index subtracts by 1 since we remove a column above
test = test[, -2]


X = sparse.model.matrix(formula_reg, train)
Y = train$efficiency
regression_model = glmnet(X, Y, lambda = 0)
#save(regression_model, file = "../Rdata/regression_model.Rdata")

# PREDICTION
xtest = sparse.model.matrix(formula_reg, test)
test$predicted <- predict(regression_model, newx = xtest, type = "response")

# Assessing prediction - RMSE
RMSE <- sqrt(mean((test$efficiency-test$predicted)^2))

# separating `hack_license` label from its value and plotting distribution of coef
hack_licenses_coef <- extract_hack_licenses_coef(regression_model) 


ggplot(hack_licenses_coef, aes(x = estimate)) + geom_histogram(binwidth = 0.1)
ggplot(hack_licenses_coef, aes(x = estimate)) + geom_density()
#ggsave("../figures/coef_distribution_of_model_without_hack_license.png")

sd(hack_licenses_coef$estimate)


##################################
# REPEAT FOR SHUFFLED DATA FRAME #
##################################

# Shuffle hack licenses
hack_licenses_shuffled <-sample(nhbd_features_df$hack_license, 
                                nrow(nhbd_features_df))
random_shifts = nhbd_features_df
random_shifts$hack_license = hack_licenses_shuffled


train = get_training_data(random_shifts)
test = get_test_data(random_shifts, train)

formula7_reg = as.formula(efficiency ~ as.factor(is_week_end)*as.factor(start_hour) + .)
formula_reg = formula7_reg

#remove start time b/c we dont need it in our model and row_num to keep the features the same
train = train[, -2] 
train = train[, -519] #index subtracts by 1 since we remove a column above
test = test[, -2]


X = sparse.model.matrix(formula_reg, train)
Y = train$efficiency
regression_model = glmnet(X, Y, lambda = 0)
#save(regression_model, file = "../Rdata/regression_model.Rdata")

# PREDICTION
xtest = sparse.model.matrix(formula_reg, test)
test$predicted <- predict(regression_model, newx = xtest, type = "response")

# Assessing prediction - RMSE
RMSE <- sqrt(mean((test$efficiency-test$predicted)^2))

# separating `hack_license` label from its value and plotting distribution of coef
hack_licenses_coef_shuffled <- extract_hack_licenses_coef(regression_model) 


ggplot(hack_licenses_coef_shuffled, aes(x = estimate)) + geom_density()
#ggsave("../figures/coef_distribution_of_model_without_hack_license.png")

sd(hack_licenses_coef$estimate)

ggplot() + 
  geom_density(data = hack_licenses_coef, 
                        aes(x=estimate, color = "blue")) + 
  geom_density(data = hack_licenses_coef_shuffled, 
               aes(x=estimate, color = "red")) + 
  xlim(-15,15) + 
  scale_color_manual(labels = c("Actual data", "Shuffled data"), values = c("red", "blue"))








