load("../Rdata/shifts_design_matrix.Rdata")
library(lubridate)
library(dplyr)
library(ggplot2)
library(glmnet)
library(lubridate)
library(broom)
library(tidyr)
library(caret)
library(ROCR)
shifts_design_matrix <- shifts_design_matrix %>% mutate(t_ave = ((tmin+tmax)/2))
############################################
#Added a new column for efficiency_category
############################################
med = median(shifts_design_matrix$efficiency)

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


###############################################
#Splitting the data into train and test frames# 
###############################################

# make sure we only test on drivers we have trained on
# solution 1 - join data frames with intersection of unique hack licenses
#set.seed(42)
#indexes <- sample(1:nrow(valid_shifts), size=0.2*nrow(valid_shifts))
#test=valid_shifts[indexes, ]
#train=valid_shifts[-indexes, ]

#unique_hl_train = train %>%
#select(hack_license) %>%
#distinct()
#test = inner_join(unique_hl_train, test)

#unique_hl_test = test %>%
#select(hack_license) %>%
#distinct()
#train = inner_join(train, unique_hl_test)

# solution 2 - group by driver, arrange by time, and split each driver's shifts into training/test 
train = valid_shifts %>% 
  group_by(hack_license) %>% 
  arrange(start) %>% 
  mutate(row_num = row_number()) %>% 
  filter(row_num <= round(0.8*max(row_num)))

test = anti_join(valid_shifts, train) 


###############################################
#Creating model to predict efficiency category#
###############################################
formula <- 
  as.formula(as.factor(efficiency_category) ~  hack_license + as.factor(is_week_end)*as.factor(start_hour) + tmin + prcp)
X = sparse.model.matrix(formula, train) 
Y = train$efficiency_category
glm_efficiency <- glmnet(X, Y, family = "binomial", lambda =0)


# PREDICTION
xtest = sparse.model.matrix(formula, test)
test$predicted <- predict(glm_efficiency, newx = xtest, type = "response")

# Assessing prediction - AUC, ROC, Accuracy
RMSE <- sqrt(mean((test$efficiency_category-test$predicted)^2))

# Setting threshold of 0.5 to classify predictions
test <- test %>% 
  mutate(efficiency_category_predicted = ifelse(predicted > 0.5, 1, 0))
test_confusion_matrix = 
  confusionMatrix(test$efficiency_category_predicted, test$efficiency_category)


# separating `hack_license` label from its value and plotting distribution of coef
plot_data <-glm_efficiency %>% 
  tidy() %>% 
  extract(term, c("hack_license","rest"), "(hack_license)(.*)") %>% 
  filter(hack_license == "hack_license") 

ggplot(plot_data, aes(x = estimate)) + geom_histogram(binwidth = 0.1)
#ggsave("../figures/coef_distribution_of_hack_licenses.png")

#######################
#Plotting the ROC curve
#######################
pred <- prediction(test$predicted, test$efficiency_category)
perf = performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf)
performance(pred, 'auc') 
sd(plot_data$estimate)
# Area = 0.7767 for features: hack_license, is_week_end*start_hour
# Area = 0.7743 for features: hack_license, is_week_end*start_hour, tmin, tmax, prcp
# Area = 0.773 for features: hack_license, is_week_end*start_hour, t_ave, prcp
# Area = 0.77327 for features: hack_license, is_week_end*start_hour, t_ave*prcp
# Area = 0.769 for features: hack_license, is_week_end*start_hour*t_ave*prcp
# Area = 0.7747 for features : hack_license + is_week_end*start_hour + tmin + prcp + shift_type
# Area = 0.777, for features: hack_license + is_week_end*start_hour + prcp + airport_pct + shift_type
# Area = 0.78, for features: hack_license + is_week_end*start_hour + airport_pct + pickups_in_man_pct + shift_type
# Area = 0.5768 for features is_week_end*start_hour + t_ave + prcp

####################################################
#Repeat modeling/predicting for shuffled data frame#
####################################################

hack_licenses_shuffled <-sample(valid_shifts$hack_license, nrow(valid_shifts))
random_shifts = valid_shifts
random_shifts$hack_license = hack_licenses_shuffled

##############################################
#Splitting the data into train and test frames
##############################################

# make sure we only test on drivers we have trained on
# solution 1 - join data frames with intersection of unique hack licenses
set.seed(42)
indexes <- sample(1:nrow(random_shifts), size=0.2*nrow(random_shifts))
test=random_shifts[indexes, ]
train=random_shifts[-indexes, ]

unique_hl_train = train %>%
  select(hack_license) %>%
  distinct()
test = inner_join(unique_hl_train, test)

unique_hl_test = test %>%
  select(hack_license) %>%
  distinct()
train = inner_join(train, unique_hl_test)

# solution 2 - group by driver, arrange by time, and split each driver's shifts into training/test 
train = random_shifts %>% 
  group_by(hack_license) %>% 
  arrange(start) %>% 
  mutate(row_num = row_number()) %>% 
  filter(row_num <= round(0.8*max(row_num)))

test = anti_join(random_shifts, train) 

###############################################
#Creating model to predict efficiency category#
###############################################
formula <- 
  as.formula(as.factor(efficiency_category) ~ hack_license + 
               is_week_end*start_hour) 
X = sparse.model.matrix(formula, train)
Y = train$efficiency_category
glm_efficiency <- glmnet(X, Y, family = "binomial", lambda = 0)


# Prediction
xtest = sparse.model.matrix(formula, test)
test$predicted <- predict(glm_efficiency, newx = xtest, type = "response")

# Assessing prediction - AUC, ROC, Accuracy
RMSE <- sqrt(mean((test$efficiency_category-test$predicted)^2))

# separating `hack_license` label from its value and plotting distribution of coef
plot_data <-glm_efficiency %>% tidy() %>% 
  extract(term, c("hack_license","rest"), "(hack_license)(.*)") %>% 
  filter(hack_license == "hack_license") 
ggplot(plot_data, aes(x = estimate)) + geom_histogram(binwidth = 0.1)
sd(plot_data$estimate)
ggsave("../figures/coef_distribution_of_hack_licenses_shuffled.png")

###############################################################################
#End of shuffled data##########################################################
###############################################################################
