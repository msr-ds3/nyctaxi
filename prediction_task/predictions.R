load("../Rdata/shifts_design_matrix.Rdata")
library(lubridate)
library(dplyr)
library(ggplot2)
library(glmnet)
library(lubridate)
library(broom)
library(tidyr)

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
  summarize(num_shifts = max(shift_num)) %>%
  filter(num_shifts >= min_num_shifts) %>%
  select(hack_license)
# join data frames to only select shifts with drivers who meet threshold
valid_shifts =
  left_join(valid_drivers, shifts_design_matrix)

##############################################
#Splitting the data into train and test frames
##############################################
set.seed(42)
indexes <- sample(1:nrow(valid_shifts), size=0.2*nrow(valid_shifts))
TTtest=valid_shifts[indexes, ]
TTtrain=valid_shifts[-indexes, ]
################################################
#Predicting efficiency category
################################################
Model1 <- 
  as.formula(as.factor(efficiency_category) ~ as.factor(hack_license) + 
               is_week_end*start_hour) 
X = sparse.model.matrix(Model1, TTtrain)
Y = TTtrain$efficiency_category
glm.model_1 <- glmnet(X, Y, family = "binomial")
plot(glm.model_1)
coef(glm.model_1) %>% head()

plot_data <-glm.model_1 %>% 
  tidy() %>% 
  extract(term, c("hack_license","rest"), "(hack_license)(.*)") %>% 
  filter(hack_license == "hack_license") 
ggplot(plot_data, aes(x = estimate)) + geom_histogram(binwidth = 1) + 
  ylim(0, 2500000)
ggsave("../figures/coef_distribution_of_hack_licenses.png")

TTtest$predicted <- predict(glm.model_1, TTtest)

RMSE <- sqrt(mean((TTtest$effiency_category-TTtest$predicted)^2))

################################################################################

#########################
#Randomizing valid shifts
#########################

hack_licenses_shuffled <-sample(valid_shifts$hack_license, nrow(valid_shifts))
random_shifts = valid_shifts
random_shifts$hack_license = hack_licenses_shuffled

##############################################
#Splitting the data into train and test frames
##############################################
set.seed(42)
indexes <- sample(1:nrow(random_shifts), size=0.2*nrow(random_shifts))
TTtest=random_shifts[indexes, ]
TTtrain=random_shifts[-indexes, ]

################################################
#Predicting efficiency using efficiency category
################################################
Model1 <- 
  as.formula(as.factor(efficiency_category) ~ hack_license + 
               is_week_end*start_hour) 
X = sparse.model.matrix(Model1, TTtrain)
Y = TTtrain$efficiency_category
glm.model_1 <- glmnet(X, Y, family = "binomial")
plot(glm.model_1)


plot_data <-glm.model_1 %>% tidy() %>% 
  extract(term, c("hack_license","rest"), "(hack_license)(.*)") %>% 
  filter(hack_license == "hack_license") 
ggplot(plot_data, aes(x = estimate)) + geom_histogram(binwidth = 1)
ggsave("../figures/coef_distribution_of_hack_licenses_shuffled.png")
