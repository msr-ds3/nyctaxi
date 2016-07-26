load("../Rdata/shifts_design_matrix.Rdata")
library(lubridate)
library(dplyr)
library(ggplot2)
library(glmnet)

################################################
#Fitlered out efficiency values greater than 75
################################################
shifts_design_matrix <- shifts_design_matrix %>% filter(efficiency < 75)

##################
#Added is_week_end
##################
is_weekend = function(vec)
{
  col = vector(mode= "numeric", length = length(vec))
  if (wday(vec) ==1 | wday(vec)==7)
  {
    TRUE
  }
  else
  {
    FALSE
  }
}
is_weekend = Vectorize(is_weekend)
shifts_design_matrix$is_week_end = is_weekend(shifts_design_matrix$ymd)
##############################################
#Splitting the data into train and test frames
##############################################
set.seed(42)
indexes <- sample(1:nrow(shifts_design_matrix), size=0.2*nrow(shifts_design_matrix))
TTtest=shifts_design_matrix[indexes, ]
TTtrain=shifts_design_matrix[-indexes, ]

################################################
#Writing the model and making some predictions
###############################################
#Features : ymd
model1 <- lm(efficiency ~ as.factor(ymd), TTtrain)
TTtest$predicted_model1 <- predict(model1, TTtest)
coef(model1)
summary(model1)
#(Intercept)           ymd 
#-994.3611927    0.0643676

#Features: ymd, and shift_type
model2 <- lm(efficiency ~ ymd + as.factor(shift_type), TTtrain)
TTtest$predicted_model2 <- predict(model2, TTtest)
coef(model2)
summary(model2)
#(Intercept)           ymd    shift_type 
#-1.003144e+03  6.493758e-02 -4.372635e-01 

#Features: shift_type
model3 <- lm(efficiency ~ as.factor(shift_type), TTtrain)
TTtest$predicted_model3 <- predict(model3, TTtest)
coef(model3)
#(Intercept)  shift_type 
#29.4767131  -0.4095997 

#Features: prcp
model4 <- lm(efficiency ~ prcp, TTtrain)
TTtest$predicted_model4 <- predict(model4, TTtest)
coef(model4)
#(Intercept)         prcp 
#29.280627923 -0.003064592 

#Features: tmax
model5 <- lm(efficiency ~ tmax, TTtrain)
TTtest$predicted_model5 <- predict(model5, TTtest)
coef(model5)
#(Intercept)        tmax 
#28.73433405  0.01574753 

#Features: total fare
model6<- lm(efficiency ~ total_fare, TTtrain)
TTtest$predicted_model6 <- predict(model6, TTtest)
coef(model6)

#Features : is_weekend
model7 <- lm(efficiency ~ ymd+ is_week_end, TTtrain)
TTtest$predicted_model7 <- predict(model7, TTtest)
coef(model7)
summary(model7)

#Features : Airport_pct
model8 <- lm(efficiency ~ ymd + airport_pct, TTtrain)
TTtest$predicted_model8 <- predict(model8, TTtest)
coef(model8)
summary(model8)

#Using glmnet


######################
#Plotting the results
######################
#model1
ggplot(TTtest) + geom_point(aes(x = ymd, y = efficiency)) + 
  geom_line(aes(x = ymd, y =predicted_model1))
ggsave("../figures/ymd_vs_efficiency_model.png")

#model2
ggplot(TTtest) + geom_point(aes(x = ymd, y = efficiency)) + 
  geom_line(aes(x = ymd, y =predicted_model2, color  =  as.factor(shift_type)))
ggsave("../figures/ymd_and_shift_type_vs_efficiency_model.png")

#model3
ggplot(TTtest) + geom_point(aes(x = as.factor(shift_type), y = efficiency)) + 
  geom_line(aes(x = shift_type, y =predicted_model3))
ggsave("../figures/shift_type_vs_efficiency_model.png")

#model4
ggplot(TTtest) + geom_point(aes(x = prcp, y = efficiency)) + 
  geom_line(aes(x = prcp, y =predicted_model4))

#model5
ggplot(TTtest) + geom_point(aes(x = tmax, y = efficiency)) + 
  geom_line(aes(x = tmax, y =predicted_model5))

#model6
ggplot(TTtest) + geom_point(aes(x = total_fare, y = efficiency)) + 
  geom_line(aes(x = total_fare, y =predicted_model6))

#model7
ggplot(TTtest, aes(x = is_week_end, y = efficiency, fill = is_week_end)) +
                                                       geom_boxplot() 
# model 2
ggplot(TTtest, aes(x = as.factor(shift_type), y = efficiency, fill = as.factor(shift_type))) +
  geom_boxplot() 
ggsave("../figures/shift_type_vs_efficiency_model_box_plot.png")

# model 2
ggplot(TTtest, aes(x = airport_pct, y = efficiency)) +
  geom_point() + geom_line(aes(x = airport_pct, y = efficiency))

