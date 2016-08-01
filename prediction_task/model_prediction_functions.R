library(dplyr)

get_training_data = function(df)
{
  
  train =  df %>% 
    group_by(hack_license) %>% 
    arrange(start) %>% 
    mutate(row_num = row_number()) %>% 
    filter(row_num %in% sample(1:max(row_num),round(0.8*max(row_num))))
  
}

get_test_data = function(df, train)
{
  test = anti_join(df, train) 
  
}

train_model_classification = function(train, formula)
{
formula = as.formula(formula)
X = sparse.model.matrix(formula, train) 
Y = train$efficiency_category
glm_efficiency <- glmnet(X, Y, family = "binomial", lambda =0)

}

test_model_classification = function(formula, test, model)
{
  formula = as.formula(formula)
  xtest = sparse.model.matrix(formula, test)
  test$predicted <- predict(model, newx = xtest, type = "response")
  test
}

plot_roc_auc = function(test)
{
  pred <- prediction(test$predicted, test$efficiency_category)
  perf = performance(pred, measure = 'tpr', x.measure = 'fpr')
  plot(perf, main = paste ("ROC CURVE\nAUC: ", performance(pred, 'auc')@y.values))
}

plot_calibration = function(test)
{
  calibration = test %>% 
  mutate(predicted_bin = round(predicted/0.1)*0.1) %>%
  group_by(predicted_bin) %>%
  summarize(actual_freq = mean(efficiency_category))
  
  ggplot(calibration, aes(x=predicted_bin, y=actual_freq)) + 
    geom_point() + 
    geom_abline(slope =1, intercept =0) +
    ylim(0,1)
  
}

extract_hack_licenses_coef = function(model)
{
  hl_coef <- model %>% tidy() %>% 
    extract(term, c("hack_license","rest"), "(hack_license)(.*)") %>% 
    filter(hack_license == "hack_license") 
  
  hl_coef
  
}

