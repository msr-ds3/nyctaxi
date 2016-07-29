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



