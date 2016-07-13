library(ggplot2)
library(scales)

#call previous script to load base one-week taxi data, and construct necessary data frames
source("one_week_analysis.R")

#Most popular pickup neighborhoods
source_neighbourhood_trips = taxi_clean %>% group_by(pickup_neighborhood) %>% summarize(numtrips = n()) %>% arrange(desc(numtrips))
pickup_best_20 = source_neighbourhood_trips[1:20,]

#View top 20 of the trip-neighborhood distribution data (descending)
pickup_best_20$Label = with(pickup_best_20, reorder(pickup_neighborhood, (-numtrips)))
ggplot(pickup_best_20) + geom_point(aes(x=Label, (-numtrips), y=numtrips)) + ggtitle("Most Popular Pickup Neighborhoods") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + xlab("Neighborhoods") 

#View ALL of the trip-neighborhood distribution data (descending)
source_neighbourhood_trips$Label = with(source_neighbourhood_trips, reorder(pickup_neighborhood, (-numtrips)))
ggplot(source_neighbourhood_trips) + geom_point(aes(x=Label, (-numtrips), y=numtrips)) + ggtitle("All Pickup Neighborhoods") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + xlab("Neighborhoods") + scale_y_log10(label=comma)

#Most popular dropoff trips neighborhood
destination_neighbourhood_trips = taxi_clean %>% group_by(dropoff_neighborhood) %>% summarize(numtrips = n()) %>% arrange(desc(numtrips))
dropoff_best_20 = destination_neighbourhood_trips[1:20,]

dropoff_best_20$Label  <- with(dropoff_best_20, reorder(dropoff_neighborhood, (-numtrips)))
ggplot(dropoff_best_20) + geom_point(aes(x=Label, y=numtrips)) + ggtitle("Most Popular Dropoff Neighborhoods") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + xlab("Dropoff Neighborhoods") 

#View ALL of the trip-neighborhood distribution data (descending)
destination_neighbourhood_trips$Label = with(destination_neighbourhood_trips, reorder(dropoff_neighborhood, (-numtrips)))
ggplot(destination_neighbourhood_trips) + geom_point(aes(x=Label, (-numtrips), y=numtrips)) + ggtitle("All Dropoff Neighborhoods") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + xlab("Neighborhoods") + scale_y_log10(label=comma)


#source-destination groups
neighbourhood_combinations = taxi_clean %>% 
  group_by(pickup_neighborhood, dropoff_neighborhood) %>% 
  summarize(numtrips = n()) %>% 
  ungroup() %>% 
  arrange(desc(numtrips))

neighbourhood_combinations$combination = paste(neighbourhood_combinations$pickup_neighborhood, neighbourhood_combinations$dropoff_neighborhood, sep = "-")
neighbourhood_combinations$Label = with(neighbourhood_combinations, reorder(combination, (-numtrips)))
ggplot(neighbourhood_combinations[1:20,]) + geom_point(aes(x=Label, y=numtrips)) + ggtitle("Top 20 Neighborhood to Neighborhood Trips") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + xlab("Neighborhood to Neighborhood")

#percentage of trips covered in top 20 neighborhood combinations
sum(neighbourhood_combinations[1:20,]$numtrips)/sum(neighbourhood_combinations$numtrips)