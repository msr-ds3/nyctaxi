library(ggplot2)
library(dplyr)

#call previous script to load base one-week taxi data, and construct necessary data frames
source("one_week_analysis.R")

#Most popular pickup neighborhoods
source_neighbourhood_trips = taxi_clean %>% group_by(pickup_neighborhood) %>% summarize(numtrips = n()) %>% arrange(desc(numtrips))
best_20 = source_neighbourhood_trips[1:20,]

#View top 20 of the trip-neighborhood distribution data (descending)
best_20$Label = with(best_20, reorder(pickup_neighborhood, (-numtrips)))
ggplot(best_20) + geom_point(aes(x=Label, (-numtrips), y=numtrips, color = Label)) + ggtitle("Most Popular Pickup Neighborhoods") + theme(axis.text.x=element_text(angle = 90, hjust = 0))

#View ALL of the trip-neighborhood distribution data (descending)
#QUESTION - LOG(NUMTRIPS) OR JUST NUMTRIPS?? 
source_neighbourhood_trips$Label = with(source_neighbourhood_trips, reorder(pickup_neighborhood, (-numtrips)))
ggplot(source_neighbourhood_trips) + geom_point(aes(x=Label, (-numtrips), y=log(numtrips))) + ggtitle("Pickup Neighborhoods") + theme(axis.text.x=element_text(angle = 90, hjust = 0))

#Most popular dropoff trips neighborhood
destination_neighbourhood_trips = taxi_clean %>% group_by(dropoff_neighborhood) %>% summarize(numtrips = n()) %>% arrange(desc(numtrips))
best_20 = destination_neighbourhood_trips[1:20,]

best_20$Label  <- with(best_20, reorder(dropoff_neighborhood, (-numtrips)))
ggplot(best_20) + geom_point(aes(x=Label, y=numtrips)) + ggtitle("Most Popular Dropoff Neighborhoods") + theme(axis.text.x=element_text(angle = 90, hjust = 0))


