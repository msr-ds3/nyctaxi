source("one_week_analysis.R")
source("heatmap_functions.R")

#########################
# pickup heatmap
#########################
# summarize rides by pickup neighborhood
get_map_by_neighborhood(is_source = F)


#########################
# dropoff heatmap
#########################
# summarize trips by dropoff neighborhoods
get_map_by_neighborhood(is_source = T)

##########################################################
# given a neighborhood X, what are destinations?
#########################################################
neighborhood <- "Williamsburg"
get_map_by_neighborhood(neighborhood = neighborhood, is_source = T)

##########################################################
# given a neighborhood X, what are the sources?
#########################################################
get_map_by_neighborhood(neighborhood = "Chelsea", is_source = F)
 