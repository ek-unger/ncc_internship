#the file is runnign another example from the prioritizr website, found here: https://prioritizr.net/articles/prioritizr.html

# load packages
library(prioritizrdata)
library(prioritizr)
library(sf)
library(terra)
library(vegan)
library(cluster)

# set seed for reproducibility
set.seed(500)

# load planning unit data
tas_pu <- get_tas_pu() #contains planning units represented as spatial polygons (a sf::sf_sf object) *what is that*

# load feature data
tas_features <- get_tas_features() #discribes the spatial distribution of the features (each layer is a different vegitation community), is a multi-level raster

# print planning unit data
print(tas_pu)

# set costs for existing protected areas to zero (this is so that areas that are already protected do not get factored in to our planning costs)
tas_pu$cost <- tas_pu$cost * !tas_pu$locked_in

# plot map of planning unit costs
plot(st_as_sf(tas_pu[, "cost"]), main = "Planning unit costs")

# plot map of planning unit coverage by protected areas
plot(st_as_sf(tas_pu[, "locked_in"]), main = "Protected area coverage")

# print planning unit data
print(tas_features)

# plot map of the first four vegetation classes
plot(tas_features[[1:4]])

# build problem
p1 <-
  problem(tas_pu, tas_features, cost_column = "cost") %>%
  add_min_set_objective() %>% #we want to minimize the total cost
  add_boundary_penalties(penalty = 0.005) %>% #increases grouping
  add_relative_targets(0.17) %>% #each layer needs to recieve 17% coverage
  add_locked_in_constraints("locked_in") %>% #we are including all the already protected land in this problem
  add_binary_decisions() #units must either be fully selected or fully not selected (binary)

# print problem
print(p1) #*still using the highs solver - will have to figure out how to change this*

# solve problem
s1 <- solve(p1)

# plot map of prioritization
plot(
  st_as_sf(s1[, "solution_1"]),
  main = "Prioritization",
  pal = c("grey90", "darkgreen")
)

# create column with existing protected areas
tas_pu$pa <- round(tas_pu$locked_in)

# calculate feature representation statistics based on existing protected areas
tc_pa <- eval_target_coverage_summary(p1, tas_pu[, "pa"])
print(tc_pa) #shows how much absolute coverage vs absolute target

# calculate  feature representation statistics based on the prioritization
tc_s1 <- eval_target_coverage_summary(p1, s1[, "solution_1"])
print(tc_s1)

# explore representation by existing protected areas
## calculate number of features adequately represented by existing protected
## areas
sum(tc_pa$met)

## summarize representation (values show percent coverage)
summary(tc_pa$relative_held * 100)

## visualize representation  (values show percent coverage)
hist(
  tc_pa$relative_held * 100,
  main = "Feature representation by existing protected areas",
  xlim = c(0, 100),
  xlab = "Percent coverage of features (%)"
)

# explore representation by prioritization
## summarize representation (values show percent coverage)
summary(tc_s1$relative_held * 100)

## calculate number of features adequately represented by the prioritization
sum(tc_s1$met)

## visualize representation  (values show percent coverage)
hist(
  tc_s1$relative_held * 100,
  main = "Feature representation by prioritization",
  xlim = c(0, 100),
  xlab = "Percent coverage of features (%)"
)

# calculate relative importance - running multiple interations with increasing budgests, but locked in areas are set to -1 for easy visualiation
imp_s1 <- eval_rank_importance(p1, s1["solution_1"], n = 10)
print(imp_s1)

# manually set locked in planning units to -1 to help with visualization,
# this way we can easily see the importance scores for the priority areas
imp_s1$rs[tas_pu$locked_in] <- -1

# plot map of importance scores
plot(st_as_sf(imp_s1[, "rs"]), main = "Overall importance")

##stop at portfolios
