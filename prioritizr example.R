#this file is working though the example for one the prioritizr website https://prioritizr.net/index.html
# load packages
library(prioritizr)
library(prioritizrdata)
library(terra)

# import planning unit data
wa_pu <- get_wa_pu() #planning unit data, this is of the class SpatRaster. holds gridded spatial (raster) data.
##an aside - explore how to view data of the spatraster class
wa_pu #shows spatial structure and metadata

#show the raw values, with the coordinates
df <- as.data.frame(wa_pu, xy = TRUE)
head(df)

#some individual calls for the metadata
names(wa_pu) #gives layer names
crs(wa_pu) #gives coordinate reference system
ext(wa_pu) #gives spatial extent/bounding box
res(wa_pu) #gives spatial resolution
nlyr(wa_pu) #gives total number of layers

# preview data
print(wa_pu)

# plot data
plot(wa_pu, main = "Costs", axes = FALSE)

# import feature data
wa_features <- get_wa_features() #import conservation features data - has the relative abundance of different bird species


# preview data
print(wa_features)

# plot the first nine features
plot(wa_features[[1:9]], nr = 3, axes = FALSE)

# calculate budget
budget <- terra::global(wa_pu, "sum", na.rm = TRUE)[[1]] * 0.05 #the cost of prioritization will represent a 5% of the total land value in the study area

# create problem
p1 <-
  problem(wa_pu, features = wa_features) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.2) %>% #each feature ideally has 20% of its distribution covered by the land selected
  add_binary_decisions() %>% #each unit is a yes or no
  add_default_solver(gap = 0.1, verbose = FALSE) #the solver must pick a solution within 10% of the optimal solution (near-optimal solution)

# print problem
print(p1)

# solve the problem
s1 <- solve(p1)

# extract the objective
print(attr(s1, "objective"))
##4.2153

# extract time spent solving the problem
print(attr(s1, "runtime"))
##78.91, much longer, around 15.5 times longer than the gurobi solver took in the example

# extract state message from the solver
print(attr(s1, "status"))
##optimal

# plot the solution
plot(s1, main = "Solution", axes = FALSE)

#evaluate the solution
# calculate number of selected planning units by solution
eval_n_summary(p1, s1)

# calculate total cost of solution
eval_cost_summary(p1, s1)

# calculate target coverage for the solution
p1_target_coverage <- eval_target_coverage_summary(p1, s1)
print(p1_target_coverage)

# check percentage of the features that have their target met given the solution
print(mean(p1_target_coverage$met) * 100)

#now, we add the layer of accounting for areas that are already protected, called 'locked in' areas
# import locked in data
wa_locked_in <- get_wa_locked_in()

# print data
print(wa_locked_in)

# plot data
plot(wa_locked_in, main = "Existing protected areas", axes = FALSE)

# create new problem with locked in constraints added to it
p2 <-
  p1 %>%
  add_locked_in_constraints(wa_locked_in) #you cann just add something new to your old problem without redoing the whole thing

# solve the problem
s2 <- solve(p2)

# plot the solution
plot(s2, main = "Solution", axes = FALSE)

#now add areas that are not avalible for protection (called locked out)
# import locked out data
wa_locked_out <- get_wa_locked_out()

# print data
print(wa_locked_out)

# plot data
plot(wa_locked_out, main = "Areas not available for protection", axes = FALSE)

# create new problem with locked out constraints added to it
p3 <-
  p2 %>%
  add_locked_out_constraints(wa_locked_out)

# solve the problem
s3 <- solve(p3)

# plot the solution
plot(s3, main = "Solution", axes = FALSE)

#now, reduce fragmentation (which increases costs and can have negitive effects on ecological conservation outcomes) by introducing a boundary penalty
# create new problem with boundary penalties added to it
p4 <-
  p3 %>%
  add_boundary_penalties(penalty = 0.003, edge_factor = 0.5) #penalty controls how much clumping happens, higher values mean more clumping. edge-factor - at 0.5, allows conservation areas to touch the edge of the map more easily. at 1, forces areas more central because it treats the edge of the map as a fragmented edge.

# solve the problem
s4 <- solve(p4)

# plot the solution
plot(s4, main = "Solution", axes = FALSE)

#figure out which planning units are most important by running the solution iteritively with an increasing budget and seeing which units are selected first
# calculate importance scores
imp <-
  p4 %>%
  eval_rank_importance(s4, n = 5)

# print scores
print(imp)
# set planning units that are locked in to -1 so we can easily
# see importance scores for priority areas
imp <- terra::mask(imp, s4, maskvalues = 0, updatevalue = -1)

# plot the total importance scores
## planning units shown in purple were not selected in solution s4
## planning units shown in blue are less important
## planning units shown in yellow are highly important
## note that locked in planning units are also shown in yellow
plot(imp, axes = FALSE, main = "Importance scores")
