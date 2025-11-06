#### set up ####
rm(list=ls()) # clear workspace
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/HerbVar/Null Model") # set working directory

# load packages
library(DescTools) # includes the function for the Gini coefficient
library(ggplot2) # for plotting

#set population parameters
n_plants = 60 # number of plants in the population
n_leaves = 4 # number of leaves per plant
n_pops = 10 # number of populations
n_runs = 10 # number of replicates

# define parameters
params <- list(
  leaf.mean = 100, # mean leaf size for varying at the scale of the leaf
  leaf.dev = 20, # standard deviation for varying at the scale of the leaf
  plant.mean = 100, # mean leaf size for varying at the scale of the plant
  plant.dev = 20, # standard deviation for varying at the scale of the plant
  pop.mean = 100, # mean leaf size for varying at the scale of the population
  pop.dev = 20, # standard deviation for varying at the scale of the population
  dev.mult = 0.2, # multiplier for std. dev with variable mean
  pct.damage = 0.15 # average percentage of leaf area to be removed
)




#### function to set up leaf sizes ####
# the input for this function is a list of parameters and one of the levels of variation in leaf size (see below)
initialize.leaves <- function(prms, leaf.var) {
  # create empty data frame with leaf, plant, and population ids
  leaf.data = expand.grid(leaf.id = rep(1:n_leaves),
                          plant.id = rep(1:n_plants),
                          pop.id = rep(1:n_pops))
  
  ## what should the level of variation in leaf size be? ##
  # [1] constant - all leaves are the same size #
  # [2] leaf - same mean across plants, leaf size distributed normally #
  # [3] plant - uniform size within a plant, chosen from a normal distribution #
  # [4] population - uniform size within a population, chosen from a normal distribution #
  # [5] leaf + plant - each plant has a different mean size #
  # [6] plant + population - each population has a different mean size #
  # [7] leaf + plant + population - each population has a different mean, each plant has a different mean drawn from the population distribution #
  
  ## determine initial leaf sizes
  
  # [1] uniform leaf sizes
  if (leaf.var == 1 || leaf.var == "constant") {
    # set leaf sizes
    mean.size = prms$leaf.mean
    leaf.size = rep(mean.size, nrow(leaf.data))
    
    # add leaf sizes, mean, and standard deviation to data frame
    leaf.data$init.size = leaf.size
    leaf.data$leaf.mean = rep(mean.size, nrow(leaf.data))
    leaf.data$leaf.dev = rep(0, nrow(leaf.data))
  } 
  
  # [2] variation in leaf size within plants
  else if (leaf.var == 2 || leaf.var == "leaf") {
    # set mean and standard deviation in leaf size
    mean.size = prms$leaf.mean
    size.dev = prms$leaf.dev
    
    # assign leaf sizes randomly from a gamma distribution
    leaf.size = ceiling(rgamma(nrow(leaf.data), shape = mean.size ^ 2 / size.dev ^ 2, scale = size.dev ^ 2 / mean.size))
    
    # add leaf sizes, mean, and standard deviation to data frame
    leaf.data$init.size = leaf.size
    leaf.data$leaf.mean = rep(mean.size, nrow(leaf.data))
    leaf.data$leaf.dev = rep(size.dev, nrow(leaf.data))
  } 
  
  # [3] variation in leaf size between plants
  else if (leaf.var == 3 || leaf.var == "plant") {
    # set mean and standard deviation in leaf size
    mean.size = prms$plant.mean
    size.dev = prms$plant.dev
    
    # assign leaf sizes randomly from a gamma distribution
    leaf.size <- ceiling(rgamma(n_plants * n_pops, shape = mean.size ^ 2 / size.dev ^ 2, scale = size.dev ^ 2 / mean.size))
    
    # assign same leaf size to each leaf within a plant
    leaf.size <- rep(leaf.size, each = n_leaves)
    
    # add leaf sizes, mean, and standard deviation to data frame
    leaf.data$init.size = leaf.size
    leaf.data$plant.mean = rep(mean.size, nrow(leaf.data))
    leaf.data$plant.dev = rep(size.dev, nrow(leaf.data))
  }
  
  # [4] variation in leaf size between populations
  else if (leaf.var == 4 || leaf.var == "population") {
    # set mean and standard deviation in leaf size
    mean.size = prms$pop.mean
    size.dev = prms$pop.dev
    
    # assign leaf sizes from a random gamma distribution
    leaf.size = ceiling(rgamma(n_pops, shape = mean.size ^ 2 / size.dev ^ 2, scale = size.dev ^ 2 / mean.size))
    
    # assign same leaf size to each leaf within a population
    leaf.size = rep(leaf.size, each = n_leaves * n_plants)
    
    # add leaf sizes, mean and standard deviation to data frame
    leaf.data$init.size = leaf.size
    leaf.data$pop.mean = rep(mean.size, nrow(leaf.data))
    leaf.data$pop.dev = rep(size.dev, nrow(leaf.data))
  }
  
  # [5] variation in leaf size within and between plants
  else if (leaf.var == 5 || leaf.var == "leaf + plant") {
    # set mean and standard deviation in leaf size at the plant level
    plant.mean = prms$plant.mean
    plant.dev = prms$plant.dev
    
    # assign mean plant sizes randomly from a gamma distribution
    leaf.mean = ceiling(rgamma(n_plants * n_pops, shape = plant.mean ^ 2 / plant.dev ^ 2, scale = plant.dev ^ 2 / plant.mean))
    leaf.dev = prms$dev.mult * leaf.mean
    
    leaf.size = rep(NA, nrow(leaf.data))
    
    for(i in 1:length(leaf.mean)) {
      mean.size = leaf.mean[i]
      size.dev = leaf.dev[i]
      leaf.size[((i - 1) * n_leaves + 1):(i * n_leaves)] = ceiling(rgamma(n_leaves, shape = mean.size ^ 2 / size.dev ^ 2, scale = size.dev ^ 2 / mean.size))
    }
    
    leaf.data$init.size = leaf.size
    leaf.data$leaf.mean = rep(leaf.mean, each = n_leaves)
    leaf.data$leaf.dev = rep(leaf.dev, each = n_leaves)
    leaf.data$plant.mean = rep(plant.mean, nrow(leaf.data))
    leaf.data$plant.dev = rep(plant.dev, nrow(leaf.data))
  }
  
  # [6] variation in leaf size within and between populations
  else if (leaf.var == 6 || leaf.var == "plant + population") {
    pop.mean = prms$pop.mean
    pop.dev = prms$pop.dev
    
    plant.mean = ceiling(rgamma(n_pops, shape = pop.mean ^ 2 / pop.dev ^ 2, scale = pop.dev ^ 2 / pop.mean))
    plant.dev = prms$dev.mult * plant.mean
    
    leaf.size = rep(NA, nrow(leaf.data))
    
    for(i in 1:length(plant.mean)) {
      mean.size = plant.mean[i]
      size.dev = plant.dev[i]
      leaf.size[((i - 1) * n_plants * n_leaves + 1):(i * n_plants * n_leaves)] = rep(ceiling(rgamma(n_plants, shape = mean.size ^ 2 / size.dev ^ 2, scale = size.dev ^ 2 / mean.size)), each = n_leaves)
    }
    
    leaf.data$init.size = leaf.size
    leaf.data$plant.mean = rep(plant.mean, each = n_plants * n_leaves)
    leaf.data$plant.dev = rep(plant.dev, each = n_plants * n_leaves)
    leaf.data$pop.mean = rep(pop.mean, nrow(leaf.data))
    leaf.data$pop.dev = rep(pop.dev, nrow(leaf.data))
  }
  
  # [7] variation in leaf size at all levels
  else if (leaf.var == 7 || leaf.var == "leaf + plant + population") {
    pop.mean = prms$pop.mean
    pop.dev = prms$pop.dev
    
    plant.mean = ceiling(rgamma(n_pops, shape = pop.mean ^ 2 / pop.dev ^ 2, scale = pop.dev ^ 2 / pop.mean))
    plant.dev = prms$dev.mult * plant.mean
    
    leaf.mean = rep(NA, n_plants * n_pops)
    for(i in 1:n_plants) {
      leaf.mean [((i - 1) * n_leaves + 1):(i * n_leaves)] = ceiling(rgamma(n_leaves, shape = plant.mean ^ 2 / plant.dev ^ 2, scale = plant.dev ^ 2 / plant.mean))
    }
    leaf.dev = prms$dev.mult * leaf.mean
    
    leaf.size = rep(NA, nrow(leaf.data))
    for(i in 1:length(leaf.mean)) {
      mean.size = leaf.mean[i]
      size.dev = leaf.dev[i]
      leaf.size[((i - 1) * n_leaves + 1):(i * n_leaves)] = ceiling(rgamma(n_leaves, shape = mean.size ^ 2 / size.dev ^ 2, scale = size.dev ^ 2 / mean.size))
    }
    
    leaf.data$init.size = leaf.size
    leaf.data$leaf.mean = rep(leaf.mean, each = n_leaves)
    leaf.data$leaf.dev = rep(leaf.dev, each = n_leaves)
    leaf.data$plant.mean = rep(plant.mean, each = n_plants * n_leaves)
    leaf.data$plant.dev = rep(plant.dev, each = n_plants * n_leaves)
    leaf.data$pop.mean = rep(pop.mean, nrow(leaf.data))
    leaf.data$pop.dev = rep(pop.dev, nrow(leaf.data))
  }
  
  # leaf.var entered wrong
  else {
    leaf.size <- rep(NA, nrow(leaf.data))
  }
  
  return(leaf.data) # returns entire dataframe
}




#### function to assign damage to leaves ####
# input for this function is a list of parameters, one of the types of damage (see below), and a data frame of plants
damage.fun <- function(prms, dam.var, leaf.data) {
  ## how should amount of damage be distributed between leaves and plants ##
  # [1] constant - a set amount of damage is distributed randomly between all leaves in a population #
  # [2] variable #
  
  ## determine amount of damage per leaf
  
  # [1] damage is uniformly distributed within a populations
  if (dam.var == 1 || dam.var == "constant") {
    # initialize damage column in data frame
    leaf.data$damage = rep(0, nrow(leaf.data))
    
    total.leaf.area = sum(leaf.data$init.size)
    total.damage = ceiling(prms$pct.damage * total.leaf.area)
  
    for(kk in 1:n_pops) { # loop through each population
      # calculate total amount of damage
      total.leaf.area = sum(subset(leaf.data, pop.id == kk)$init.size)
      total.damage = ceiling(prms$pct.damage * total.leaf.area)
      
      # assign damage randomly to leaves in the population
      for(ii in 1:total.damage) {
        target = floor(runif(1, min = n_plants * n_leaves * (kk - 1) + 1, max = n_plants * n_leaves * kk + 1))
        while (ceiling(leaf.data$init.size[target]) == leaf.data$damage[target]) {
          target = floor(runif(1, min = n_plants * n_leaves * (kk - 1) + 1, max = n_plants * n_leaves * kk + 1))
        }
        leaf.data$damage[target] = leaf.data$damage[target] + 1
      }
    }
    
    # add the percent of leaf area removed from the population to the data frame
    leaf.data$dam.mean = rep(prms$pct.damage, nrow(leaf.data))
  }
  
  # [2] damage is specified based on leaf size
  else if (dam.var == 2 || dam.var == "variable") {
    # initialize damage column in data frame
    leaf.data$damage = rep(0, nrow(leaf.data))
    
    for(kk in 1:n_pops) { # loop through each population
      # calculate total amount of damage
      total.leaf.area = sum(subset(leaf.data, pop.id == kk)$init.size)
      total.damage = rpois(1, lambda = ceiling(prms$pct.damage * total.leaf.area))
      
      # assign damage randomly to leaves in the population
      for(ii in 1:total.damage) {
        target = floor(runif(1, min = n_plants * n_leaves * (kk - 1) + 1, max = n_plants * n_leaves * kk + 1))
        while (ceiling(leaf.data$init.size[target]) == leaf.data$damage[target]) {
          target = floor(runif(1, min = n_plants * n_leaves * (kk - 1) + 1, max = n_plants * n_leaves * kk + 1))
        }
        leaf.data$damage[target] = leaf.data$damage[target] + 1
      }
    }
    
    # add the percent of leaf area removed from the population to the data frame
    leaf.data$dam.mean = rep(prms$pct.damage, nrow(leaf.data))
  }
  
  
  # dam.var entered wrong
  else {
    leaf.data$damage = rep(NA, nrow(leaf.data))
  }
  
  return(leaf.data)
}




#### function to run the simulation ####
run.sim <- function() {
  # initialize leaves and assign damage to them
  # change leaf.var and dam.var for different levels of variation (see above functions)
  leaf.data = initialize.leaves(leaf.var = 1, prms = params)
  leaf.data = damage.fun(dam.var = 1, prms = params, leaf.data = leaf.data)
  
  # calculate final size and percent damage
  leaf.data$fin.size = leaf.data$init.size - leaf.data$damage
  leaf.data$pct.damage = (leaf.data$init.size - leaf.data$fin.size) / leaf.data$init.size
  
  return(leaf.data)
}

leaf.data <- run.sim()
save(leaf.data, file=file.path("data/sampling.error", "null0.4leaves.15pct.RData"))




#### organize data by plant ####
# define coefficient of variation
cv <- function(data) sd(data) / mean(data) 

# find average damage per plant
plant.data <- aggregate(leaf.data$pct.damage, by = list(plant.id = leaf.data$plant.id, pop.id = leaf.data$pop.id, pct.damage = leaf.data$dam.mean), FUN = mean)
names(plant.data) <- c("plant.id", "pop.id", "mean.damage", "pct.damage") # reassign names

plant.data$total.dam <- aggregate(leaf.data$damage, by = list(plant.id = leaf.data$plant.id, pop.id = leaf.data$pop.id, pct.damage = leaf.data$dam.mean), FUN = sum)$x
plant.data$avg.dam <- plant.data$total.dam / n_leaves

# calculate summary statistics (cv and gini)
plant.data$pct.dam.cv <- aggregate(leaf.data$pct.damage, by = list(plant.id = leaf.data$plant.id, pop.id = leaf.data$pop.id, pct.damage = leaf.data$dam.mean), FUN = cv)$x
plant.data$pct.dam.gini <- aggregate(leaf.data$pct.damage, by = list(plant.id = leaf.data$plant.id, pop.id = leaf.data$pop.id, pct.damage = leaf.data$dam.mean), FUN = Gini)$x

# find percent zeroes per plant 
plant.data$no.dam <- aggregate(leaf.data$pct.damage == 0, by = list(plant.id = leaf.data$plant.id, pop.id = leaf.data$pop.id, pct.damage = leaf.data$dam.mean), FUN = sum)$x / n_leaves
plant.data$low.dam <- aggregate(leaf.data$pct.damage <= 0.05, by = list(plant.id = leaf.data$plant.id, pop.id = leaf.data$pop.id, pct.damage = leaf.data$dam.mean), FUN = sum)$x / n_leaves

# save file
save(plant.data, file = file.path("data/sampling.error", "null0.4leaves.15pct.plants.RData"))




#### organize data by population ####
# calculate the total damage per population
pop.data <- aggregate(plant.data$pct.damage, by = list(pop.id = plant.data$pop.id, mean.damage = plant.data$mean.damage), FUN = mean)
names(pop.data) <- c("pop.id", "mean.damage", "pct.damage") # reassign names

# calculate summary statistics
pop.data$pct.dam.cv <- aggregate(plant.data$pct.damage, by = list(pop.id = plant.data$pop.id, mean.damage = plant.data$mean.damage), FUN = cv)$x
pop.data$pct.dam.gini <- aggregate(plant.data$pct.damage, by = list(pop.id = plant.data$pop.id, mean.damage = plant.data$mean.damage), FUN = Gini)$x

pop.data$avg.dam.cv <- aggregate(plant.data$avg.dam, by = list(pop.id = plant.data$pop.id, mean.damage = plant.data$mean.damage), FUN = cv)$x
pop.data$avg.dam.gini <- aggregate(plant.data$avg.dam, by = list(pop.id = plant.data$pop.id, mean.damage = plant.data$mean.damage), FUN = Gini)$x


# find percent zeroes per pop
pop.data$no.dam.leaves <- aggregate(leaf.data$pct.damage == 0, by = list(pop.id = leaf.data$pop.id, mean.damage = leaf.data$dam.mean), FUN = sum)$x / (n_plants * n_leaves)
pop.data$low.dam.leaves <- aggregate(leaf.data$pct.damage <= 0.05, by = list(pop.id = leaf.data$pop.id, mean.damage = leaf.data$dam.mean), FUN = sum)$x / (n_plants * n_leaves)

pop.data$no.dam.plants <- aggregate(plant.data$pct.damage == 0, by = list(pop.id = plant.data$pop.id, mean.damage = plant.data$mean.damage), FUN = sum)$x / n_plants
pop.data$low.dam.plants <- aggregate(plant.data$pct.damage <= 0.05, by = list(pop.id = plant.data$pop.id, mean.damage = plant.data$mean.damage), FUN = sum)$x / n_plants

save(pop.data, file = file.path("data/sampling.error", "null0.4leaves.15pct.pops.RData"))
