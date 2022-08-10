library(tidyverse)
library(furrr)
library(collapse)

plan(multisession) # for parallel processing with furrr

# A "safe" sample function, per the R help docs (?sample)
resample <- function(x, ...) x[sample.int(length(x), ...)]

# Distribution of needy requests to sample for greedy requests
load('data/request_dist.rda')

# This function only handles the case where all agents are
# greedy and stingy with the specified probabilities
sim <- function(
  N = 10000, # total number of agents; should be evenly divisible by group_size
  t_max = 100, # Total number of time steps
  t_meas = 50, # When to measure survival (might not be used)
  prob_shock = 0.1,
  initial_cattle = 70,
  min_cattle = 64,
  group_size = 16,
  prob_greedy = 0,
  prob_stingy = 0,
  prob_nbr = 1, # probability of making a need-based request
  stat_out = FALSE
){
  
  # Compute all growth rates/shocks/losses for all agents for all time steps
  # For each matrix, agents are rows, time steps are columns
  growth_rate <- matrix(rnorm(t_max*N, 0.034, 0.0253), nrow=N, ncol=t_max)
  shocks <- matrix(rbinom(t_max*N, 1, prob = prob_shock), nrow=N, ncol=t_max)
  losses <- matrix(rnorm(t_max*N, 0.3, 0.1), nrow=N, ncol=t_max) * shocks
  cattle <- matrix(NA_real_, nrow=N, ncol=t_max) # Initialize all cattle for all agents to NA
  colnames(cattle) <- paste0('t', 1:t_max)
  cattle[,1] <- initial_cattle # Set initial cattle for t=1
  survived <- matrix(1, nrow=N, ncol=t_max) # Alive = 1, dead = 0; initialize to all 1's
  colnames(survived) <- paste0('t', 1:t_max)
  prob_transfer <- 1 - prob_stingy
  
  # loop through the rows in sets of "group_size", up to N rows
  # each row is one agent
  # ideally, N should be evenly divisible by group_size
  for (i in seq(group_size, N, group_size)){
    
    # row numbers of the current group -- these are the agents
    ids <- (i-group_size+1):i
    
    # loop across the all time steps (columns)
    for (j in 2:t_max){
      
      alive <- ids[survived[ids,j] == 1] # The surviving cohort ids
      if (length(alive) == 0) break
      
      # New cattle value from previous cattle, growth, and losses
      cattle[alive, j] <- cattle[alive, j-1] + cattle[alive, j-1] * growth_rate[alive, j] - cattle[alive, j-1] * losses[alive, j]
      
      # If more than 1 alive, NBT and GBT
      if (length(alive) > 1) {
        
        # agents whose cattle are < min_cattle
        needy <- alive[(cattle[alive, j] < min_cattle) & rbinom(length(alive), 1, prob = prob_nbr)]
        
        # any non-needy agent with probability = prob_greedy
        greedy <- alive[(cattle[alive, j] >= min_cattle) & rbinom(length(alive), 1, prob = prob_greedy)]
        
        # Need-based transfers
        for (k in needy){
          target <- resample(alive[alive != k], 1) # randomly target request at anyone but oneself
          request <- min_cattle - cattle[k, j] # amount to request
          surplus <- max(cattle[target, j] - min_cattle, 0)
          transfer <- min(request, surplus) * rbinom(1, 1, prob = prob_transfer) # Stingy prob
          cattle[k, j] <- cattle[k, j] + transfer
          cattle[target, j] <- cattle[target, j] - transfer
        }
        
        # Greed-based transfers
        for (k in greedy){
          target <- resample(alive[alive != k], 1) # randomly target request at anyone but oneself
          request <- sample(request_dist, 1) # amount to request (from actual dist of NBT requests)
          surplus <- max(cattle[target, j] - min_cattle, 0)
          transfer <- min(request, surplus) * rbinom(1, 1, prob = prob_transfer) # Stingy prob
          cattle[k, j] <- cattle[k, j] + transfer
          cattle[target, j] <- cattle[target, j] - transfer
        }
      }
      
      # Agents whose cattle < min_cattle for two consecutive time steps die
      died <- alive[cattle[alive, j] < min_cattle & cattle[alive, j-1] < min_cattle]
      
      # Set survival of all dead agents to 0 for this and all future time steps
      survived[died, j:t_max] <- 0
    }
  }
  
  if(stat_out) {
    groups = rep(1:(N/group_size), each = group_size)
    datout <-   bind_rows(
      Mean = colMeans(survived), # Mean survival at all time steps
      SD_survival = apply(survived, 2, sd),
      SD_cattle = colMeans(collap(cattle, groups, FUN=function(x) sd(x, na.rm = T)), na.rm = T), # mean of group SD's
      .id = "Stat"
    ) %>% 
      bind_cols(list(group_size=group_size, greedy=prob_greedy, stingy=prob_stingy, nbr=prob_nbr))
  } else {
    datout <- mean(survived[,t_meas])  # Mean survival only at time step: t_meas
  }
  return(datout)
}

# Run the simulation for all combinations of parameter values (parameter grid)
# group_size : greedy : stingy
# 15*11*11 = 1815 rows
params <- expand_grid(group_size=seq(2, 16, 1), greedy=seq(0, 1, 0.1), stingy=seq(0, 1, 0.1))

# This code runs sim() for each row in the params data frame
# and collects results in the survival data frame
# I'm using furrr::future_pmap_dfr to parallelize it across available cores
# Each row of survival = mean survival for each time step for N agents with
# the parameter values set to the corresponding row of params
out <- 
  future_pmap_dfr(
    params5, # using the minimal parameter grid
    .f = ~sim(N=100000, t_max = 100, prob_shock = 0.1, group_size= ..1, prob_greedy = ..2, prob_stingy = ..3),
    .options = furrr_options(seed=T)
  )

# Combine the parameter values with the sim values and save
# out <- bind_cols(params4[rep(1:4, each=2),], survival) # using the minimal parameter grid
save(out, file = 'data/model-survival.rda')



params2 <- tibble(
  group_size = c(2, 2, 2, 2, 16, 16, 16, 16),
  greedy = c(0, 1, 0, 1, 0, 1, 0, 1),
  stingy = c(0, 0, .5, 0, 0, 0, .5, 0),
  nbr = c(1, 1, 1, 0, 1, 1, 1, 0)
)

out <- 
  future_pmap_dfr(
    params2, # using the minimal parameter grid
    .f = ~sim(N=10000, t_max = 100, prob_shock = 0.1, group_size= ..1, prob_greedy = ..2, prob_stingy = ..3, prob_nbr = ..4, stat_out = TRUE),
    .options = furrr_options(seed=T)
  )

# Combine the parameter values with the sim values and save
# out <- bind_cols(params4[rep(1:4, each=2),], survival) # using the minimal parameter grid
save(out, file = 'data/model-stats.rda')

