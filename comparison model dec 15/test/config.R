# config.R TEST
library(tidyverse)
library(MCMCglmm)
library(MCMCvis)
library(stargazer)

# Set working directory - replace with your path
setwd("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo/comparison model dec 15/test")


# Function to prepare data
prepare_data <- function(data_path) {
  data <- read.csv(data_path)
  prepared_data <- data %>%
    select(year, bcnh_nest_density, dcco_nest_density, bcnh_growthindex,
           dcco_growthindex, bcnh_road_proximity, bcnh_nest_success, 
           dcco_usurpation, deterrence_activenestremoval) %>%
    mutate(across(-year, scale)) %>%
    arrange(year)
  return(prepared_data)
}

# Define standard priors
get_standard_priors <- function(n_fixed_effects) {
  list(
    R = list(V = 1, nu = 0.002),
    G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 1)),
    B = list(mu = rep(0, n_fixed_effects),
             V = diag(n_fixed_effects) * 25)
  )
}