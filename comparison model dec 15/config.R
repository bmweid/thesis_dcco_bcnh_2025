# config.R - Store all shared configurations and data preparation
library(tidyverse)
library(MCMCglmm)
library(MCMCvis)
library(stargazer)

setwd("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo/comparison model dec 15")

# Function to prepare data
prepare_data <- function(data_path) {
  data <- read.csv(data_path)
  prepared_data <- data %>%
    select(year, bcnh_nest_density, dcco_nest_density, bcnh_growthindex,
           dcco_growthindex, bcnh_road_proximity, bcnh_nest_success, 
           dcco_usurpation, deterrence_activenestremoval, raccoon_predation) %>% # Added raccoon_predation
    mutate(across(-year, scale)) %>%
    arrange(year)
  
  head(prepared_data)
  
  # Scale variables  ----
  prepared_data <- prepared_data %>%
    mutate(across(-year, scale)) %>%
    arrange(year)
  
  # Create lagged bcnh_growthindex
  prepared_data <- prepared_data %>%
    mutate(bcnh_growthindex_lag = lag(bcnh_growthindex))
  
  # Handle missing values in fixed predictors ----
  # First, calculate summary statistics for observed data
  observed_stats <- list(
    # For normally distributed variables
    bcnh_nest_density = list(
      mean = mean(prepared_data$bcnh_nest_density, na.rm = TRUE),
      sd = sd(prepared_data$bcnh_nest_density, na.rm = TRUE)
    ),
    dcco_nest_density = list(
      mean = mean(prepared_data$dcco_nest_density, na.rm = TRUE),
      sd = sd(prepared_data$dcco_nest_density, na.rm = TRUE)
    ),
    bcnh_growthindex = list(
      mean = mean(prepared_data$bcnh_growthindex, na.rm = TRUE),
      sd = sd(prepared_data$bcnh_growthindex, na.rm = TRUE)
    ),
    dcco_growthindex = list(
      mean = mean(prepared_data$dcco_growthindex, na.rm = TRUE),
      sd = sd(prepared_data$dcco_growthindex, na.rm = TRUE)
    ),
    # Added raccoon_predation
    raccoon_predation = list(
      mean = mean(prepared_data$raccoon_predation, na.rm = TRUE),
      sd = sd(prepared_data$raccoon_predation, na.rm = TRUE)
    )
  )
  
  # Function to impute missing values
  impute_missing_values <- function(data) {
    # Make a copy of the data
    imputed_data <- data
    
    # Set random seed for reproducibility
    set.seed(123)
    
    # Handle normally distributed variables
    for (var in c("bcnh_nest_density", "dcco_nest_density", "bcnh_growthindex", 
                  "dcco_growthindex", "raccoon_predation")) {  # Added raccoon_predation
      missing_indices <- which(is.na(imputed_data[[var]]))
      if (length(missing_indices) > 0) {
        imputed_data[[var]][missing_indices] <- rnorm(
          n = length(missing_indices),
          mean = observed_stats[[var]]$mean,
          sd = observed_stats[[var]]$sd
        )
      }
    }
    
    # Handle management variable (Bernoulli)
    missing_management <- which(is.na(imputed_data$deterrence_activenestremoval))
    if (length(missing_management) > 0) {
      prob_management <- mean(imputed_data$deterrence_activenestremoval, na.rm = TRUE)
      imputed_data$deterrence_activenestremoval[missing_management] <- rbinom(
        n = length(missing_management),
        size = 1,
        prob = prob_management
      )
    }
    
    # Handle prior colony growth index
    missing_prior_growth <- which(is.na(imputed_data$bcnh_growthindex_lag))
    if (length(missing_prior_growth) > 0) {
      min_growth <- min(imputed_data$bcnh_growthindex_lag, na.rm = TRUE)
      max_growth <- max(imputed_data$bcnh_growthindex_lag, na.rm = TRUE)
      imputed_data$bcnh_growthindex_lag[missing_prior_growth] <- runif(
        n = length(missing_prior_growth),
        min = min_growth,
        max = max_growth
      )
    }
    
    # Additional variables
    for (var in c("bcnh_road_proximity", "bcnh_nest_success", "dcco_usurpation")) {
      missing_indices <- which(is.na(imputed_data[[var]]))
      if (length(missing_indices) > 0) {
        var_mean <- mean(imputed_data[[var]], na.rm = TRUE)
        var_sd <- sd(imputed_data[[var]], na.rm = TRUE)
        imputed_data[[var]][missing_indices] <- rnorm(
          n = length(missing_indices),
          mean = var_mean,
          sd = var_sd
        )
      }
    }
    
    return(imputed_data)
  }
  
  # Apply the imputation
  prepared_data <- impute_missing_values(prepared_data)
  
  return(prepared_data)
}

# Shared model settings
MODEL_SETTINGS <- list(
  nitt = 50000,
  thin = 10,
  burnin = 2000,
  pr = TRUE,
  verbose = TRUE
)

# Define standard priors
get_standard_priors <- function(n_fixed_effects) {
  list(
    R = list(V = 1, nu = 0.002),
    G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 1)),
    B = list(mu = rep(0, n_fixed_effects),  # includes intercept
             V = diag(n_fixed_effects) * 25)
  )
}

# Function for model diagnostics
run_diagnostics <- function(model, model_name) {
  diagnostics <- list(
    effective_size_sol = effectiveSize(model$Sol),
    effective_size_vcv = effectiveSize(model$VCV),
    summary = summary(model),
    pvalue = mean(predict(model, marginal = NULL) > 
                    model$data$bcnh_growthindex)
  )
  return(diagnostics)
}