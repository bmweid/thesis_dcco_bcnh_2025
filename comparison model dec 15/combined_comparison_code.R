# config.R - Store all shared configurations and data preparation ----
library(tidyverse)
library(MCMCglmm)
library(MCMCvis)
library(stargazer)
library(dplyr)

# Shared model settings
MODEL_SETTINGS <- list(
  nitt = 50000,
  thin = 10,
  burnin = 2000,
  pr = TRUE,
  verbose = TRUE
)

# Function to prepare data
prepare_data <- function(file_path) {
data <- read.csv("combined_dataset.csv")

# Initial data preparation
prepared_data <- data |>
  select(year, bcnh_nest_density, dcco_nest_density, bcnh_growthindex,
         dcco_growthindex, bcnh_road_proximity, bcnh_nest_success, 
         dcco_usurpation, deterrence_activenestremoval, raccoon_predation) |>
  mutate(total_nest_density = bcnh_nest_density + dcco_nest_density) |>
  mutate(across(-year, scale)) |>
  arrange(year)

  head(prepared_data)
  
  # Create lagged bcnh_growthindex
  prepared_data <- prepared_data |>
    mutate(bcnh_growthindex_lag = lag(bcnh_growthindex))
  
  # Handle missing values in fixed predictors ----
  # calculate summary statistics for observed data
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
    raccoon_predation = list(
      mean = mean(prepared_data$raccoon_predation, na.rm = TRUE),
      sd = sd(prepared_data$raccoon_predation, na.rm = TRUE)
    ),  
      total_nest_density = list(
        mean = mean(prepared_data$total_nest_density, na.rm = TRUE),
        sd = sd(prepared_data$total_nest_density, na.rm = TRUE)
    )
  )
  
  # Function to impute missing values
  impute_missing_values <- function(data, obs_stats) {
    # Make a copy of the data
    imputed_data <- data
    # Set random seed for reproducibility
    set.seed(123)
    
    # Handle normally distributed variables
    for (var in c("bcnh_nest_density", "dcco_nest_density", "bcnh_growthindex", 
                  "dcco_growthindex", "raccoon_predation", "total_nest_density")) {
      missing_indices <- which(is.na(imputed_data[[var]]))
      if (length(missing_indices) > 0) {
        imputed_data[[var]][missing_indices] <- rnorm(
          n = length(missing_indices),
          mean = obs_stats[[var]]$mean,
          sd = obs_stats[[var]]$sd
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
  
  # Apply the imputation and return the result
  prepared_data <- impute_missing_values(prepared_data, observed_stats)
  return(prepared_data)
}

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

# model1.R - First model version ----
run_model1 <- function(prepared_data) {
  model1_formula <- bcnh_growthindex ~ bcnh_nest_density + dcco_nest_density + 
    bcnh_nest_success + dcco_growthindex + bcnh_road_proximity + 
    dcco_usurpation + deterrence_activenestremoval
  
  # Count number of fixed effects 
  n_fixed <- length(attr(terms(model1_formula), "term.labels")) + 1
  
  model1 <- MCMCglmm(model1_formula,
                     random = ~ year,
                     family = "gaussian",
                     data = prepared_data,
                     prior = get_standard_priors(8),  
                     nitt = 50000,
                     thin = 10,
                     burnin = 2000,
                     pr = TRUE,
                     verbose = TRUE)
  
  return(model1)
}

# model2.R - Second model version ----

run_model2 <- function(prepared_data) {
  model2_formula <- bcnh_growthindex ~ bcnh_nest_density + dcco_nest_density + 
    dcco_growthindex + bcnh_road_proximity + dcco_usurpation + 
    raccoon_predation + deterrence_activenestremoval
  
  n_fixed <- length(attr(terms(model2_formula), "term.labels")) + 1
  
  model2 <- MCMCglmm(model2_formula,
                     random = ~ year,
                     family = "gaussian",
                     data = prepared_data,
                     prior = get_standard_priors(n_fixed),
                     nitt = 50000,
                     thin = 10,
                     burnin = 2000,
                     pr = TRUE,
                     verbose = TRUE)
  
  return(model2)
}

# model3.R - Third model version ----

run_model3 <- function(prepared_data) {
  model3_formula <- bcnh_growthindex ~ bcnh_nest_density + dcco_nest_density + 
    dcco_growthindex + bcnh_road_proximity + 
    deterrence_activenestremoval
  
  n_fixed <- length(attr(terms(model3_formula), "term.labels")) + 1
  
  model3 <- MCMCglmm(model3_formula,
                     random = ~ year,
                     family = "gaussian",
                     data = prepared_data,
                     prior = get_standard_priors(n_fixed),
                     nitt = 50000,
                     thin = 10,
                     burnin = 2000,
                     pr = TRUE,
                     verbose = TRUE)
  
  return(model3)
}

run_model4 <- function(prepared_data) {
  # Modified formula removing the redundant total_nest_density term
  model4_formula <- bcnh_growthindex ~ bcnh_nest_density + dcco_nest_density + 
    dcco_growthindex + bcnh_road_proximity + dcco_usurpation + 
    raccoon_predation + deterrence_activenestremoval
  
  # Count number of fixed effects (including intercept)
  n_fixed <- length(attr(terms(model4_formula), "term.labels")) + 1
  
  model4 <- MCMCglmm(model4_formula,
                     random = ~ year,
                     family = "gaussian",
                     data = prepared_data,
                     prior = get_standard_priors(n_fixed),
                     nitt = 50000,
                     thin = 10,
                     burnin = 2000,
                     pr = TRUE,
                     verbose = TRUE)
  
  return(model4)
}

# main.R - Main script to run everything ----

compare_models <- function(models_list) {
  # Compare DIC
  dic_scores <- sapply(models_list, function(m) m$DIC)
  
  # Compare fixed effects and create custom comparison table
  summaries <- lapply(models_list, summary)
  
  # Extract posterior means and 95% credible intervals
  model_results <- lapply(names(models_list), function(model_name) {
    sum_table <- summaries[[model_name]]$solutions
    data.frame(
      Model = model_name,
      Parameter = rownames(sum_table),
      Mean = sum_table[,"post.mean"],
      Lower_CI = sum_table[,"l-95% CI"],
      Upper_CI = sum_table[,"u-95% CI"],
      pMCMC = sum_table[,"pMCMC"]
    )
  })
  
  # Combine results
  comparison_table <- do.call(rbind, model_results)
  
  # Print formatted comparison
  cat("\nModel Comparison Results:\n")
  cat("\nDIC Scores:\n")
  print(dic_scores)
  
  cat("\nParameter Estimates:\n")
  print(comparison_table, row.names = FALSE)
  
  return(list(
    dic_scores = dic_scores,
    summaries = summaries,
    comparison_table = comparison_table
  ))
}

# Main execution
main <- function() {
  # Read and prepare data
  data <- prepare_data("combined_dataset.csv")
  
  # Run all models
  model1 <- run_model1(data)
  model2 <- run_model2(data)
  model3 <- run_model3(data)
  model4 <- run_model4(data)
  
  # Store models in list
  models <- list(
    model1 = model1,
    model2 = model2,
    model3 = model3,
    model4 = model4
  )
  
  # Run diagnostics for each model
  diagnostics <- lapply(names(models), function(name) {
    run_diagnostics(models[[name]], name)
  })
  
  # Compare models
  comparison <- compare_models(models)
  
  # Save results
  save(models, diagnostics, comparison,
       file = "model_comparison_results.RData")
  
  # Return results
  return(list(
    models = models,
    diagnostics = diagnostics,
    comparison = comparison
  ))
}

# Run the analysis
results <- main()

# create table ----

library(tidyr)
library(dplyr)
library(kableExtra)
library(webshot2)

# Create dataframe with model 3 results
model3_results <- data.frame(
  Parameter = c("Intercept", 
                "BCNH nest density", 
                "DCCO nest density",
                "DCCO growth index",
                "BCNH road proximity",
                "Active nest removal"),
  Mean = c(-0.021023, 
           0.01221272,
           0.0226327,
           0.4460474,
           -0.00004395844,
           -0.01911731),
  Lower = c(-0.361587332,
            -0.348261208,
            -0.341978236,
            0.081900973,
            -0.395979504,
            -0.433151394),
  Upper = c(0.3392793,
            0.3953971,
            0.4184719,
            0.8127304,
            0.4363143,
            0.3998757)
)

# Format numbers to 3 decimal places
model3_results <- model3_results %>%
  mutate(across(c(Mean, Lower, Upper), ~format(round(., 3), nsmall = 3)))

# Create the formatted table
table_output <- model3_results %>%
  kbl(
    caption = "Table 2: Estimated parameters of Black-crowned Night-Heron colony growth model examining the relationship with Double-crested Cormorant abundance.",
    col.names = c("Parameter", 
                  "Posterior Mean", 
                  "2.5% Quantile",
                  "97.5% Quantile"),
    align = c("l", "r", "r", "r"),
    booktabs = TRUE
  ) %>%
  kable_classic(full_width = FALSE) %>%
  row_spec(0, bold = TRUE) %>%
  add_header_above(c(" " = 1, "Parameter Estimates" = 3)) %>%
  footnote(
    general = "Values represent posterior means and 95% credible intervals from the best-fitting MCMC model (DIC = -12.61). Significant effects are indicated by credible intervals that do not overlap zero.",
    threeparttable = TRUE
  )

# Display the table
table_output

# Save as HTML
save_kable(table_output, "model3_posterior_estimates.html")