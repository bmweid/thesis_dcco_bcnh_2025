# main.R TEST

setwd("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo/comparison model dec 15/test")

source("config.R")

# Load and prepare data
data <- read.csv("combined_dataset.csv")
prepared_data <- data %>%
  select(year, bcnh_nest_density, dcco_nest_density, bcnh_growthindex,
         dcco_growthindex, bcnh_road_proximity, bcnh_nest_success, 
         dcco_usurpation, deterrence_activenestremoval) %>%
  mutate(across(-year, scale)) %>%
  arrange(year)

# Handle missing values
# First, calculate summary statistics for observed data
observed_stats <- list(
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
  )
)

# Function to impute missing values
impute_missing_values <- function(data) {
  imputed_data <- data
  set.seed(123)
  
  # Handle normally distributed variables
  for (var in c("bcnh_nest_density", "dcco_nest_density", "bcnh_growthindex", "dcco_growthindex")) {
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
  
  # Handle additional variables
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

# Check for any remaining missing values
missing_summary <- sapply(prepared_data, function(x) sum(is.na(x)))
print("Number of missing values after imputation:")
print(missing_summary)

# Model 1
model1_formula <- bcnh_growthindex ~ bcnh_nest_density + dcco_nest_density + 
  bcnh_nest_success + dcco_growthindex + bcnh_road_proximity + 
  dcco_usurpation + deterrence_activenestremoval

n_fixed1 <- length(attr(terms(model1_formula), "term.labels")) + 1

model1 <- MCMCglmm(model1_formula,
                   random = ~ year,
                   family = "gaussian",
                   data = prepared_data,
                   prior = get_standard_priors(n_fixed1),
                   nitt = 50000,
                   thin = 10,
                   burnin = 2000,
                   pr = TRUE,
                   verbose = TRUE)

# Model 2
model2_formula <- bcnh_growthindex ~ bcnh_nest_density + dcco_nest_density + 
  bcnh_road_proximity + dcco_usurpation

n_fixed2 <- length(attr(terms(model2_formula), "term.labels")) + 1

model2 <- MCMCglmm(model2_formula,
                   random = ~ year,
                   family = "gaussian",
                   data = prepared_data,
                   prior = get_standard_priors(n_fixed2),
                   nitt = 50000,
                   thin = 10,
                   burnin = 2000,
                   pr = TRUE,
                   verbose = TRUE)

# Compare models
models <- list(model1 = model1, model2 = model2)

# Save results
save(models, file = "model_comparison_results.RData")