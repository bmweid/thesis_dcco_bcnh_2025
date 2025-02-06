# Load libraries ----
library(tidyverse)  # for data manipulation (tidyr, dplyr), visualization, (ggplot2), ...
library(lme4)  # for hierarchical models
library(sjPlot)  # to visualise model outputs
library(ggeffects)  # to visualise model predictions
library(MCMCglmm)  # for Bayesian models
library(MCMCvis)  # to visualise Bayesian model outputs
library(stargazer)  # for tables of model outputs

# Set WD and load data ----
#setwd("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo")

data <- read.csv("combined_dataset.csv")
str(data)
head(data)

# Prepare the data ----
#Select columns from combined dataset to use as fixed effects.
#dec 12: omitted all management beyond active nest removal, omitted total nest density for autocorrelation,
#omitted racoon predation as likely reflected in bcnh nest success
#aiming for 5 fixed effects

prepared_data <- data %>%
  select(year, bcnh_nest_density, dcco_nest_density,bcnh_growthindex, dcco_growthindex,
         bcnh_road_proximity, bcnh_nest_success, dcco_usurpation, deterrence_activenestremoval)

head(prepared_data)

# Scale variables  ----
#Scale numerical covariates, excluding year, to have standard deviation of 1 and mean of 0.
#Ensure data is in chronological order.

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
  )
)

# Function to impute missing values
impute_missing_values <- function(data) {
  # Make a copy of the data
  imputed_data <- data
  
  # Set random seed for reproducibility
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
  
  # Handle prior colony growth index (uniform between observed min and max)
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

# Check for any remaining missing values
missing_summary <- sapply(prepared_data, function(x) sum(is.na(x)))
print("Number of missing values after imputation:")
print(missing_summary)

# Set weakly informative priors ----
priors <- list(
  # Residual variance structure (R)
  R = list(V = 1, nu = 0.002),
  
  # Random effects variance structure (G)
  G = list(
    # For year random effect
    G1 = list(V = 1,
              nu = 1,
              alpha.mu = 0,
              alpha.V = 1)),
  
  # Fixed effects priors (B)
  B = list(mu = rep(0, 6),  
           V = diag(6) * 25) 
)

# Define the model ----

model <- MCMCglmm(bcnh_growthindex ~ bcnh_nest_density + dcco_nest_density + 
                    dcco_growthindex + bcnh_road_proximity + 
                    deterrence_activenestremoval,
                  random = ~ year,
                  family = "gaussian",
                  data = prepared_data,
                  prior = priors,
                  nitt = 50000,
                  thin = 10,
                  burnin = 2000,
                  pr = TRUE,
                  verbose = TRUE)

# Check convergence & run diagnostics ----

# Reset the graphics parameters and set up a larger plotting window
par(mar = c(4,4,2,2))  # Reduce margins
dev.off()  # Clear the current device

# Adjust the plotting commands
# For prior distribution plot
windows(width = 10, height = 6)  
plot(density(rnorm(1000, 0, sqrt(25))), 
     main="Prior Distribution for Fixed Effects",
     cex.main = 0.9)  # Slightly smaller title 

# For trace plots
windows(width = 12, height = 8)  # Larger window for multiple traces
plot(model$Sol)

# Examine effective sample sizes
effectiveSize(model$Sol)
effectiveSize(model$VCV)

# Generate summary of the model
summary(model)

# Calculate Bayesian p value ----
# pvalue around A p-value around 0.5 suggests good model fit
# Extreme values (close to 0 or 1) suggest poor fit

# Get fitted values (using NULL for marginal to include all effects)
fitted_values <- predict(model, marginal = NULL)

# Get observed values
observed_values <- prepared_data$bcnh_growthindex

# Calculate Bayesian p-value
pvalue <- mean(fitted_values > observed_values)

# Print results
print("Bayesian p-value:")
print(pvalue)

# Look at residuals
residuals <- observed_values - fitted_values
hist(residuals, main="Histogram of Residuals", xlab="Residuals")




# Create Figure 3: Save posterior densities as HTML ----

# Install and load required packages
if (!require(webshot2)) install.packages("webshot2")
library(webshot2)

# Create descriptive effect names for the parameters
effect_names <- c(
  "Intercept",
  "Effect of BCNH nest density on growth",
  "Effect of DCCO nest density on growth",
  "Effect of DCCO growth index on BCNH",
  "Effect of road proximity on BCNH",
  "Effect of active nest removal on BCNH"
)

# Create HTML content with more compact layout
html_content <- '
<!DOCTYPE html>
<html>
<head>
  <title>Posterior Densities</title>
  <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
  <style>
    body {
      max-width: 800px;
      margin: 0 auto;
      font-family: "Times New Roman", Times, serif;
    }
    .grid-container {
      display: grid;
      grid-template-columns: repeat(2, 1fr);
      gap: 10px;
      padding: 5px;
    }
    .plot {
      width: 100%;
      height: 250px;
    }
    .plot-title {
      text-align: center;
      font-size: 10px;
      margin-bottom: 5px;
    }
    h1 {
      text-align: center;
      font-size: 12px;
      margin: 10px 0;
    }
    .caption {
      font-size: 10px;
      margin: 10px;
      text-align: justify;
    }
  </style>
</head>
<body>
  <h1>Posterior Densities for Model Parameters</h1>
  <div class="grid-container">'

# Generate plots for each parameter
for(i in 1:length(effect_names)) {
  dens <- density(posterior_samples[,i], n = 512)
  
  plot_data <- list(
    x = dens$x,
    y = dens$y,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'rgb(0, 0, 0)', width = 1)
  )
  
  layout <- list(
    showlegend = FALSE,
    margin = list(t = 20, r = 20, b = 30, l = 40),
    xaxis = list(
      zeroline = TRUE,
      zerolinewidth = 1,
      zerolinecolor = 'rgb(0, 0, 0)',
      showgrid = TRUE,
      tickfont = list(size = 8)
    ),
    yaxis = list(
      title = list(text = "Density", font = list(size = 8)),
      showgrid = TRUE,
      tickfont = list(size = 8)
    )
  )
  
  html_content <- paste0(
    html_content,
    sprintf('
    <div>
      <div class="plot-title">%s</div>
      <div class="plot" id="plot%d"></div>
    </div>
    <script>
      Plotly.newPlot("plot%d", [%s], %s, {displayModeBar: false});
    </script>',
            effect_names[i], i, i,
            jsonlite::toJSON(plot_data),
            jsonlite::toJSON(layout)
    )
  )
}

# Add caption and close HTML
html_content <- paste0(html_content, '
  </div>
  <div class="caption">
    <strong>Figure 3.</strong> Estimated posterior densities for effects of black-crowned night-heron (BCNH) nest density, double-crested cormorant (DCCO) nest density, DCCO growth index, road proximity, and active nest removal management on BCNH colony growth index in the Tommy Thompson Park, Toronto, Ontario. Vertical lines show the location of 0, the value indicating no effect. Note the variation in scale of the x-axis among plots. All numerical covariates were scaled to have a standard deviation of 1 prior to model fitting.
  </div>
</body>
</html>')

# Save HTML file
writeLines(html_content, "Figure3_posterior_densities.html")

# Save as PDF using basic webshot2 settings
webshot2::webshot(
  url = "Figure3_posterior_densities.html",
  file = "Figure3_posterior_densities.pdf",
  delay = 2,
  zoom = 2,
  vwidth = 800,
  vheight = 1000
)