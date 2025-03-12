# Load necessary libraries
library(MCMCvis)
library(ggplot2)
library(gridExtra)

# Get the MCMC chains for Model 3 specifically
mcmc_chains <- as.mcmc.list(results$models$model3$Sol)

# Get parameter names
param_names <- colnames(mcmc_chains[[1]])

# Create a named vector for parameter name mapping
nice_names <- c(
  "(Intercept)" = "Intercept",
  "bcnh_nest_density" = "Night-Heron Nest Density",
  "dcco_nest_density" = "Cormorant Nest Density",
  "dcco_growthindex" = "Cormorant Growth Index",
  "bcnh_road_proximity" = "Night-Heron Road Proximity",
  "deterrence_activenestremoval" = "Management"
)

# Keep only the fixed effects parameters
fixed_effects <- c("(Intercept)", "bcnh_nest_density", "dcco_nest_density", 
                   "dcco_growthindex", "bcnh_road_proximity", 
                   "deterrence_activenestremoval")

# Filter to keep only the fixed effects parameters that exist in the model
params_to_plot <- intersect(param_names, fixed_effects)

# Create a list to store all the plots
plot_list <- list()

# Create a ggplot for each parameter
for (i in seq_along(params_to_plot)) {
  param <- params_to_plot[i]
  
  # Get the MCMC samples for this parameter
  samples <- as.vector(mcmc_chains[[1]][, param])
  
  # Get the nice display name for this parameter
  display_name <- nice_names[param]
  
  # Create a data frame for plotting
  df <- data.frame(value = samples)
  
  # Create the ggplot
  p <- ggplot(df, aes(x = value)) +
    geom_density(fill = "lightblue", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    labs(title = display_name,
         x = "Parameter Estimate", 
         y = "Density") +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9),
      plot.title = element_text(size = 10, hjust = 0.5),
      plot.margin = margin(t = 10, r = 10, b = 20, l = 30, unit = "pt")
    )
  
  # Add to the list
  plot_list[[i]] <- p
}

# Arrange all plots in a grid
arranged_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = 2))

# Save the combined plot
ggsave("model3_posterior_densities_ggplot.png", arranged_plots, width = 10, height = 8, units = "in", dpi = 300)