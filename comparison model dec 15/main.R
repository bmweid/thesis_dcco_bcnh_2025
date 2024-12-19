setwd("C:/Users/baill/OneDrive/r-projects/cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo/comparison model dec 15")

#run config.R

# main.R - Main script to run everything
source("config.R")
source("model1.R")
source("model2.R")
source("model3.R")
source("comparison.R")

# Run analysis
data <- prepare_data("combined_dataset.csv")

# Run all models
model1 <- run_model1(data)
model2 <- run_model2(data)
model3 <- run_model3(data)

# Store models in list
models <- list(
  model1 = model1,
  model2 = model2,
  model3 = model3
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