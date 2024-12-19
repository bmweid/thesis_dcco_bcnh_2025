# comparison.R - Run and compare all models
source("config.R")
source("model1.R")
source("model2.R")
source("model3.R")

# Function to compare models
compare_models <- function(models_list) {
  # Compare DIC
  dic_scores <- sapply(models_list, function(m) m$DIC)
  
  # Compare fixed effects
  summaries <- lapply(models_list, summary)
  
  # Create comparison table using stargazer
  stargazer(models_list,
            type = "text",
            title = "Model Comparison",
            style = "aer")
  
  return(list(dic_scores = dic_scores,
              summaries = summaries))
}
