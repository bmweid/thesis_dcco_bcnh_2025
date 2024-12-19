# model1.R - First model version
source("config.R")

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