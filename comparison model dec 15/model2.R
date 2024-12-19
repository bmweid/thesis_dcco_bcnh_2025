# model2.R - Second model version
# model2.R
source("config.R")

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