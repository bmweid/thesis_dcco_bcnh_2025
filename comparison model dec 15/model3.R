# model3.R - Third model version
# model3.R
source("config.R")

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