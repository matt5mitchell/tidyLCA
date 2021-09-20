## Get estimated probabilities for all models in tidyLCA object
lca_get_estimates <- function(x, ...) {
  
  # Function to get conditional probability estimates from model
  estimates_from_model <- function(n) {
    # Classes in model
    n_class <- length(x[[n]]$P)
    
    # Probabilities from model
    probs <- x[[n]]$probs
    
    # Number of variables
    n_vars <- length(probs)
    
    # Variable names
    var_names <- list()
    for (i in 1:length(probs)) {
      var_names[[i]] <- rep(names(probs[i]), times = length(colnames(probs[[i]])))
    }
    var_names <- unlist(var_names)
    
    # Variables levels
    var_levels <- list()
    for (i in 1:n_vars) {
      var_levels[[i]] <- colnames(probs[[i]])
    }
    var_levels <- unlist(var_levels)
    
    # Conditional probabilities by variable for each class
    estimates <- tibble(Model = rep(n_class, times = length(unlist(probs))),
                        Variable = rep(var_names, each = n_class),
                        Class = rep(1:n_class, times = length(var_levels)),
                        Var_Level = rep(var_levels, each = n_class),
                        Probability = round(unlist(probs), 4)) %>%
      arrange(Model, Variable, Class, Var_Level)
    return(estimates)
  }
  
  # number of models
  n_models <- length(x)
  
  # output
  out <- 1:n_models %>%
    map_df(estimates_from_model)
  
  return(out)
  
}