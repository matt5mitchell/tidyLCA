## Get fit statistics for all models in tidyLCA object
lca_get_fit <- function(x, ...) {
  
  # poLCA.entropy() is not the same calculation used by MPlus or SAS
  # Reference: Ramaswamy, et al. 1993, https://doi.org/10.1287/mksc.12.1.103
  polca_entropy <- function(fit) {
    
    # Required packages
    if(!require("dplyr")){cat("Please install dplyr package...")}
    library(dplyr)
    if(!require("tidyr")){cat("Please install tidyr package...")}
    library(tidyr)
    
    machine_tolerance <- sqrt(.Machine$double.eps)
    n <- fit$N # number of observations
    k <- length(fit$P) #number of classes
    
    # Function to calculate numerator
    # Expects n-by-k matrix of posterior class probabilities
    entropy_num <- function(p) {
      data.frame(p) %>%
        gather("class", "prob", 1:k) %>% # gather classes to rows in order to sum classes and observations
        filter(prob > machine_tolerance) %>% # since Lim_{p->0} p*log(p) = 0
        summarize(entropy = sum(-prob * log(prob)))
    }
    
    num <- entropy_num(fit$posterior) # numerator 
    entropy <- 1 - (num / (n * log(k)))
    return(entropy)
    
  }
  
  # fit statistics to output
  fit_statistics <- function(n) {
    tibble(Classes = length(x[[n]]$P),
           MinSize = min(x[[n]]$P),
           Parameters = x[[n]]$npar,
           ResidDF = x[[n]]$resid.df,
           LogLik = x[[n]]$llik,
           GSq = x[[n]]$Gsq,
           ChiSq = x[[n]]$Chisq,
           AIC = x[[n]]$aic,
           BIC = x[[n]]$bic,
           Entropy = polca_entropy(x[[n]])$entropy)
  }
  
  # number of models
  n_models <- length(x)
  
  # output
  out <- 1:n_models %>%
    map_df(fit_statistics) %>%
    # Average of relative AIC, BIC, and Entropy 
    mutate(Best_Fit = (
      (1 - (AIC - min(AIC))/min(AIC)) +
        (1 - (BIC - min(BIC))/min(BIC)) +
        (1 + (Entropy - max(Entropy, na.rm = TRUE))/max(Entropy, na.rm = TRUE))
    )
    /3)
  
  return(out)
  
}