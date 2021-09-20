## Fit LCA models across multiple numbers of classes
lca_estimate_classes <- function(data, formula = NULL, covariates = NULL, nclass, 
                                 maxiter = 10000, graphs = FALSE, tol = 1e-10, 
                                 na.rm = TRUE, probs.start = NULL, nrep = 1, 
                                 verbose = FALSE, calc.se = TRUE) {
  
  require(poLCA)
  require(dplyr)
  require(purrr)
  
  # Syntactically valid names
  colnames(data) <- make.names(colnames(data))
  
  # Formula
  if (is.null(formula)) { # if formula is not specified
    if(is.null(covariates)) { # if covariates is not specified
      formula <- as.formula(
        paste0(
          "cbind(", 
          paste(colnames(data), collapse = ","), 
          ") ~ 1"))
    } else { # else if covariates is specified
      formula <- as.formula(
        paste0(
          "cbind(", 
          paste(colnames(dplyr::select(data, -all_of(covariates))), collapse = ","), 
          ") ~ 1 + ", 
          paste(covariates, collapse = " + ")))
    }
  }
  
  # function to enable fitting models across multiple numbers of classes
  lca_fn <- function(n_class) {
    poLCA(formula = formula, data = data, nclass = n_class, 
          maxiter = maxiter, graphs = graphs, tol = tol, 
          na.rm = na.rm, probs.start = probs.start, nrep = nrep,
          verbose = verbose, calc.se = calc.se)
  }
  
  # fit model for each class
  if (require(furrr)) { #enable parallel processing with furrr
    require(furrr)
    out <- all_of(nclass) %>%
      future_map(lca_fn, .options = furrr_options(seed = TRUE))
  } else { #sequential processing with purrr
    out <- all_of(nclass) %>%
      map(lca_fn)
  }
  
  # name model fits
  names(out) <- paste0("LCA_", nclass)
  
  return(out)
  
}