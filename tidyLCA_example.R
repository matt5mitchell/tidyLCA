## Examples ##

# Simulate data
source(simulate_LCA_data.R)

# Fit LCA models with multiple numbers of classes
lca_fit <- lca_data %>%
  lca_estimate_classes(cbind(var1, var2, var3, var4) ~ 1,
                       nclass = 2:6)

# Get fit statistics for all models
lca_get_fit(lca_fit)

# Get conditional probabilities by variable for each class
lca_get_estimates(lca_fit)
