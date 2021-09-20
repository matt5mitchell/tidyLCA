#######################
## Simulate LCA data ##
#######################

library(tidyverse)

# Simulate data for classes A, B, C, & D
lca_data_A <- data.frame(
  class = "A",
  var1 = sample(c("A", "B", "C", "D"), 150, replace = TRUE, prob = c(0, 0, .1, .9)),
  var2 = sample(c("E", "F", "G"), 150, replace = TRUE, prob = c(0, .1, .9)),
  var3 = sample(c("H", "I"), 150, replace = TRUE, prob = c(.95, .05)),
  var4 = sample(c("J", "K", "L", "M"), 150, replace = TRUE, prob = c(.2, .2, .3, .3))
)

lca_data_B <- data.frame(
  class = "B",
  var1 = sample(c("A", "B", "C", "D"), 200, replace = TRUE, prob = c(.9, .1, 0, 0)),
  var2 = sample(c("E", "F", "G"), 200, replace = TRUE, prob = c(0, .05, .95)),
  var3 = sample(c("H", "I"), 200, replace = TRUE, prob = c(.4, .6)),
  var4 = sample(c("J", "K", "L", "M"), 200, replace = TRUE, prob = c(0, 0, .1, .9))
)

lca_data_C <- data.frame(
  class = "C",
  var1 = sample(c("A", "B", "C", "D"), 300, replace = TRUE, prob = c(.1, .2, .3, .4)),
  var2 = sample(c("E", "F", "G"), 300, replace = TRUE, prob = c(.95, .05, 0)),
  var3 = sample(c("H", "I"), 300, replace = TRUE, prob = c(.1, .9)),
  var4 = sample(c("J", "K", "L", "M"), 300, replace = TRUE, prob = c(.2, .2, .3, .3))
)

lca_data_D <- data.frame(
  class = "D",
  var1 = sample(c("A", "B", "C", "D"), 100, replace = TRUE, prob = c(.3, .3, .2, .2)),
  var2 = sample(c("E", "F", "G"), 100, replace = TRUE, prob = c(.95, .05, 0)),
  var3 = sample(c("H", "I"), 100, replace = TRUE, prob = c(.9, .1)),
  var4 = sample(c("J", "K", "L", "M"), 100, replace = TRUE, prob = c(.9, .1, 0, 0))
)

# Assemble final data frame
lca_data <- rbind(lca_data_A, lca_data_B, lca_data_C, lca_data_D) %>%
  mutate(across(everything(), as.factor))

# Shuffle rows
set.seed(2432)
shuffle <- sample(nrow(lca_data))
lca_data <- lca_data[shuffle,]
