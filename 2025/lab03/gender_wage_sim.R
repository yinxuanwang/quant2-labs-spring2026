library(tidyverse)
set.seed(1)

# - Suppose the following DGP
#      - `Occ` = -0.1 * `Fem` + u + $\epsilon_1$
#      - `Wage` = -0.1 * `Fem` + 2 * u + `Occ` + $\epsilon_2$
#      - `Fem` ~ Bernoulli(0.5)
#      - $\epsilon_1, \epsilon_2, u \sim N(0,1)$

# Simluate data using the above structural model 

N <- 10000
tb <- tibble(
    female = sample(c(0, 1), N, replace = T),
    u = rnorm(N),

    # occ = ...
    # wage = ...
)

# Regress wage on female with and without occupation control. 
# What do we observe?