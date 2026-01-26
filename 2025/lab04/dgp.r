library(tidyverse)
# Simulate data
set.seed(3142)
n_obs <- 100
n_vars <- 100
theta <- 1
X <- matrix(rnorm(n_obs * n_vars), nrow = n_obs, ncol = n_vars)

# 100 normally distributed variables
# Let's make some nonlinearities
for(i in 90:100) {
    # Make a random polynomial combination of several other variables
    n_poly <- sample(1:10, 1)
    vars <- sample(1:50, n_poly)
    powers <- sample(1:8, n_poly, replace=T)
    col <- X[,i]
    for (j in 1:n_poly) {
        col = col + X[,vars[j]] ^ powers[j]
    }
    X[,i] <- col
}

d_effects <- rbinom(n_obs, size=1, prob=0.6)
d <- X %*% d_effects + rnorm(n_obs)

x_effects <- rbinom(n_obs, size=1, prob=0.6)
y <- (X %*% x_effects) + (theta * d) + rnorm(n_obs)

df <- data.frame(
    y=y,
    d=d
) %>% cbind(X) 
colnames(df) <- c('y', 'd', sapply(1:100, \(x) paste0('X', x)))
df %>% head
df %>% saveRDS(here::here("lab04/data.rds"))
