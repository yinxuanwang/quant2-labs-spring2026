make_clustered_data <- function(G, N) {
    g <- sample(1:G, size = N, replace = T)
    treat.g <- sample(c(0, 1), size = G, replace = T)
    treat.i <- sapply(g, \(x) treat.g[x])
    effect.g <- rnorm(1:G)
    g.i <- sapply(g, \(x) effect.g[x])
    y <- rnorm(N) + g.i + 0.5 * treat.i
    data.frame(
        g = g,
        treat = treat.i,
        y = y
    )
}

set.seed(123)
df <- make_clustered_data(10, 100)
model <- lm(y ~ treat, df)
summary(model)
df$y_pred <- predict(model, df)
df$residuals <- df$y - df$y_pred

# Making some changes that I don't like

# Implement wild bootstrap to estimate the SE of \beta_(treat)
# given the observed data `df`
# Residual sign is randomized at the cluster level (df$g)
# Then construct y_star = y_pred + random_residual
# Estimate y_star~treat for a bunch of different randomized residual sets
# Retrieve sd(beta_treat) as our estimate for the true (cluster adjusted) SE 
# ...