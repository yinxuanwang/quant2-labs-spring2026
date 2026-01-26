make_data <- function(seed) {
    set.seed(seed)
    N <- 400

    data <- data.frame(
        X1 = rnorm(N)
    )

    for (i in 1:10) {
        y <- rnorm(N)
        name <- paste0("Y", i)
        data[name] <- y
    }

    data$Y11 <- data$Y10 + rnorm(N)
    data$Y12 <- data$Y11 + rnorm(N)
    data$Y13 <- data$Y12 + rnorm(N)
    data$Y14 <- data$Y13 + rnorm(N)
    data$Y15 <- data$Y14 + rnorm(N)

    small <- 0.18
    for (i in 16:20) {
        y <- small * data$X1 + rnorm(N)
        name <- paste0("Y", i)
        data[name] <- y
    }

    data
}

analyze <- function(data) {
    lapply(
        1:20,
        \(i) {
            formula <- as.formula(paste0("Y", i, " ~ X1"))
            feols(formula, data = data)
        }
    )
}