# Senate RDD 

pacman::p_load(rdrobust, tidyverse, haven)
senate <- read_csv("CIT_2020_CUP_senate.csv")
# State-year panel of party vote share in year T and victory margin in year T+2
# The idea is to test for incumbency advantage
Y <- senate$demvoteshfor2
X <- senate$demmv
# Plot the raw data
rdplot(Y, X,
    nbins = c(2500, 500), p = 0, col.lines = "red", col.dots = "lightgray", title = "",
    y.lim = c(0, 100)
)

# Local comparison of means: restrict to cases where absolute value of X < 50
rdplot(Y[abs(X) <= 50], X[abs(X) <= 50],
    nbins = c(2500, 500), p = 4, col.lines = "red", col.dots = "lightgray", title = "",
    y.lim = c(0, 100)
)

# Group the data into bins across X and plot the means of each bin
# rdplot automatically decides the bin width if y ou don't specify any arguments
rdplot(
    y = Y, x = X, ci = 95,
    title = "RD Plot: U.S. Senate Election Data",
    y.label = "Vote Share in Election at time t+2",
    x.label = "Vote Share in Election at time t",
    p = 1 # draw a linear trendline across the bins
)

# Estimate RD effects 
# Important settings: cutoff, kernel and bandwidth
# cutoff default value is 0. For this application since margin of victory cutoff
# is 0 we don't have to do anything

# Use uniform kernel and a bandwidth of 10 percentage points
summary(rdrobust(y = Y, x = X, all = T, kernel = "uniform", h = 10))

# The main effect is 6.89

# We can calculate the main effect by hand using local regressions 
localdata <- senate %>% filter(abs(demmv) <= 10)
m1 <- lm(demvoteshfor2 ~ demmv, localdata %>% filter(demmv > 0))
m2 <- lm(demvoteshfor2 ~ demmv, localdata %>% filter(demmv < 0))

# Note: Why does subtracting the constants recover the RD estimate?
m1$coefficients[[1]] - m2$coefficients[[1]]

# Commonly used triangular kernel (downweighting observations far from the cutpoint)
# rdrobust also finds an optimal bandwidth for you based on reducing the MSE
# of the local regression within the bin.
rdresults <- rdrobust(y = Y, x = X, all = T, kernel = "triangular")

# You can inspect the bandwidth for yourself like this:
rdresults$bws

# Or it's printed in the summary as well
summary(rdresults)

# You can add covariates to rdrobust analysis
summary(
    rdrobust(
        y = Y, x = X,
        covs = cbind(senate$population, senate$dmidterm, senate$dpresdem),
        all = T
    )
)

# You can also estimate using higher order local regressions 
summary(rdrobust(y = Y, x = X, all = T, kernel = "triangular", p=4))
summary(rdrobust(y = Y, x = X, all = T, kernel = "triangular", p=2))
