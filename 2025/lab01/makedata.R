# R script to generate some data 
library(tidyverse)

set.seed(123)
N <- 1000

# 2 random covariates
x1 <- runif(N, 0, 1)
x2 <- rnorm(N, 0, 0.5)

# Some noise
e <- rnorm(N, 0, 1)

# Treatment effect
d <- rnorm(N, 1, 1)

# Potential outcomes
y0 <- x1 * 4 + x2 + e
y1 <- y0 + d

# Treatment assignment
ts <- c()
for(i in 1:N) {
    t <- sample(c(0,1), 1, prob=c(1-x1[[i]], x1[[i]]))
    ts <- c(ts, t)
}

df <- data.frame(
    x1=x1, 
    x2=rnorm(N, 1, 2),
    x3=x2,
    y0=y0,
    y1=y1,
    t=ts,
    y=ts * y1 + (1-ts) * y0
)
df %>% ggplot(aes(x=y0, y=t)) + geom_point(position='jitter') 

summary(lm(y ~ t, data=df))

dmeans <- mean(df %>% filter(t == 1) %>% .$y) - mean(df %>% filter(t == 0) %>% .$y)
treat_effect <- mean(df$y1) - mean(df$y0) 
treat_effect
dmeans

library(here)
df %>% write_tsv(here('lab1/thescience.tsv'))

cor(df$t, df$y0)

df$t2 <- sample(c(0,1), N, replace=T)
df$y2 <- df$t2 * y1 + (1 - df$t2) * y0

dmeans <- mean(df %>% filter(t2 == 1) %>% .$y2) - mean(df %>% filter(t2 == 0) %>% .$y2)
cor(df$t2, df$y0)
dmeans
treat_effect

df %>% ggplot(aes(x=y, y=t2)) + geom_point(position='jitter') 
df %>% ggplot(aes(x=y2, y=t2)) + geom_point(position='jitter') 
summary(lm(y2 ~ t2, data=df))

