pacman::p_load(fixest, ivDiag)

set.seed(1234)

# Setup
# regions (e.g., commuting zones)
N <- 300    
# industries
J <- 15     
# time periods
T <- 10       
beta_true <- 0.5

# Base covaraites 
X1_base <- rnorm(N, mean = 0, sd = 1)
X2_base <- rbinom(N, size = 1, prob = 0.5)
U_base <- rnorm(N, mean = 0, sd = 1)
# Generate Base Shares (fixed across time to act as lagged/base year shares)
shares_raw <- matrix(exp(rnorm(N * J, 0, 1)), nrow = N, ncol = J)
shares_raw[, 1] <- exp(2 * X1_base + rnorm(N, 0, 0.5)) 
shares_raw[, 2] <- exp(2 * X2_base + rnorm(N, 0, 0.5)) 
shares_raw[, 3] <- exp(3 * X1_base + rnorm(N, 0, 0.5)) 
shares_raw[, 4] <- exp(3 * X2_base + rnorm(N, 0, 0.5)) 
shares_raw[, 5] <- exp(2 * U_base + rnorm(N, 0, 0.5)) 
shares_raw[, 6] <- exp(3 * U_base + rnorm(N, 0, 0.5)) 
shares <- shares_raw / rowSums(shares_raw)
colnames(shares) <- paste0("ind", 1:J)

# -------------------------------------- #
# Simulate Panel Data ----
# -------------------------------------- #
df_list <- list()
shifts_list <- list()

for (t in 1:T) {
  # Global shifts trending over time
  g_global <- 0.5 * t + rnorm(J, mean = 0, sd = 1)
  
  # Local unobserved economic shock
  U_it <- rnorm(N, mean = 0.2, sd = 1)
     
  # Covariates 
  # X1 and X2 are driven by their baseline traits.
  X1_it <- X1_base * sqrt(t) + rnorm(N, 0, 0.5) 
  X2_it <- X2_base * sqrt(t) + rnorm(N, 0, 0.5) 
  U2_it <- U_base * sqrt(t) + rnorm(N, 0, 0.5) 
  # X3 
  X3_it <- rnorm(N, mean = 0, sd = 2)     
  
  # US Shift (endogenous), global shock + domestic industry influence
  dom_ind <- colSums(shares * U_it) / N 
  g_US_t <- g_global + 2 * dom_ind + rnorm(J, 0, 0.2)
  
  # Other Country shift (exogenous), global shock + noise
  g_OC_t <- g_global + rnorm(J, 0, 0.2)
  
  shifts_list[[t]] <- data.frame(
    time = t,
    ind = paste0("ind", 1:J),
    shift_US = g_US_t,
    shift_OC = g_OC_t
  )
  
  # Exposure
  D_it <- as.numeric(shares %*% g_US_t)
  
  # Outcome Y depends on exposure, all covariates, fixed effects, and unobserved U
  alpha_i <- rnorm(N) 
  tau_t <- rnorm(1)   

  Y_it <- alpha_i + tau_t + beta_true * D_it + 
    0.8 * X1_it - 0.5 * X2_it + 0.4 * X3_it + 
    0.5 * U_it + 0.3 * U2_it + 
    rnorm(N, 0, 0.5)
  
  df_list[[t]] <- data.frame(
    region = 1:N,
    time = t,
    Y = Y_it,
    D = D_it,
    X1 = X1_it,
    X2 = X2_it,
    X3 = X3_it
  )
}

df_panel <- do.call(rbind, df_list)
df_shifts <- do.call(rbind, shifts_list)

# ---------------------------- #
# Shift-share IV Construction
# ---------------------------- #

# CODE HERE


# ---------------------------- #
# Estimation 
# ---------------------------- #

# OLS
ols <- feols(? | region + time, cluster = ~region, data = df_panel)

# 2SLS
iv  <- feols(? | region + time | ?, cluster = ~region, data = df_panel)

etable(
  list("OLS" = ols, "Shift-share IV" = iv),
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  fitstat = ~ ivf
)

# First-stage F statistic
fitstat(iv, "ivf")

# ivDiag
ivd <- ivDiag(
  data = df_panel,
  Y = "Y",
  D = "D",
  Z = YOURIV,
  controls = c(?),
  FE = c("region","time"),
  cl = c("region"),
  bootstrap = T
)

ivd$F_stat

plot_coef(ivd)
