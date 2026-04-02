pacman::p_load(tidyverse, fixest, panelView, did, HonestDiD,
  PanelMatch, haven, bacondecomp, TwoWayFEWeights)

mafia_raw <- haven::read_dta("data/Organized Crime and Political Quality.dta")

mafia_df <- mafia_raw |>
  mutate(
    year = as.integer(year),
    ID_municip = as.integer(ID_municip),
    mafiaben = coalesce(as.numeric(mafiaben), 0),
    befcomgeneral = coalesce(as.numeric(befcomgeneral), 0),
    befcomgeneral_recode = (1 - befcomgeneral) * mafiaben)


panelview(
  MeanEduPol ~ befcomgeneral_recode, data = mafia_df,
  index = c("ID_municip", "year"), xlab = "Year", ylab = "Municipality",
  display.all = TRUE, gridOff = TRUE, by.timing = TRUE)

# Pooled TWFE
mafia_twfe <- feols(
  MeanEduPol ~ befcomgeneral_recode | ID_municip[trend] + year, 
  data = mafia_df, cluster = ~ID_municip)
etable(mafia_twfe)

# TODO: Define treatment groups based on their first year of treatment
# binary treatment indicator: mafiaben
# become treated indicator: befcomgeneral_recode
# period indicator: year

# Callaway & Sant'Anna estimator
set.seed(1)
cs_dr <- att_gt(yname = "MeanEduPol",      # outcome variable
                gname = "group",           # YOUR GROUP VARIABLE NAME
                idname = "ID_municip",     # unit identifier
                tname = "year",            # year variable
                xformla = ~1,              # covariates (if any)
                data = mafia_df,           # data
                est_method = "dr",         # estimation method
                allow_unbalanced_panel = T, # allow unbalanced panel
                control_group = "nevertreated" # control group
)

# TODO: summarize effect with aggte()


# TODO: Create staggered dataframe

# cohorts <- mafia_df |>
#   group_by(ID_municip) |>
#   summarise(
#     first_treat = if (any(befcomgeneral_recode == 1))
#       min(year[befcomgeneral_recode == 1]) else 0L,
#     ever_treated = as.integer(first_treat > 0),
#     .groups = "drop")
# 
# mafia_staggered <- mafia_df |>
#   left_join(cohorts, by = "ID_municip") |>
#   mutate(
#     time_to_treat = if_else(ever_treated == 1, year - first_treat, 0L))


# TODO: TWFE event study

# TODO: Sun & Abraham event study

