setwd("../")
library(ProjectTemplate)
reload.project()
library(ordinal)
library(pscl)
library(parameters)

# Analysis 1
# =====================================

dat_1 <- dat %>%
  mutate_at(vars(-sef, -intolerance, -sex, -score_cidia), as_numeric)

indeps <- c(
  "start", "dur", "score_mhd", "score_phq", "fam",
  "score_gad", "score_cidia", "score_pad", "bmi",
  "age", "sex"
)

hm_uni <- fit_uni_hurdle(dat_1, "mean_sef", indeps)
hm <- fit_hurdle(dat_1, "mean_sef", indeps)

cache("hm_uni")
cache("hm")

# hm_20_uni <- fit_uni_hurdle(dat_1, "mean_sef_20", indeps)
# hm_20 <- fit_hurdle(dat_1, "mean_sef_20", indeps)

# cache("hm_20_uni")
# cache("hm_20")

# hm_15_uni <- fit_uni_hurdle(dat_1, "mean_sef_15", indeps)
# hm_15 <- fit_hurdle(dat_1, "mean_sef_15", indeps)

# cache("hm_15_uni")
# cache("hm_15")

# hm_10_uni <- fit_uni_hurdle(dat_1, "mean_sef_10", indeps)
# hm_10 <- fit_hurdle(dat_1, "mean_sef_10", indeps)

# cache("hm_10_uni")
# cache("hm_10")

m_sef <- fit_prop(dat_1, "sef", indeps)
cache("m_sef")

m_into <- fit_prop(dat_1, "intolerance", indeps)
cache("m_into")

# Analysis 2
# =====================================

dat_2 <- dat %>%
  mutate_at(vars(-remission, -ben, -sex, -mean_eff, -score_cidia, -n_best), as_numeric)

covs <- c(
  "start", "dur", "score_mhd", "score_phq",
  "score_gad", "score_cidia", "score_pad", "fam",
  "bmi", "age", "sex"
)

sef_indeps <- c("mean_sef", "intolerance", "sef")
eff_deps <- c("mean_eff", "remission", "ben", "n_best")

m_eff_mean_sef <- fit_eff_sef(dat_2, eff_deps, "mean_sef")

m_eff_mean_intolerance <- fit_eff_sef(dat_2, eff_deps, "intolerance")

m_eff_mean_sef <- fit_eff_sef(dat_2, eff_deps, "sef")

cache("m_eff")

m_ben <- fit_prop(dat_2, "ben", c("mean_sef", covs))
cache("m_ben")

m_remi <- fit_prop(dat_2, "remission", c("mean_sef", covs))
cache("m_remi")

# Literature suggets that we can actually again use this same model for
# ordinal resposne for zero-inflated data by grouping zero into its own
# cateogry

# Odds ratio, relative risk etc.?
