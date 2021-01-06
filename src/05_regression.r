setwd("../")
library(ProjectTemplate)
reload.project()
library(ordinal)
library(pscl)
library(parameters)
library(VGAM)
library(googlesheets4)
library(kableExtra)

table(dat[["score_cidid"]])
table(dat[["score_cidia"]])

no_mhd_bmi <- function(indeps) {
  indeps %>% discard(. %in% c("bmi"))
}


map_write <- function(names, list) {
  walk2(names, list, function(n, l) {
    sheet_write(format_model(l), url, n)
  })
}

fit_model <- function(deps, indeps, covs1, covs2, dat) {
  map(deps, function(d) {
    print(d)
    if (is.numeric(dat[[d]])) {
      fit <- fit_hurdle
    } else {
      fit <- fit_prop
    }

    ms <- list()

    # table(dat[["mhd_schizophrenia_numeric"]])
    # table(dat[["mhd_schizoaffective_numeric"]])
    # mat <- dat %>%
    #   na_omit_dat(d, indeps) %>%
    #   .[colnames(.) %in% indeps] %>%
    #   map_df(as.numeric) %>%
    #   as.matrix() %>%
    #   Matrix::rankMatrix()

    ms[[1]] <- format_model(fit(dat, d, indeps))
    vars <- ms[[1]][["var"]]
    
    if (!is.null(covs2)) {
      ms[[2]] <- format_model(fit(dat, d, c(indeps, covs2))) %>%
      filter(var %in% vars) %>%
      arrange(var)
    }

    # ms[[3]] <- format_model(fit(dat, d, c(indeps, covs1, covs2))) %>%
    #   filter(var %in% vars) %>%
    #   arrange(var)

    n <- map_dbl(ms, ~ attr(.x, "n"))

    ms_dat <- plyr::join_all(ms, by = "var")
    colnames(ms_dat) <- make.unique(colnames(ms_dat))
    # for (n in unique(names(ms_dat))) {
    #   ms_dat[n] <- reduce(ms_dat[grep(n, names(ms_dat))], paste)
    # }
    # Remove duplicated columns
    # ms_dat <- ms_dat[unique(names(ms_dat))]
    # sheet_write(ms_dat, url, paste(d, names(indeps), note))
    attr(ms_dat, "dep") <- d
    attr(ms_dat, "n") <- n

    return(ms_dat)
  })
}


fit_explore <- function(deps, indeps, covs1, covs2, dat) {
  models <- map(deps, function(d) {
    if (is.numeric(dat[[d]])) {
      fit <- fit_hurdle
    } else {
      fit <- fit_prop
    }

    mis <- map(indeps, function(i) {
      print(paste(d, i))

      ms <- list()

      ms[[1]] <- fit(dat, d, i) %>%
        filter(grepl(i, Parameter)) %>%
        filter(!grepl("cidia", Parameter))

      ms[[2]] <- fit(dat, d, c(i, covs1)) %>%
        filter(grepl(i, Parameter)) %>%
        filter(!grepl("cidia", Parameter))

      # ms[[3]] <- fit(dat, d, c(i, covs1, covs2)) %>%
      #   filter(grepl(i, Parameter)) %>%
      #   filter(!grepl("cidia", Parameter))

      ms_dat <- bind_rows(ms)
      # ms_dat <- plyr::join_all(ms, by = "var")
      # for (n in unique(names(ms_dat))) {
      #   ms_dat[n] <- reduce(ms_dat[grep(n, names(ms_dat))], paste)
      # }

      return(ms_dat)
    }) %>%
      bind_rows()

    format_model(mis)
    print(mis, n = 200)
    mis[[1]] %>% recode(!!!labels)
    mis[[1]]



    all_cols <- map(mis, colnames)
    max_cols <- which.max(map_dbl(mis, ncol))
    most_cols <- unlist(all_cols[max_cols])

    mis <- map(mis, function(m) {
      cn <- colnames(m)
      if (length(cn) == 7) {
        colnames(m) <- str_replace(cn, "_1", "_0")
      }

      # m[setdiff(most_cols, cn)] <- NA
      return(m)
    })



    if (!length(unique(n)) == 1) {

    }

    mis <- do.call(rbind, mis)

    return(mis)
  })
}


dat <- select(dat, -c(mhd_mdd_numeric, mhd_gad_numeric))

# Analysis 1: Side effect severity and intolerance
# =====================================

dat_1 <- dat %>%
  mutate_at(vars(-sef, -intolerance), as.numeric)

indeps <- c(
  "age", "sex", "start", "dur", "score_mhd", "fam",
  "score_cidia", "score_cidid", "score_pad","wsas", "bmi", "smoke", 
  "martial", "employment"
)

indeps_mhd <- c(
  "age", "sex", "start", "dur", grep("mhd_", names(dat), v = T), "fam",
  "score_cidia", "score_cidid", "score_pad", "wsas", "bmi", "smoke", 
  "martial", "employment"
)

sef_deps <- c("mean_sef", "sef", "intolerance")

# covs1 <- c("age", "sex")
covs2 <- c("score_phq", "score_gad")
# covs2 <- c("score_phq")

m1_1 <- fit_model(sef_deps, indeps, covs1, covs2, dat_1)
m1_w1 <- fit_model(sef_deps, no_mhd_bmi(indeps), covs1, covs2, dat_1)
m1_2 <- fit_model(sef_deps, indeps_mhd, covs1, covs2, dat_1)
m1_w2 <- fit_model(sef_deps, no_mhd_bmi(indeps_mhd), covs1, covs2, dat_1)

dat_1_now <- dat_1[last_treatment_now, ]

m1_3 <- fit_model(sef_deps, c(indeps, "score_phq"), covs1, NULL, dat_1_now)
m1_w3 <- fit_model(sef_deps, c(no_mhd_bmi(indeps), "score_phq"), covs1, NULL, dat_1_now)
m1_4 <- fit_model(sef_deps, c(indeps_mhd, "score_phq"), covs1, NULL, dat_1_now)
m1_w4 <- fit_model(sef_deps, c(no_mhd_bmi(indeps_mhd), "score_phq"), covs1, NULL, dat_1_now)

# Analysis 2 Associations with effectiveness
# =====================================

dat_2 <- dat %>%
  mutate_at(vars(-remission, -ben, -mean_eff, -n_best), as.numeric)

indeps_mean_sef <- c(
  "age", "sex", "start", "dur", "score_mhd", "fam",
  "score_cidia", "score_cidid", "score_pad", "wsas", "bmi", "smoke", 
  "martial", "employment",
  "mean_sef"
)

indeps_into <- c(
  "age", "sex", "start", "dur", "score_mhd", "fam",
  "score_cidia", "score_cidid", "score_pad", "wsas", "bmi", "smoke", 
  "martial", "employment",
  "intolerance"
)

indeps_sef <- c(
  "age", "sex", "start", "dur", "score_mhd", "fam",
  "score_cidia", "score_cidid", "score_pad", "wsas", "bmi", "smoke", 
  "martial", "employment",
  "sef"
)

indeps_mean_sef_mhd <- c(
  "age", "sex", "start", "dur", grep("mhd\\_", names(dat), v = T), "fam",
  "score_cidia", "score_cidid", "score_pad", "wsas", "bmi", "smoke", 
  "martial", "employment",
  "mean_sef"
)

indeps_into_mhd <- c(
  "age", "sex", "start", "dur", grep("mhd\\_", names(dat), v = T), "fam",
  "score_cidia", "score_cidid", "score_pad", "wsas", "bmi", "smoke", 
  "martial", "employment",
  "intolerance"
)

indeps_sef_mhd <- c(
  "age", "sex", "start", "dur", grep("mhd\\_", names(dat), v = T), "fam",
  "score_cidia", "score_cidid", "score_pad", "wsas", "bmi", "smoke", 
  "martial", "employment",
  "sef"
)

eff_deps <- c("mean_eff", "remission", "ben", "n_best")

m2_1 <- fit_model(eff_deps, indeps_mean_sef, covs1, covs2, dat_2)
m2_w1 <- fit_model(eff_deps, no_mhd_bmi(indeps_mean_sef), covs1, covs2, dat_2)
m2_2 <- fit_model(eff_deps, indeps_into, covs1, covs2, dat_2)
m2_w2 <- fit_model(eff_deps, no_mhd_bmi(indeps_into), covs1, covs2, dat_2)
m2_3 <- fit_model(eff_deps, indeps_sef, covs1, covs2, dat_2)
m2_w3 <- fit_model(eff_deps, no_mhd_bmi(indeps_sef), covs1, covs2, dat_2)

m2_4 <- fit_model(eff_deps, indeps_mean_sef_mhd, covs1, covs2, dat_2)
m2_5 <- fit_model(eff_deps, indeps_into_mhd, covs1, covs2, dat_2)
m2_6 <- fit_model(eff_deps, indeps_sef_mhd, covs1, covs2, dat_2)
m2_w4 <- fit_model(eff_deps, no_mhd_bmi(indeps_mean_sef_mhd), covs1, covs2, dat_2)
m2_w5 <- fit_model(eff_deps, no_mhd_bmi(indeps_into_mhd), covs1, covs2, dat_2)
m2_w6 <- fit_model(eff_deps, no_mhd_bmi(indeps_sef_mhd), covs1, covs2, dat_2)

dat_2_now <- dat_2[last_treatment_now, ]

m2_7 <- fit_model(eff_deps, c(indeps_mean_sef, "score_phq"), covs1, "score_gad", dat_2)
m2_w7 <- fit_model(eff_deps, c(no_mhd_bmi(indeps_mean_sef), "score_phq"), covs1, "score_gad", dat_2)
m2_8 <- fit_model(eff_deps, c(indeps_into, "score_phq"), covs1, "score_gad", dat_2)
m2_w8 <- fit_model(eff_deps, c(no_mhd_bmi(indeps_into), "score_phq"), covs1, "score_gad", dat_2)
m2_9 <- fit_model(eff_deps, c(indeps_sef, "score_phq"), covs1, "score_gad", dat_2)
m2_w9 <- fit_model(eff_deps, c(no_mhd_bmi(indeps_sef), "score_phq"), covs1, "score_gad", dat_2)

m2_10 <- fit_model(eff_deps, c(indeps_mean_sef_mhd, "score_phq"), covs1, "score_gad", dat_2)
m2_w10 <- fit_model(eff_deps, c(no_mhd_bmi(indeps_mean_sef_mhd), "score_phq"), covs1, "score_gad", dat_2)
m2_11 <- fit_model(eff_deps, c(indeps_into_mhd, "score_phq"), covs1, "score_gad", dat_2)
m2_w11 <- fit_model(eff_deps, c(no_mhd_bmi(indeps_into_mhd), "score_phq"), covs1, "score_gad", dat_2)
m2_12 <- fit_model(eff_deps, c(indeps_sef_mhd, "score_phq"), covs1, "score_gad", dat_2)
m2_w12 <- fit_model(eff_deps, c(no_mhd_bmi(indeps_sef_mhd), "score_phq"), covs1, "score_gad", dat_2)

for (object in ls() %>% grep("m[12]_", ., v = T)) cache(object)

all_mods <- list()
for (object in c("m1_1", "m2_1", "m2_2", "m2_3", "m2_4", "m2_5", "m2_6")) {
  m_tmp <- get(object)
  m_tmp[["var"]]
}


m <- m1_1[[3]]
nr <- nrow(m)

levels <- str_extract(colnames(m), "[0-9]+") %>%
    unique() %>%
    subset(!is.na(.))

split_levels <- map(levels, ~ rep(.x, nr)) %>%
    unlist() %>%
    as.factor()

adjusted_p <- p.adjust(unlist(m[grep("\\.2", colnames(m))]))


map(c("m1_1", "m2_1", "m2_2", "m2_3", "m2_4", "m2_5", "m2_6"), function(object) {
    get(object)
}

map(c("m1_1", "m2_1", "m2_2", "m2_3", "m2_4", "m2_5", "m2_6"), function(object) {
  unlist(map(get(object), ~ .x[["var"]]))
}) %>%
  unlist() %>%
  table()



indeps_all_sef <- names(dat)[42:72]

covs_mean_sef <- c(
  "start", "dur", "score_mhd", "fam",
  "score_cidia", "score_cidid", "score_pad", "bmi", "score_phq", "score_gad"
)

m_exp <- fit_explore(eff_deps[4], indeps_all_sef, covs1, covs_mean_sef, dat_2)

cache("m_exp")

# Literature suggets that we can actually again use this same model for
# ordinal resposne for zero-inflated data by grouping zero into its own
# cateogry

# Odds ratio, relative risk etc.?
