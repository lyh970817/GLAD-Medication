library(lindia)
library(sjPlot)
library(brant)
library(summarytools)
library(tidyverse)
library(gladfunctions)
library(ggplot2)
library(ggcorrplot)
library(kableExtra)
library(gladfunctions)
library(googlesheets4)
library(ggcorrplot)

sheets_auth(
  email = gargle::gargle_oauth_email(),
  path = "../../sheet_scripts/glad-dict.json",
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

na_remove <- function(data) {
  map_df(data, function(col) {
    col[col == -99] <- NA
    col[col == -88] <- NA
    return(col)
  })
}

setwd("~/Data/GLAD/Medications/")
dat_med <- read_csv("MED_Renamed.csv")
setwd("~/Data/GLAD/data_clean/rds_renamed/")
dat_gad <- readRDS("GAD_Renamed.rds")
dat_phq <- readRDS("PHQ_Renamed.rds")
dat_cidip <- readRDS("CIDIP_Renamed.rds")
dat_cidid <- readRDS("CIDID_Renamed.rds")
dat_saspd <- readRDS("SASPD_Renamed.rds")
dat_fam <- readRDS("FAM_Renamed.rds")
dat_dem <- readRDS("DEM_Renamed.rds")
dat_cts <- readRDS("CTS_Renamed.rds")

sheet_gad <- GLAD_sheet("GAD")[[1]]
sheet_fam <- GLAD_sheet("FAM")[[1]]
sheet_dem <- GLAD_sheet("DEM")[[1]]
sheet_pad <- GLAD_sheet("PAD")[[1]]
sheet_cts <- GLAD_sheet("CTS")[[1]]
sheet_phq <- GLAD_sheet("PHQ")[[1]]

unique(grep("Depression", sheet_fam[["title"]], v = T))
unique(grep("Anxiety", sheet_fam[["title"]], v = T))
unique(grep("Panic", sheet_fam[["title"]], v = T))

dat_dem <- GLAD_derive(dat_dem, sheet_dem)
dat_cts <- GLAD_derive(dat_cts, sheet_cts)

dat_list <- list(dat_dem, dat_med, dat_gad, dat_phq, dat_cidip, dat_saspd)
dat_all <- reduce(dat_list, inner_join, by = "ID") %>% na_remove()


ids <- c("ID", "Sex", "Age", "Birthyear")
score_gad_all <- GLAD_derive(dat_gad, sheet_gad)[["gad.total_score"]]
score_phq_all <- GLAD_derive(dat_phq, sheet_phq)[["phq.new.total"]]

mean(score_gad_all, na.rm = T)
mean(score_gad, na.rm = T)
t.test(score_gad_all, score_gad)

mean(score_phq_all, na.rm = T)
mean(score_phq, na.rm = T)
t.test(score_phq_all, score_phq)

score_gad <- rowSums(dat_all %>% select(matches("gad", ignore.case = F)) %>%
  select_if(is.numeric))
score_phq <- rowSums(dat_all %>% select(matches("phq", ignore.case = F)) %>%
  select_if(is.numeric))
score_saspd <- rowSums(dat_all %>% select(matches("saspd", ignore.case = F)) %>%
  select_if(is.numeric))

cidip.items <- c(
  "cidip.see_something_numeric",
  "cidip.hear_voices_numeric",
  "cidip.strange_force_numeric",
  "cidip.unjust_plot_numeric",
  "cidip.happen_past_year_numeric"
  # "cidip.talk_health_professional_numeric",
  # "cidip.prescribed_medication_numeric"
)
score_cidip <- rowSums(dat_all[cidip.items], na.rm = T)

bmi <- dat_all[["dem.bmi_metric"]]
smoke_num <- dat_all[["dem.smoking_status_numeric"]]

smoke_perday <- dat_all[["dem.smoking_perday"]]
smoke_perday[is.na(smoke_perday)] <- 0

smoke_sec <- dat_all[["dem.smoking_secondhand"]]
smoke_years <- dat_all[["dem.smoking_yearssmoked"]]
smoke_years[is.na(smoke_years)] <- 0

# dat_all_eth <- dat_all_dem %>% select(matches("ethnicity.*_numeric"))
# dat_all_eth <- dat_all_dem %>%
#   select(contains("ethnicity")) %>%
#   select(-contains("numeric"))
# table(dat_all_eth[1])
# Everyone is white

dat_med <- dat_all %>% select(matches("MED", ignore.case = F))
add_eff <- dat_med %>%
  select(matches("ADD.*EFF", ignore.case = F)) %>%
  select(-contains("OTHERADD"))
mean_eff <- rowMeans(add_eff, na.rm = T)

n_med <- dat_med[, 2:29] %>%
  rowSums(na.rm = T)

add_sef <- dat_med %>%
  select(matches("ADD.*SEF")) %>%
  select(-contains("OTHERADD"))
mean_sef <- rowSums(add_sef, na.rm = T) / n_med

ben <- dat_med %>%
  select(contains("BEN")) %>%
  pull()

best <- dat_med %>%
  select(contains("BEST")) %>%
  select(-contains("OTHER"))

n_best <- rowSums(best, na.rm = T)
worst <- dat_med %>%
  select(contains("WORST")) %>%
  select(-contains("OTHER"))

sheet_med <- GLAD_sheet("MED")[[1]]

med_qs <- GLAD_getdescr(colnames(dat_med[, 1:29]), sheet_med)

n_medcat <- function(cat) {
  rowSums(dat_med[map_int(cat, grep, med_qs)], na.rm = T)
}

SSRI <- c(
  "Citalopram",
  "Sertraline",
  "Fluoxetine",
  "Paroxetine",
  "Escitalopram",
  "Fluvoxamine"
)

ssri <- n_medcat(SSRI)

NaSSA <- c(
  "Mirtazapine"
)

nassa <- n_medcat(NaSSA)

NSMA <- c(
  "Amitriptyline",
  "Nortriptyline",
  "Clomipramine",
  "Imipramine"
)

nsma <- n_medcat(NSMA)

SNRI <- c(
  "Duloxetine",
  "Venlafaxine",
  "Dosulepin",
  "Lofepramine"
)

snri <- n_medcat(SNRI)

Serotonergics <- c(
  "Trazodone",
  "Vortioxetine"
)

sero <- n_medcat(Serotonergics)

MAO <- c(
  "Moclobemide"
)

mao <- n_medcat(MAO)

Irreversible <- c(
  "Phenelzine",
  "Tranylcypromine",
  "Isocarboxazid"
)

irr <- n_medcat(Irreversible)

Noradrenergics <- c(
  "Mianserin"
)

nora <- n_medcat(Noradrenergics)

Alpha1 <- c(
  "Doxepin",
  "Trimipramine"
)
alpha1 <- n_medcat(Alpha1)

NRI <- c(
  "Reboxetine"
)
nri <- n_medcat(NRI)

dat <- tibble(
  sex = dat_med[["MED.SEX.1.0"]],
  age = dat_all[["Age"]],
  score_gad,
  score_phq,
  score_saspd,
  bmi,
  # smoke_perday,
  # smoke_sec,
  # smoke_years,
  mean_eff,
  mean_sef,
  ben,
  n_best,
  ssri,
  nassa,
  nsma,
  snri,
  sero,
  mao,
  irr,
  nora,
  alpha1,
  nri
)

labels <- c(
  "Sex",
  "Age",
  "Current Anxiety",
  "Current Depression",
  "Personality Disorder",
  "BMI",
  "Mean Effectiveness",
  "Mean Side Effects",
  "Benefits",
  "Number of Best Aspects",
  # "Cigarettes Per Day",
  # "Second Hand Smoking",
  # "Smoking Years",
  "SSRI",
  "NASSA",
  "NSMA",
  "SNRI",
  "SERO",
  "MAO",
  "IRR",
  "NORA",
  "ALPHA1",
  "NRI"
) %>% setNames(colnames(dat))

dat_descr <- dat %>%
  plyr::rename(labels) %>%
  .[, 1:10]
cor_mat <- cor(dat_descr, use = "pairwise.complete.obs", method = "spearman")

all <- summarytools::descr(dat_descr %>%
  select(-Sex),
transpose = T,
stats = c(
  "mean", "med", "sd", "max",
  "min", "skewness"
), headings = FALSE
)

female <- summarytools::descr(dat_descr %>%
  filter(Sex == 0) %>%
  select(-Sex),
transpose = T,
stats = c(
  "mean", "med", "sd", "max",
  "min", "skewness"
), headings = FALSE
)

male <- summarytools::descr(dat_descr %>%
  filter(Sex == 0) %>%
  select(-Sex),
transpose = T,
stats = c(
  "mean", "med", "sd", "max",
  "min", "skewness"
), headings = FALSE
)

sef_ben_1 <- lm(
  data = dat,
  mean_sef ~
  sex +
    age +
    score_gad +
    score_phq +
    score_saspd +
    bmi +
    # smoke_perday +
    # smoke_sec +
    # smoke_years +
    ben +
    ssri +
    nassa +
    nsma +
    snri +
    sero +
    mao +
    irr +
    nora +
    alpha1 +
    nri
)
diag_ols_1 <- plot_model(sef_ben_1, type = "diag")

sef_ben_2 <- lm(
  data = dat,
  log(mean_sef + 1) ~
  sex +
    age +
    score_gad +
    score_phq +
    score_saspd +
    bmi +
    # smoke_perday +
    # smoke_sec +
    # smoke_years +
    ben +
    ssri +
    nassa +
    nsma +
    snri +
    sero +
    mao +
    irr +
    nora +
    alpha1 +
    nri
)
diag_ols_2 <- plot_model(sef_ben_2, type = "diag")

dat <- dat %>%
  mutate(ben = factor(ben, levels = 1:5, ordered = T))
plor_fit <- MASS::polr(
  data = dat,
  ben ~
  sex +
    age +
    score_gad +
    score_phq +
    score_saspd +
    bmi +
    # smoke_perday +
    # smoke_sec +
    # smoke_years +
    mean_sef +
    ssri +
    nassa +
    nsma +
    snri +
    sero +
    mao +
    irr +
    nora +
    alpha1 +
    nri,
  method = "logistic"
)
# tab_model(plor_fit,
#   # pred.labels = c(
#   #   "Intercept", labels[
#   #     !labels %in% c(
#   #       "Mean Side Effects",
#   #       "Mean Effectiveness"
#   #     )
#   #   ]
#   # ),
#   dv.labels = "log(Side Effects + 1)"
# )
bt <- brant(plor_fit)

plor_fit2 <- MASS::polr(
  data = dat,
  ben ~ sex +
    score_phq +
    score_saspd +
    bmi +
    # smoke_perday +
    # smoke_sec +
    # smoke_years +
    mean_sef +
    ssri +
    mao +
    nsma,
  method = "logistic"
)
bplor2 <- brant(plor_fit2)

plor_fit3 <- MASS::polr(
  data = dat,
  ben ~ sex +
    score_phq +
    score_saspd +
    bmi +
    # smoke_perday +
    # smoke_sec +
    # smoke_years +
    mean_sef +
    ssri +
    # mao +
    nsma,
  method = "logistic"
)

dat["mean_eff_cat"] <- cut(dat[["mean_eff"]], breaks = 4, ordered_result = T)
plor_fit_eff1 <- MASS::polr(
  data = dat,
  mean_eff_cat ~ sex +
    age +
    score_phq +
    score_gad +
    score_saspd +
    bmi +
    # smoke_perday +
    # smoke_sec +
    # smoke_years +
    mean_sef +
    ssri +
    nassa +
    nsma +
    snri +
    sero +
    mao +
    irr +
    nora +
    alpha1 +
    nri,
  method = "logistic"
)
plor_fit_eff2 <- MASS::polr(
  data = dat,
  mean_eff_cat ~
  score_phq +
    score_saspd +
    bmi +
    # smoke_perday +
    # smoke_sec +
    # smoke_years +
    mean_sef +
    ssri +
    nassa +
    sero,
  method = "logistic"
)

sef_n_best <- lm(
  data = dat,
  n_best ~
  sex +
    age +
    score_gad +
    score_phq +
    score_saspd +
    bmi +
    # smoke_perday +
    # smoke_sec +
    # smoke_years +
    mean_sef +
    ssri +
    nassa +
    nsma +
    snri +
    sero +
    mao +
    irr +
    nora +
    alpha1 +
    nri
)
tab_model(sef_n_best,
  # pred.labels = c(
  #   "Intercept", labels[
  #     !labels %in% c(
  #       "Mean Side Effects",
  #       "Mean Effectiveness"
  #     )
  #   ]
  # ),
  dv.labels = "log(Side Effects + 1)"
)
diag_ols_best <- plot_model(sef_n_best, type = "diag")
