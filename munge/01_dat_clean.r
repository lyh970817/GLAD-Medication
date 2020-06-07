# googlesheets4::sheets_auth(
#   email = gargle::gargle_oauth_email(),
#   path = "~/Yandex.Disk/Projects/Research/GLAD/GLAD-Questionnaires/analysis_scripts/Response-and-Side-Effects/Medication_Lin/config/glad-dict.json",
#   scopes = "https://www.googleapis.com/auth/spreadsheets",
#   cache = gargle::gargle_oauth_cache(),
#   use_oob = gargle::gargle_oob_default(),
#   token = NULL
# )

# sheet_dem <- GLAD_sheet("DEM")[[1]]
# sheet_phq <- GLAD_sheet("PHQ")[[1]]
# sheet_gad <- GLAD_sheet("GAD")[[1]]
# sheet_fam <- GLAD_sheet("FAM")[[1]]
# sheet_pad <- GLAD_sheet("PAD")[[1]]
# sheet_cidip <- GLAD_sheet("CIDIP")[[1]]
# sheet_saspd <- GLAD_sheet("SASPD")[[1]]
# sheet_med <- GLAD_sheet("MED")[[1]]
# sheet_cidid <- GLAD_sheet("CIDID")[[1]]
# sheet_cidia <- GLAD_sheet("CIDIA")[[1]]
# sheet_pad <- GLAD_sheet("PAD")[[1]]
# sheet_mhd <- GLAD_sheet("MHD")[[1]]

for (object in ls() %>% grep("sheet", ., v = T)) cache(object)

## Create derived variables

dat.dem <- GLAD_derive(dat.dem, sheet_dem)
dat.gad <- GLAD_derive(dat.gad, sheet_gad)
dat.phq <- GLAD_derive(dat.phq, sheet_phq)
dat.saspd <- GLAD_derive(dat.saspd, sheet_saspd)
dat.cidid <- GLAD_derive(dat.cidid, sheet_cidid)
dat.cidia <- GLAD_derive(dat.cidia, sheet_cidia)
# dat.cidip <- GLAD_derive(dat.cidip, sheet_cidip)
dat.pad <- GLAD_derive(dat.pad, sheet_pad)

# map_dbl(dat.mhd, ~ sum(is.na(.x)))
# Why would there be so many NAs for the few diagnoses? Especially panic
# disorder
dat.mhd <- gladfunctions::GLAD_derive(dat.mhd, sheet_mhd)

# This is the 'None of the above' option we removed
dat.med["MED.ADD.1.0_0"] <- NULL

# Merge datasets

dat_list <- list(
  dat.dem, dat.med, dat.gad, dat.phq, dat.cidip,
  dat.saspd, dat.cidid, dat.cidia, dat.pad, dat.mhd,
  dat.fam
)

## Get medication ids

### This is the last mandatory question for all participants. If this
### question is NA it means participants did not finish the whole
### questionnaire and we will discard them.

complete <- which(!is.na(dat.med[["MED.POS.BEST.RDS"]]))
ids <- dat.med["ID"][complete, ]

# No all NA participants found
# row_na <- which(is_rowna(dat.med[-c(1, 2, 3)]))
# ids <- ids[-row_na, ]

## Merge

dat_list <- map(dat_list, function(dat) {
  left_join(ids, dat, by = "ID")
}) %>%
  setNames(c(
    "dem", "med", "gad", "phq", "cidip",
    "saspd", "cidid", "cidia", "pad", "mhd",
    "fam"
  ))

# Deriving variables

## Demographics

# Have a look at ethnicity

dat_all_eth <- dat_list[["dem"]] %>%
  select(contains("ethnicity")) %>%
  select(-contains("numeric"))
table(dat_all_eth[1])
# Everyone is white

bmi <- dat_list[["dem"]][["dem.bmi_metric"]] %>%
  apply_lim(13, 60)

smoke_num <- dat_list[["dem"]][["dem.smoking_status_numeric"]]

smoke_perday <- dat_list[["dem"]][["dem.smoking_perday"]] %>%
  ifelse(is.na(.), 0, .)

smoke_sec <- dat_list[["dem"]][["dem.smoking_secondhand"]]

smoke_years <- dat_list[["dem"]][["dem.smoking_yearssmoked"]] %>%
  ifelse(is.na(.), 0, .)

## GAD Score

### We will compare total sample GAD score with medicaion GAD score

score_gad_all <- dat.gad[["gad.total_score"]]
cache("score_gad_all")

score_gad <- dat_list[["gad"]][["gad.total_score"]]

## PHQ Score

score_phq_all <- dat.phq[["phq.new.total"]]
cache("score_phq_all")

score_phq <- dat_list[["phq"]][["phq.new.total"]]

# CIDID Score

score_cidid <- dat_list[["cidid"]][["cidid.depressed_symptoms"]]

## SASPD Score

score_saspd <- dat_list[["saspd"]][["saspd.total"]]

## CIDIP Score

score_cidip <- dat_list[["cidip"]][["cidip.total"]]

## CIDIA Score

score_cidia <- dat_list[["cidia"]][["cidia.lifetime_anxiety"]] %>%
  factor(levels = c(0, 1), labels = c("No", "Yes"))

# PAD

score_pad <- dat_list[["pad"]][["pad.scr.total"]]

# MHD

score_mhd <- dat_list[["mhd"]][["mhd.total"]]

# FAM Number of relatives with psychiatric diagnoses

psychiatric_diag <- c(
  "FAM.depression_number", "FAM.anxiety_number", "FAM.panicattacks_number",
  "FAM.antepostnataldepression_number", "FAM.socialanxiety_number",
  "FAM.specificphobia_number", "FAM.mania_number", "FAM.autism_number",
  "FAM.ptsd_number", "FAM.ocd_number", "FAM.psychosisother_number",
  "FAM.otherobsessivecompulsive_number", "FAM.personalitydisorder_number",
  "FAM.overeating_number", "FAM.mentalhealthother_number",
  "FAM.agoraphobia_number", "FAM.anorexia_number",
  "FAM.schizophrenia_number", "FAM.bulimia_number"
)

no_fam <- !dat_list[["fam"]][["ID"]] %in% dat.fam[["ID"]]

fam <- dat_list[["fam"]][psychiatric_diag] %>%
  na_remove() %>%
  rowSums(na.rm = T) %>%
  ifelse(no_fam, NA, .) %>%
  # Is this plausible?
  apply_lim(0, 30)

## Medications

dat_list[["med"]] <- dat_list[["med"]] %>% na_remove()

items_add <- dat_list[["med"]] %>%
  select(contains("ADD")) %>%
  select(-contains("OTHERADD"))

### Average effectiveness

mean_eff <- items_add %>%
  select(contains("EFF")) %>%
  rowMeans(na.rm = T)

### Average side effects

#### See diagnostics #1
items_med <- dat_list[["med"]][, 4:32] %>%
  select(-contains("OTHERADD")) %>%
  mutate_all(~ ifelse(is.na(.x), 0, .x))

med_names <- get_name(colnames(items_med), 3)
cache("med_names")

n_med <- items_med %>%
  select(-contains("OTHERADD")) %>%
  rowScore()

cache("n_med")

# This should be zero if they indicate no side effect (which seems to have
# been recoded on Qualtircs)

items_sef <- items_add %>%
  select(contains("SEF"))

sef_names <- get_name(colnames(items_sef), 6) %>% unique()
sef_labels <- GLAD_getdescr(colnames(items_sef), sheet_med) %>%
  get_descr(2) %>%
  unique()

cache("sef_names")
cache("sef_labels")

# Total medication instances
tot_ins <- sum(n_med)

sef_perct <- map_dbl(sef_names, function(name) {
  items_name <- items_sef %>%
    select(contains(name))
  sum(items_name, na.rm = T) / tot_ins
})



cache("sef_perct")

se_20_rm <- sef_names[sef_perct >= 0.2] %>%
  setNames(sef_labels[sef_perct >= 0.2]) %>%
  .[. != "RSX"]

cache("se_20_rm")

se_15_rm <- sef_names[sef_perct >= 0.15] %>%
  setNames(sef_labels[sef_perct >= 0.15]) %>%
  .[!. %in% c("RSX", "SUT")]

cache("se_15_rm")

se_10_rm <- sef_names[sef_perct >= 0.1] %>%
  setNames(sef_labels[sef_perct >= 0.1]) %>%
  .[!. %in% c("RSX", "SUT")]

cache("se_10_rm")

sef_fill <- rep(NA, length(sef_names))
sef_fill[sef_names %in% se_10_rm] <- "Discarded at 10%"
sef_fill[sef_names %in% se_15_rm] <- "Discarded at 15%"
sef_fill[sef_names %in% se_20_rm] <- "Discarded at 20%"

sef_perct_dat <- tibble(
  sef_perct = sef_perct,
  sef_labels = sef_labels,
  sef_fill = factor(sef_fill)
)

mean_sef <- get_mean_sef(items = items_sef)
mean_sef_20 <- get_mean_sef(items_sef, se_20_rm)
mean_sef_15 <- get_mean_sef(items_sef, se_15_rm)
mean_sef_10 <- get_mean_sef(items_sef, se_10_rm)

### Earliest treatment start age

start <- items_add %>%
  select(contains("START")) %>%
  map_df(apply_lim, 8, 100) %>%
  apply(1, min, na.rm = T)

### Time on antidepressants

dur <- items_add %>%
  select(contains("DUR")) %>%
  # First antidepressant was approved 62 years ago. See docs.
  map_df(apply_lim, 0, 62) %>%
  rowScore()

### Likelihood of remission

remission <- items_add %>%
  select(contains("FURSYM")) %>%
  rowMeans(na.rm = T)

### Likelihood of intolerance

intolerance <- items_add %>%
  select(contains("STOP")) %>%
  rowMeans(na.rm = T)

### Average improvement duration

mean_imprvdur <- items_add %>%
  select(contains("IMPRVDUR")) %>%
  rowMeans(na.rm = T)

### Overall benefit

ben <- dat_list[["med"]][["MED.POS.BENEFIT"]] %>%
  # Why do we have 0 for a 1-5 item?
  ifelse(. == 0, NA, .) %>%
  factor(ordered = T)

### Overall side effects

sef <- dat_list[["med"]][["MED.SE.HARM"]] %>%
  # reverse coding
  (function(x) 6 - x) %>%
  # See diagnostics #7
  ifelse(. == 0, NA, .) %>%
  factor(ordered = T)

### Number of best aspects

best <- dat_list[["med"]] %>%
  select(contains("BEST")) %>%
  select(-contains("OTHER"))

# na.rm =  T because if someone is all NA it means there are zero best
# aspects
n_best <- rowSums(best, na.rm = T) %>%
  factor(ordered = T)

dat <- tibble(
  sex = dat_list[["dem"]][["Sex"]],
  age = dat_list[["dem"]][["Age"]] %>% apply_lim(18, 110),
  score_gad,
  score_phq,
  # score_cidid,
  score_cidia,
  # score_cidip,
  # score_saspd,
  score_pad,
  score_mhd,
  fam,
  bmi,
  # smoke_perday,
  # smoke_sec,
  # smoke_years,
  mean_eff,
  mean_sef,
  # mean_sef_20,
  # mean_sef_15,
  # mean_sef_10,
  start,
  dur,
  remission,
  intolerance,
  mean_imprvdur,
  ben,
  sef,
  n_best
) %>%
  mutate_if(is.numeric, ~ ifelse(!is.finite(.), NA, .))

dat_uncut <- dat
cache("dat_uncut")

dat <- dat %>%
  mutate(
    mean_eff = cut(mean_eff, 6, labels = 1:6, ordered_result = T),
    remission = cut(remission, 3, labels = 1:3, ordered_result = T),
    intolerance = cut(intolerance, 3, labels = 1:3, ordered_result = T)
  )

cache("dat")

labels <- c(
  "Sex",
  "Age",
  "Current anxiety (GAD)",
  "Current depression (PHQ)",
  # "Lifetime depression",
  "Lifetime anxiety (CIDIA)",
  # "Personality disorder",
  "Panic disorder symptoms (N) (PAD)",
  "Psychiatric diagnoses (N) (MHD)",
  "Relatives with psychitric diagnoses (N) (FAM)",
  "BMI (DEM)",
  "Mean effectiveness (MED)",
  "Mean side effects (MED)",
  # "Mean side effects (SE_20)",
  # "Mean side effects (SE_15)",
  # "Mean side effects (SE_10)",
  "Age at earliest treatment (MED)",
  "Time on Antidepressants (MED)",
  "Likelihood of remission (MED)",
  "Likelihood of intolerance (MED)",
  "Mean improvement duration (MED)",
  "Overall Benefits (MED)",
  "Overall side effects (MED)",
  "Number of Best Aspects (MED)"
  # "Cigarettes Per Day",
  # "Second Hand Smoking",
  # "Smoking Years"
) %>% setNames(colnames(dat))

cache("labels")

cache("dat_list")
