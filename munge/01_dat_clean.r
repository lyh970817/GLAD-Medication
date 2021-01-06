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
# sheet_wsas <- GLAD_sheet("WSAS")[[1]]
# sheet_phy <- GLAD_sheet("PHY")[[1]]

# for (object in ls() %>% grep("sheet", ., v = T)) cache(object)

## Create derived variables

dat.dem <- GLAD_derive(dat.dem, sheet_dem)
dat.gad <- GLAD_derive(dat.gad, sheet_gad)
dat.phq <- GLAD_derive(dat.phq, sheet_phq)
dat.saspd <- GLAD_derive(dat.saspd, sheet_saspd)
dat.cidid <- GLAD_derive(dat.cidid, sheet_cidid)
dat.cidia <- GLAD_derive(dat.cidia, sheet_cidia)
dat.wsas <- GLAD_derive(dat.wsas, sheet_wsas)
# dat.cidia[["cidia.felt_worried"]] > 1
# sum(is.na(with(dat.cidia, eval(parse(text = sheet_cidia[["formula"]][164])))))

# sum(is.na(dat.cidia["cidia.lifetime_anxiety"]))
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
  dat.fam, dat.wsas, dat.phy, cidid.diag, cidia.diag
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
    "fam", "wsas", "phy", "cidid.diag", "cidia.diag"
  ))

# Deriving variables

browser()
## Demographics

# Have a look at ethnicity

dat_all_eth <- dat_list[["dem"]] %>%
  select(contains("ethnicity")) %>%
  select(-contains("numeric"))
table(dat_all_eth[1])
# Everyone is white

bmi <- dat_list[["dem"]][["dem.bmi_metric"]] %>%
  apply_lim(13, 60)

employment <- dat_list[["dem"]][["dem.employment"]]
levels(employment)[1] <- "Employed"
employment[employment %in% levels(employment)[c(1, 2, 3, 4, 6, 7, 8)]] <- "Employed"
employment <- factor(employment)

martial <- dat_list[["dem"]][["dem.marital_status"]]
levels(martial)[4] <- "Relationship"

martial[martial %in% levels(martial)[c(2, 3, 4, 5)]] <- "Relationship"
martial[martial %in% levels(martial)[c(5, 6, 7, 8)]] <- "Single"
martial <- factor(martial)

smoke_num <- dat_list[["dem"]][["dem.smoking_status_numeric"]]

smoke <- dat_list[["dem"]][["dem.smoking_yearssmoked"]] %>%
  ifelse(smoke_num == 0, 0, .)

score_wsas <- dat_list[["wsas"]][["wsas.total"]]
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

score_cidid <- dat_list[["cidid"]][["cidid.lifetime_depression"]]

## SASPD Score

score_saspd <- dat_list[["saspd"]][["saspd.total"]]

## CIDIP Score

score_cidip <- dat_list[["cidip"]][["cidip.total"]]

## CIDIA Score

score_cidia <- dat_list[["cidia"]][["cidia.lifetime_anxiety"]] %>%
  factor(levels = c(0, 1), labels = c(0, 1))

# PAD

score_pad <- dat_list[["pad"]][["pad.scr.total"]]

# MHD

score_mhd <- dat_list[["mhd"]][["mhd.total"]]
mhd <- dat_list[["mhd"]][c(43:59, 60:73)] %>%
  na_remove()


no_ans <- (mhd[["mhd.none_of_the_above_numeric"]] == 1 | mhd[["mhd.dont_know_numeric"]] == 1 | mhd[["mhd.prefernot_numeric"]] == 1) &
  (mhd[["mhd.none_of_the_above_2_numeric"]] == 1 | mhd[["mhd.dont_know_2_numeric"]] == 1 | mhd[["mhd.prefernot_2_numeric"]] == 1)

mhd <- modify(mhd, function(col) {
  col[is.na(col)] <- 0
  col[no_ans] <- NA
  return(col)
})

mhd <- mhd[-c(14:17, 28:31)]
mhd <- mhd[!names(mhd) %in% c("mhd.perinatal_dep_numeric", "mhd.PMDD_numeric")]

colnames(mhd) <- tolower(str_replace(colnames(mhd), "\\.", "_"))

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

# Total medication instances
tot_ins <- sum(n_med)

sef_perct <- map_dbl(sef_names, function(name) {
  items_name <- items_sef %>%
    select(contains(name))
  sum(items_name, na.rm = T) / tot_ins
})


cache("sef_perct")

# If prefer not to say for side effect q then true NA?
sef_index <- map(sef_names, function(name) {
  items_name <- items_sef %>%
    select(contains(name))
  rowSums(items_name, na.rm = T) / n_med
}) %>%
  setNames(paste0("sef_", tolower(sef_names))) %>%
  as_tibble()

sef_labels <- setNames(sef_labels, colnames(sef_index))

cache("sef_labels")
cache("sef_index")

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
  select(contains(".DUR")) %>%
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
  smoke,
  employment,
  martial,
  score_gad,
  score_phq,
  score_cidid = dat_list[["cidid.diag"]][[2]],
  score_cidia = dat_list[["cidia.diag"]][[2]],
  # score_cidip,
  # score_saspd,
  score_pad,
  score_mhd,
  score_wsas,
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

dat <- dat %>%
  bind_cols(mhd, sef_index)

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
  "Smoking",
  "Employment",
  "Martial",
  "Current anxiety (GAD)",
  "Current depression (PHQ)",
  "Lifetime depression (CIDID)",
  "Lifetime anxiety (CIDIA)",
  # "Personality disorder",
  "Panic disorder symptoms (N) (PAD)",
  "Psychiatric diagnoses (N) (MHD)",
  "Work and adjustment (WSAS)",
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
  "Overall benefits (MED)",
  "Overall side effects (MED)",
  "Number of Best Aspects (MED)",
  # "Cigarettes Per Day",
  # "Second Hand Smoking",
  # "Smoking Years",
  "MDD",
  # "Perinatal depression",
  # "PMDD",
  "Bipolar",
  "GAD",
  "Social anxiety",
  "Specifc phobia",
  "Agoraphobia",
  "Panic disorder",
  "Panic attacks",
  "PTSD",
  "OCD",
  "BDD",
  "AN",
  "Atypical AN",
  "BN",
  "BED",
  "Schizophrenia",
  "Schizoaffective",
  "Psychosis",
  "Personality Disorder",
  "ASD",
  "ADHD",
  sef_labels
) %>% setNames(colnames(dat))
cache("labels")

cache("dat_list")

# Determine who are taking antidepressants cureently and are depressed
start <- items_add %>%
  select(contains("START")) %>%
  map_df(apply_lim, 8, 100)

dur <- items_add %>%
  select(contains(".DUR")) %>%
  # First antidepressant was approved 62 years ago. See docs.
  map_df(apply_lim, 0, 62)

last_treatment_diff <- dat_uncut$age - map2_df(start, dur, ~ .x + .y)
last_treatment_diff <- map_df(map2_df(start, dur, ~ .x + .y), ~ dat_uncut$age - .x)

last_treatment_now <- apply(last_treatment_diff, 1, function(x) {
  x[which.min(x)[1]]
}) %>% unlist()

last_treatment_now <- last_treatment_now <= 0

cache("last_treatment_now")

# Find percentage of discontinuation  for each antidepressants
dat_med <- dat_list[["med"]]
add_stop <- dat_med[grep("ADD.*SE\\.STOP", names(dat_med))]

stop_pert <- colMeans(add_stop, na.rm = T)
stop_order <- order(stop_pert, decreasing = T)
stop_n <- map_dbl(add_stop, ~ sum(!is.na(.)))

stop_n <- stop_n[stop_order]
stop_pert <- stop_pert[stop_order]

cache("stop_n")
cache("stop_pert")


# sum(table(last_treatment_now[dat_uncut$score_phq >= 10 | dat_uncut$score_gad >= 10])[1:15])

# early_dur <- apply(dur, 1, function(x) {
#   x[which.min(x)]
# }) %>% unlist()
