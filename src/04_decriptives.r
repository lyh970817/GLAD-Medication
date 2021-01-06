setwd("../")
library(ProjectTemplate)
load.project()
library(polycor)
library(ggcorrplot)


hists_var_uncut <- dat_uncut %>%
  select(mean_eff, intolerance, remission) %>%
  plyr::rename(labels[names(labels) %in% colnames(.)]) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram() +
  labs(
    x = " ",
    y = "Count"
  )
cache("hists_var_uncut")

hists_var_cut <- dat %>%
  select(mean_eff, intolerance, remission) %>%
  plyr::rename(labels[names(labels) %in% colnames(.)]) %>%
  mutate_all(as.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram() +
  labs(
    x = " ",
    y = "Count"
  )
cache("hists_var_cut")

hists <- dat %>%
  plyr::rename(labels) %>%
  mutate_all(as.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram() +
  labs(
    x = " ",
    y = "Count"
  )
cache("hists")

cor_mat <- hetcor(dat, use = "pairwise.complete.obs", std.err = F)$correlations
dimnames(cor_mat) <- list(labels, labels)
cache("cor_mat")

cor_plot <- ggcorrplot(cor_mat,
  type = "lower",
  lab = TRUE,
  title = "Correlations Between All Variables in the Dataset"
) +
  labs(subtitle = "Pearson product-moment between numeric variables, polyserial between numeric and ordinal variables, and polychoric between ordinal variables")
cache("cor_plot")

# There is a whole bunch of interesting correlations and it seems that
# according to the literature polyserial correlations can be severely
# unbiased towards zero when the continuous variable is skewed which is the
# case for a number of variables in the data (especially for SE_20 etc.) so
# these correlations are to be interpreted as underestimates.

# this perhaps is worth writing a function for?
# Note that if we don't use range this can be changed to pivot_longer
# followed by pivot_wider which is more straightforward

gender_t_test <- imap_dbl(
  bind_cols(n_med = n_med, dat[-1]), function(col, name) {
    male <- as.numeric(col[dat[["sex"]] == "Male"])
    female <- as.numeric(col[dat[["sex"]] == "Female"])
    t.test(male, female)$p.value
  }
)

sum_tab <- dat %>%
  mutate_at(vars(-sex), as.numeric) %>%
  # Insert n_med
  mutate(n_med = n_med) %>%
  # Reorder
  select("Number of antidepressants taken" = n_med, age, everything()) %>%
  filter(!is.na(sex)) %>%
  pivot_longer(-sex) %>%
  # Change name to an ordered factor so summarize does not sort them
  group_by(name = fct_inorder(name), sex) %>%
  summarize_all(list(
    mean = ~ mean(., na.rm = T),
    median = ~ median(., na.rm = T),
    sd = ~ sd(., na.rm = T),
    range = ~ paste(
      prettyNum(range(., na.rm = T),
        digits = 2
      ),
      collapse = "-"
    ),
    n_valid = ~ sum(!is.na(.))
  )) %>%
  # Format the sex suffix to lower case
  mutate(sex = tolower(sex)) %>%
  pivot_wider(
    names_from = sex,
    values_from = colnames(.)[3:ncol(.)]
  ) %>%
  ungroup() %>%
  mutate(
    valid_pct = (n_valid_male + n_valid_female) / nrow(.),
    pval = gender_t_test
  ) %>%
  # Reorder columns
  select(name, matches("_male"), pval, matches("_female"), valid_pct)

cache("sum_tab")
