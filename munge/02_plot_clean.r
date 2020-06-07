# We can no longer ignore the difference between NA and zeroes for
# computing the mean for plotting (because for computing mean rahter than
# sum requires we have the right denominator, which is the number of
# non-missing responses) so we recode those who answered yes to a
# medication question but are NA in any side effect of that medication to
# zero.

# Shoule this be moved to the main cleaning script?
items_sef <- map_dfc(med_names, function(med) {
  items_med_col <- select(items_med, contains(med))
  items_sef %>%
    select(contains(med)) %>%
    modify(
      ~ ifelse(is.na(.x) & items_med_col == 1, 0, .x)
    ) %>%
    return()
})
cache("items_sef")

plot_dat <- imap_dfr(med_cat, function(cat_col, cat_name) {
  items_sef_cat <- items_sef %>%
    select(contains(na.exclude(cat_col)))

  map_dfr(sef_names, function(sef) {
    items_sef_cat_sef <- items_sef_cat %>%
      select(contains(sef))

    n <- items_sef_cat_sef %>%
      summarise_all(~ sum(!is.na(.))) %>%
      rowSums()

    x <- items_sef_cat_sef %>%
      summarise_all(sum, na.rm = T) %>%
      rowSums()

    # Note that this CI (from `binom.test`) is is not the asymptoic
    # (Z-score) CI but the exact Clopper-Pearson interval so works (?) for
    # small samples but is conservative. See the relevant Wikipedia.

    ci_lgl <- test_ci(x, n, 0.4)
    perct <- (x / n) %>% ifelse(ci_lgl & . > 0, ., NA)
    # Make a row
    r <- tibble(sef = sef, cat = cat_name, perct = perct)
    return(r)
  })
})

cache("plot_dat")

plot_dat_sex <- imap_dfr(med_cat, function(cat_col, cat_name) {
  items_sef_cat <- items_sef %>%
    select(contains(na.exclude(cat_col))) %>%
    bind_cols(dat["sex"]) %>%
    group_by(sex)

  map_dfr(sef_names, function(sef) {
    items_sef_cat_sef <- items_sef_cat %>%
      select(contains(sef))

    n <- items_sef_cat_sef %>%
      summarise_all(~ sum(!is.na(.))) %>%
      filter(!is.na(sex)) %>%
      select(-sex) %>%
      rowSums()

    x <- items_sef_cat_sef %>%
      summarise_all(sum, na.rm = T) %>%
      filter(!is.na(sex)) %>%
      select(-sex) %>%
      rowSums()

    ci_lgl <- c(test_ci(x[1], n[1], 0.4), test_ci(x[2], n[2], 0.4))
    perct <- (x / n) %>% ifelse(ci_lgl & . > 0, ., NA)
    # Make two rows (one for each gender)
    rs <- tibble(
      sex = c("Male", "Female"), sef = sef,
      cat = cat_name, perct = perct
    )
    return(rs)
  })
})

cache("plot_dat_sex")
