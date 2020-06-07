# Extract proportions significantly greater than 0 and reasonbale sample
# size

prop_sig <- map_lgl(items_sef, function(col) {
  n <- sum(!is.na(col))
  x <- sum(col, na.rm = T)
  diff(binom.test(x = x, n = n, p = 0, "greater")$conf.int) < 0.8
})


plot_dat_all <- items_sef[prop_sig] %>%
  summarise_all(mean, na.rm = T) %>%
  pivot_longer(everything(), values_to = "perct") %>%
  separate(name, into = c(NA, NA, "med", NA, NA, "sef")) %>%
  mutate_if(is.character, as.factor)

plot_dat_all_sex <- items_sef[prop_sig] %>%
  bind_cols(dat["sex"]) %>%
  group_by(sex) %>%
  summarise_all(mean, na.rm = T) %>%
  ungroup() %>%
  filter(!is.na(sex)) %>%
  pivot_longer(-sex, values_to = "perct") %>%
  separate(name, into = c(NA, NA, "med", NA, NA, "sef")) %>%
  mutate_if(is.character, as.factor)

prop_sig <- map(med_cat, function(cat) {
  items_sef_cat <- items_sef %>%
    select(contains(na.exclude(cat)))
  map_lgl(sef_names, function(sef) {
    items_sef_cat_sef <- items_sef_cat %>%
      select(contains(sef))
    n <- sum(items_sef_cat_sef, na.rm = T)
    x <- sum(items_sef_cat_sef == 1, na.rm = T)
    if (n > 0) {
      diff(binom.test(x = x, n = n, p = 0, "greater")$conf.int) < 0.8
    } else {
      return(FALSE)
    }
  })
}) %>%
  unlist()

plot_dat_sex <- imap_dfr(med_cat, function(cat, name) {
  items_sef %>%
    select(contains(na.exclude(cat))) %>%
    bind_cols(dat["sex"]) %>%
    pivot_longer(-sex) %>%
    separate(name, into = c(NA, NA, NA, NA, NA, "sef")) %>%
    mutate(cat = name) %>%
    group_by(sex, sef, cat) %>%
    filter(!is.na(sex)) %>%
    summarise_all(mean, na.rm = T) %>%
    ungroup()
})

get_cat_sef <- map_dfc(med_cat, function(cat) {
  items_sef_cat <- items_sef %>%
    select(contains(na.exclude(cat)))

  map_dfc(sef_names, ~ items_sef_cat %>%
    select(contains(.x)) %>%
    colMeans(na.rm = T) %>%
    mean(na.rm = T))
}) %>%
  setNames(colnames(med_cat) %>%
    rep(length(sef_names)) %>%
    paste(sef_names, sep = ".")) %>%
  pivot_longer(everything(), values_to = "perct")

plot_dat_cat <- items_sef %>%
  summarise_all(mean, na.rm = T) %>%
  pivot_longer(everything(), values_to = "perct") %>%
  separate(name, into = c(NA, NA, "med", NA, NA, "sef")) %>%
  mutate_if(is.character, as.factor)

# Can't plot dist matrix in a heatmap because the distances differ too much
dist_mat <- dist(med_mat, diag = FALSE, upper = FALSE) %>%
  as.matrix() %>%
  as_tibble() %>%
  mutate(X = colnames(.)) %>%
  pivot_longer(-X, names_to = "Y")

ggplot(dist_mat, aes(X, Y, fill = value)) +
  geom_tile()

# The facto that different distance type give the same center might mean
# that it fails at the first split?
xmeans <- Xmeans(norm_med_mat,
  kmax = nrow(norm_med_mat), dist.type = "cos",
)

cache("xmeans")
