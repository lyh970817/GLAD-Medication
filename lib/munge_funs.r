na_remove <- function(data) {
  mutate_if(data, is.numeric, ~ ifelse(. %in% c(-99, -88, -77), NA, .))
}

na_check <- function(data, id = NULL) {
  if (!is.null(id)) {
    sort(map_dbl(
      data[data[["ID"]] %in% id, ],
      ~ sum(is.na(.x) | .x %in% c(-77, -99, -88, "Seen but not answered", "Don't know", "Prefer not to say"))
    )) %>% rev()
  } else {
    sort(map_dbl(
      data,
      ~ sum(is.na(.x) | .x %in% c(
        -77, -99, -88,
        "Seen but not answered", "Don't know", "Prefer not to say"
      ))
    )) %>% rev()
  }
}

is_rowna <- function(data) {
  apply(data, 1, function(x) {
    all(is.na(x))
  })
}

rowScore <- function(data) {
  all_na <- is_rowna(data)
  scores <- data %>%
    mutate_if(~ is.numeric(.), abs) %>%
    rowSums(na.rm = T)
  scores[all_na] <- NA
  return(scores)
}

apply_lim <- function(v, mi, ma) {
  ifelse(abs(v) > ma | abs(v) < mi, NA, abs(v))
}

get_mean_sef <- function(items = items_sef, remv = NULL) {
  if (!is.null(remv)) items <- select(items, !contains(unname(remv)))
  items %>%
    rowSums(na.rm = T) %>%
    (function(x) x / n_med)
}
