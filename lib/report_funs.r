kable_prop <- function(model) {
  colnames(model) <- make.unique(colnames(model))
  levels <- str_extract(colnames(model), "[0-9]+") %>%
    unique() %>%
    subset(!is.na(.))
  dep <- labels[attr(model, "dep")]
  n <- attr(model, "n")

  vars <- model["var"]

  model <- map(levels, function(i) {
    model[grep(paste0("_", i), colnames(model))]
  }) %>%
    bind_cols()

  model <- bind_cols(vars, model)


  # Find which row has NA and hence is ordinal effect
  # row_group <- which(apply(tab, 1, function(x) any(is.na(x))))[1]

  headers <- map_chr(
    levels,
    ~ paste("Higher than", .x, "(vs. lower than or equal to", .x, ")")
  )

  levels <- str_extract(colnames(model), "[0-9]+") %>%
    unique() %>%
    subset(!is.na(.))

  pos <- map_dbl(levels, function(l) {
    length(grep(paste0("_", l), colnames(model)))
  })

  p_headers <- map(n, function(x) {
    c("OR (95%CI)", paste0("p (n = ", x, ")"))
  })

  n_pairs <- pos / 2

  # Later sample must contain the previous samples
  # so if only one model at some level, it must be the first sample

  p_headers_all <- map(n_pairs, function(n) {
    unlist(p_headers[seq(n)])
  }) %>%
    unlist()

  pos <- c(1, pos)

  names(pos) <- c(" ", headers)

  model %>%
    mutate_at(
      vars(contains("p")),
      ~ ifelse(. < 0.05, cell_spec(scales::pvalue(.), "latex", bold = T), round(., digits = 2))
    ) %>%
    setNames(c(dep, p_headers_all)) %>%
    kable(format = "latex") %>%
    # pack_rows("Nominal effects", 1, row_group - 1) %>%
    # pack_rows("Ordinal effects", row_group, nrow(tab)) %>%
    add_header_above(pos)
  # %>%
  #     kable_styling(full_width = T)
}

kable_hurd <- function(model) {
  dep <- labels[attr(model, "dep")]
  n <- attr(model, "n")
  vars <- model["var"]

  model <- map(1:2, function(i) {
    model[grep(paste0("_", i), colnames(model))]
  }) %>%
    bind_cols()

  model <- bind_cols(vars, model)

  headers <- c(
    paste(dep, "(Y = 0)"),
    paste(dep, "(Y > 0)")
  )

  levels <- str_extract(colnames(model), "[0-9]+") %>%
    unique() %>%
    subset(!is.na(.))

  pos <- map_dbl(levels, function(l) {
    length(grep(paste0("_", l), colnames(model)))
  })

  pos <- c(1, pos)
  names(pos) <- c(" ", headers)

  p_headers <- map(n, function(x) {
    c("OR (95%CI)", paste0("p (n = ", x, ")"))
  }) %>%
    unlist()

  model %>%
    mutate_at(
      vars(contains("p")),
      ~ ifelse(. < 0.05, cell_spec(scales::pvalue(.), "latex", bold = T), round(., digits = 2))
    ) %>%
    setNames(c(
      "", p_headers,
      rep(c("exp(beta)", "p"), 2)
    )) %>%
    kable(format = "latex") %>%
    add_header_above(pos)
}

format_model <- function(model) {
  stats_gen <- function(tab) {
    tab["esti"] <- paste0(
      tab[["esti"]], " ",
      "(",
      paste(tab[["low"]], tab[["high"]], sep = "-"),
      ")"
    )
    tab <- tab[!colnames(tab) %in% c("low", "high")]
    return(tab)
  }

  tab <- model %>%
    as_tibble() %>%
    setNames(c("Parameters", "esti", "low", "high", "p")) %>%
    dplyr::mutate(time = Parameters) %>%
    filter(!grepl("Intercept|^[0-9]\\|[0-9]$", time)) %>%
    # mutate(p = p / adjust) %>%
    mutate_at(vars(c(esti, low, high)), ~ round(., digits = 2)) %>%
    stats_gen() %>%
    mutate(var = str_extract(time, "[a-z_]+[0-9]{0,2}")) %>%
    mutate(time = str_extract(time, "^[0-9]")) %>%
    # Give the NA column a number index. length(unique(.)) because it's
    # character
    mutate_at(vars(time), ~ ifelse(is.na(.), length(unique(.)), .)) %>%
    select(-Parameters) %>%
    pivot_wider(names_from = time, values_from = c(esti, p)) %>%
    mutate(var = recode(var, !!!labels)) %>%
    arrange(var)

  seq_levels <- unique(str_extract(colnames(tab)[-1], "[0-9]"))

  e_fst <- paste("esti", min(seq_levels), sep = "_")
  p_fst <- paste("p", min(seq_levels), sep = "_")

  ordered_cols <- map(
    seq_levels,
    function(num) {
      list(paste0(c("esti", "p"), "_", num))
    }
  ) %>%
    unlist()

  tab <- tab[c("var", ordered_cols)]
  tab[is.na(tab[[e_fst]]), e_fst] <- tab[is.na(tab[[e_fst]]), paste("esti", tail(seq_levels, 1), sep = "_")]
  tab[is.na(tab[[p_fst]]), p_fst] <- tab[is.na(tab[[p_fst]]), paste("p", tail(seq_levels, 1), sep = "_")]

  # Unless we fit a model with a three-level factor and it has no ordinal
  # effect (which is impossible) seq_levels will always be larger than 2
  if (length(seq_levels) > 2) {
    tab[paste("esti", tail(seq_levels, 1), sep = "_")] <- NULL
    tab[paste("p", tail(seq_levels, 1), sep = "_")] <- NULL
  }

  attr(tab, "n") <- attr(model, "n")

  return(tab)
}
