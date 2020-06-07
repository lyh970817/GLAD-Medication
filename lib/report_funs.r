kable_prop <- function(model) {
  tab <- format_model(model)
  dep <- labels[attr(model, "dep")]

  # Find which row has NA and hence is ordinal effect
  row_group <- which(apply(tab, 1, function(x) any(is.na(x))))[1]
  levels <- str_extract(colnames(tab), "[0-9]+") %>%
    unique() %>%
    subset(!is.na(.))

  headers <- map_chr(
    levels,
    ~ paste("Lower than or equal to", .x, "(vs. higher than", .x, ")")
  )
  pos <- c(1, rep(2, length(levels)))
  names(pos) <- c(" ", headers)

  tab %>%
    mutate_at(
      vars(contains("p")),
      ~ ifelse(. < 0.05, cell_spec(scales::pvalue(.), "html", bold = T), .)
    ) %>%
    setNames(c(dep, rep(c("OR (95%CI)", "p"), 5))) %>%
    kable(format = "html", escape = F) %>%
    pack_rows("Nominal Effects", 1, row_group - 1) %>%
    pack_rows("Ordinal Effects", row_group, nrow(tab)) %>%
    add_header_above(pos) %>%
    kable_styling(full_width = T)
}

kable_hurd <- function(model) {
  tab <- format_model(model)
  dep <- labels[attr(model, "dep")]

  # Find which row has NA and hence is ordinal effect
  headers <- c(
    paste(dep, "(Y = 0)"),
    paste(dep, "(Y > 0)")
  )
  pos <- c(1, rep(2, 2))
  names(pos) <- c(" ", headers)
  tab %>%
    mutate_at(
      vars(contains("p")),
      ~ ifelse(. < 0.05, cell_spec(scales::pvalue(.), "html", bold = T), .)
    ) %>%
    setNames(c("", "OR (95%CI)", "p", "exp(beta)", "p")) %>%
    kable(format = "html", escape = F) %>%
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
    setNames(c("esti", "low", "high", "p")) %>%
    mutate(time = rownames(model)) %>%
    filter(!grepl("Intercept", time)) %>%
    mutate_if(is.numeric, ~ round(., digits = 2)) %>%
    stats_gen() %>%
    mutate(var = str_extract(time, "[a-z_]+[0-9]{0,2}")) %>%
    mutate(time = str_extract(time, "^[0-9]")) %>%
    # Give the NA column a number index. length(unique(.)) because it's
    # character
    mutate_at(vars(time), ~ ifelse(is.na(.), length(unique(.)), .)) %>%
    pivot_wider(names_from = time, values_from = c(esti, p)) %>%
    mutate(var = recode(var, !!!labels))

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

  return(tab)
}
