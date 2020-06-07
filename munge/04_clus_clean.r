med_profile <- map_df(med.profile[2:ncol(med.profile)], function(col) {
  nums <- str_extract(col[grep(">", col)], "[0-9]+")
  col[grep(">", col)] <- nums
  col <- as.numeric(col)
  col[is.na(col)] <- 0
  return(col)
}) %>%
  bind_cols(med.profile[1]) %>%
  select("Compound", everything()) %>%
  select(-c(ncol(.), ncol(.) - 1))

med_mat <- as.matrix(med_profile[, 2:ncol(med_profile)])
dimnames(med_mat)[[1]] <- med_profile[["Compound"]]

cache("med_mat")
