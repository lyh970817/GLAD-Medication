as_numeric <- function(f) {
  if (inherits(f, "factor")) {
    as.numeric(levels(f)[f])
  } else {
    f
  }
}

fit_hurdle <- function(dat, dep, indeps) {

  # Create variable for logistic regression
  lg <- paste(dep, "l", sep = "_")
  dat[lg] <- ifelse(dat[[dep]] > 0, 1, 0)

  fm_zero <- as.formula(paste(lg, "~", paste(indeps, collapse = "+")))
  fm <- as.formula(paste(dep, "~", paste(indeps, collapse = "+")))

  # Keep observations larger than 0 for gamma glm
  dat_gamma <- dat[dat[[dep]] > 0, ]

  m0 <- glm(fm_zero, data = dat, family = binomial(link = logit))
  m <- glm(fm, data = dat_gamma, Gamma(link = "inverse"))

  # print(termplot(m0, partial.resid = T, data = dat))
  # print(termplot(m, partial.resid = T, data = dat_gamma))

  ms <- imap(list(m0, m), function(mod, i) {
    parameters(mod) %>%
      as_tibble() %>%
      mutate_at(vars(Coefficient, CI_low, CI_high), exp) %>%
      select(c("Parameter", "Coefficient", "CI_low", "CI_high", "p")) %>%
      mutate(Parameter = paste(i, Parameter, sep = "."))
  }) %>%
    setNames(c("logistic", "gamma"))

  attr(ms, "dep") <- dep
  return(ms)
}

nom_refit <- function(m, t) {
  nom_sig <- rownames(t)[which(t[[5]] <= 0.05)]
  if (length(nom_sig) == 0) {
    return(m)
  }
  dat <- m$model
  dep <- colnames(dat)[1]
  if (dep == "remission") {
    browser()
  }
  dat[nom_sig] <- dat[nom_sig] %>%
    mutate_if(is.numeric, scale, scale = FALSE)
  nom_nosig <- rownames(t)[which(t[[5]] > 0.05)]

  clm(as.formula(paste(dep, "~", paste(nom_nosig, collapse = "+"))),
    nominal = as.formula(paste("~", paste(nom_sig, collapse = "+"))),
    link = "logit",
    data = dat
  )
}

fit_prop <- function(dat, dep, indeps) {
  # nominal_test.clm is not working properly with environment so global
  # assignment required
  fm <<- paste(dep, "~", paste(indeps, collapse = "+"))
  mod <- clm(as.formula(fm), data = dat, link = "logit")

  nom_test <- nominal_test_clm(mod)
  mod <- nom_refit(mod, nom_test)

  ms <- parameters(mod) %>%
    as_tibble() %>%
    mutate_at(vars(Coefficient, CI_low, CI_high), exp) %>%
    select(c("Parameter", "Coefficient", "CI_low", "CI_high", "p"))

  attr(ms, "dep") <- dep
  return(ms)
}

# coeff_ci <- function(coeff, cis) {
#   dimnames(coeff)[[2]][c(2, 3)] <- dimnames(cis)[[2]]
#   coeff[, 1] <- exp(coeff[, 1])
#   coeff[, c(2, 3)] <- exp(cis)

#   m <- coeff

#   return(m)
# }

fit_uni_hurdle <- function(dat, dep, indeps) {
  mod_list <- list()
  for (i in seq_along(indeps)) {
    mod_list[[i]] <- fit_hurdle(dat, dep, indeps[i])
  }
  ms <- bind_rows(flatten(mod_list))
  attr(ms, "dep") <- attr(mod_list[[1]], "dep")
  return(ms)
}

fit_uni_prop <- function(dat, dep, indeps) {
  mod_list <- list()
  for (i in seq_along(indeps)) {
    mod_list[[i]] <- fit_prop(dat, dep, indeps[i])
  }
  bind_rows(flatten(mod_list))
}

fit_eff_sef <- function(dat, eff_deps, sef_indep) {
  ms <- map(eff_deps, function(dep) {
    fit_prop(dat, dep, c(sef_indep, covs)) %>%
      filter(grepl(sef_indep, Parameter))
  }) %>%
    bind_rows()
  attr(ms, "indep") <- sef_indep
  return(ms)
}
