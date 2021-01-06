as_numeric <- function(f) {
  if (inherits(f, "factor")) {
    as.numeric(levels(f)[f])
  } else {
    f
  }
}

fit_hurdle <- function(dat, dep, indeps) {
  dat <- na_omit_dat(dat, dep, indeps)
  indeps <- indeps[indeps %in% colnames(dat)]
  n <- nrow(dat)

  # Create variable for logistic regression
  lg <- paste(dep, "l", sep = "_")
  dat[lg] <- ifelse(dat[[dep]] > 0, 1, 0)

  fm_zero <- as.formula(paste(lg, "~", paste(indeps, collapse = "+")))
  fm <- as.formula(paste(dep, "~", paste(indeps, collapse = "+")))

  # Keep observations larger than 0 for gamma glm
  dat_gamma <- dat[dat[[dep]] > 0, ]

  m0 <- glm(fm_zero, data = dat, family = binomial(link = logit))
  m <- glm(fm, data = dat_gamma, Gamma(link = "log"))

  # print(termplot(m0, partial.resid = T, data = dat))
  # print(termplot(m, partial.resid = T, data = dat_gamma))

  ms <- imap(list(m0, m), function(mod, i) {
    parameters(mod) %>%
      as_tibble() %>%
      mutate_at(vars(Coefficient, CI_low, CI_high), exp) %>%
      select(c("Parameter", "Coefficient", "CI_low", "CI_high", "p")) %>%
      mutate(Parameter = paste(i, Parameter, sep = "."))
  }) %>%
    bind_rows()
  # setNames(c("logistic", "gamma"))

  attr(ms, "dep") <- dep
  attr(ms, "n") <- n

  return(ms)
}

nom_refit <- function(m, t) {
  t <- t[-1, ]
  nom_sig <- rownames(t)[which(t[[5]] <= 0.05)]
  if (length(nom_sig) == 0) {
    return(m)
  }
  dat <- m$model
  dep <- colnames(dat)[1]
  dat[nom_sig] <- dat[nom_sig] %>%
    mutate_if(is.numeric, scale, scale = FALSE)
  nom_nosig <- rownames(t)[which(t[[5]] > 0.05 | is.na(t[[5]]))]

  if (length(nom_nosig) == 0) {
    tryCatch(vglm(as.formula(paste(dep, "~", paste(nom_sig, collapse = "+"))), dat,
      family = cumulative(link = "logitlink", parallel = FALSE, reverse = TRUE)
    ), error = function(e) {
      browser()
    })
  }
  else {
    clm(as.formula(paste(dep, "~", paste(nom_nosig, collapse = "+"))),
      nominal = as.formula(paste("~", paste(nom_sig, collapse = "+"))),
      link = "logit",
      data = dat
    )
  }
}

fit_prop <- function(dat, dep, indeps) {
  dat <- na_omit_dat(dat, dep, indeps)
  indeps <- indeps[indeps %in% colnames(dat)]
  n <- nrow(dat)
  map(dat, table)

  tss <- function(object, scope, trace = FALSE, ...)
                  ### Test nominal effects for all (or selected) terms in location
  ### and scale formulas.
  {
    ## get scope: vector of terms names which to add to nominal:
    termsnm <- attr(object$terms, "term.labels")
    if (!is.null(object$S.terms)) {
      termsnm <- union(termsnm, attr(object$S.terms, "term.labels"))
    }
    if (!missing(scope) && !is.null(scope)) {
      if (!is.character(scope)) {
        scope <- attr(
          terms(update.formula(object, scope)),
          "term.labels"
        )
      }
      if (!all(match(scope, termsnm, 0L) > 0L)) {
        stop("scope is not a subset of term labels")
      }
    } else {
      scope <- termsnm
    }
    if (!is.null(object$nom.terms)) {
      scope <- scope[!scope %in% attr(
        object$nom.terms,
        "term.labels"
      )]
    }
    if (!length(scope)) {
      message("\nno additional terms to add to nominal\n")
    }
    env <- environment(formula(object))
    ## get list of (updated) nominal formulas:
    nomforms <- if (!is.null(object$call$nominal)) {
      lapply(scope, function(tm) {
        update.formula(
          old = formula(object$nom.terms),
          new = as.formula(paste("~. + ", tm))
        )
      })
    } else {
      lapply(scope, function(tm) {
        as.formula(paste("~", tm), env = env)
      })
    }
    ns <- length(scope)
    ## results matrix:
    ans <- matrix(
      nrow = ns + 1L, ncol = 3L,
      dimnames = list(
        c("<none>", scope),
        c("df", "logLik", "AIC")
      )
    )
    ans[1L, ] <- c(object$edf, object$logLik, AIC(object))
    n0 <- nobs(object)
    ## for all terms in scope:
    i <- 1
    for (i in seq(ns)) {
      if (trace) {
        cat("trying +", scope[i], "\n", sep = " ")
        utils::flush.console()
      }
      ## update and fit model with nominal effect added:
      nfit <- try(update(object,
        nominal = nomforms[[i]],
        convergence = "silent"
      ), silent = TRUE)
      ## model may not be identifiable or converge:
      if (!inherits(nfit, "try-error") &&
        ### NOTE: non-negative convergence codes indicate that the likelihood
        ### is correctly determined:
        nfit$convergence$code >= 0) {
        ans[i + 1L, ] <- c(nfit$edf, nfit$logLik, AIC(nfit))
        nnew <- nobs(nfit)
        if (all(is.finite(c(n0, nnew))) && nnew != n0) {
          stop("number of rows in use has changed: remove missing values?")
        }
      }
    }
    dfs <- ans[, 1L] - ans[1L, 1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, logLik = ans[, 2L], AIC = ans[, 3L])
    rownames(aod) <- rownames(ans)
    ## compute likelihood ratio statistic and p-values:
    LR <- 2 * (ans[, 2L] - ans[1L, 2L])
    LR[1L] <- NA
    nas <- !is.na(LR)
    P <- LR
    P[nas] <- pchisq(LR[nas], dfs[nas], lower.tail = FALSE)
    aod[, c("LRT", "Pr(>Chi)")] <- list(LR, P)
    head <- c(
      "Tests of nominal effects",
      paste("\nformula:", Deparse(formula(object$terms)))
    )
    if (!is.null(object$call$scale)) {
      head <- c(head, paste(
        "scale:  ",
        Deparse(formula(object$S.terms))
      ))
    }
    if (!is.null(object$call$nominal)) {
      head <- c(head, paste(
        "nominal:",
        Deparse(formula(object$nom.terms))
      ))
    }
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
  }

  # nominal_test.clm is not working properly with environment so global
  # assignment required
  fm <<- paste(dep, "~", paste(indeps, collapse = "+"))
  mod <<- clm(as.formula(fm), data = dat, link = "logit")
  nom_test <- tss(mod)

  mod <- nom_refit(mod, nom_test)

  ms <- parameters(mod) %>%
    as_tibble() %>%
    mutate_at(vars(Coefficient, CI_low, CI_high), ~ -(.x)) %>%
    mutate_at(vars(Coefficient, CI_low, CI_high), exp) %>%
    # Switch columns because of the negative sign
    select(c("Parameter", "Coefficient", "CI_low" = CI_high, "CI_high" = CI_low, "p"))
  # select(c("Parameter", "Coefficient", "CI_high", "CI_low", "p"))

  attr(ms, "dep") <- dep
  attr(ms, "n") <- n
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

  ms <- bind_rows(mod_list)
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

na_omit_dat <- function(dat, dep, indeps) {
  # Why so many NAs for cidia?
  # NA might mean zero?
  dat <- imap_dfc(na.omit(dat[c(dep, indeps)]), function(col, name) {
    if (grepl("mhd_", name)) {
      if (sum(col == 1) < 20) {
        return(NULL)
      }
      else {
        return(col)
      }
    }
    else {
      return(col)
    }
  })

  return(dat)
}
