setwd("../")
library(ProjectTemplate)
library(polycor)
reload.project()

dat_list <- list(
  dat.dem, dat.med, dat.gad, dat.phq, dat.cidip,
  dat.saspd, dat.cidid, dat.cidia, dat.pad, dat.mhd,
  dat.fam
)

for (i in 1:10) {
  print(i)
}

na_check(dat)
cor(dat[["mean_sef"]], as.numeric(dat[["ben"]]), use = "pairwise.complete.obs")


ids <- dat.med["ID"]

na_check(dat[grep("mhd_", colnames(dat))])
na_check(dat)

dat <- reduce(dat_list, full_join)
na_check(dat)

full_join(dat_list[[1]], dat_list[[2]])

full_join(dat_joined, dat_list[[3]])
