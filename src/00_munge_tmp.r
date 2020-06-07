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

ids <- dat.med["ID"]

dat <- reduce(dat_list, full_join)

full_join(dat_list[[1]], dat_list[[2]])

full_join(dat_joined, dat_list[[3]])
