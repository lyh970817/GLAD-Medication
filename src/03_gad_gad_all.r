setwd("../")
library("ProjectTemplate")
load.project()

# Compare GAD Score between all participants and those who completed the
# medicaiont optional

gad_t <- t.test(score_gad_all, dat[["score_gad"]])
cache("gad_t")

phq_t <- t.test(score_phq_all, dat[["score_phq"]])
cache("phq_t")
