library(tidyverse)
library(feather)

dat <- read_feather("data/dat_fam.feather")
nrow(dat)
map_dbl(dat, ~ sum(is.na(.x)))

with(dat, ifelse((cidia.felt_worried == 1 | cidia.felt_worried_more == 1)
& (cidia.most_days == 1)
& (cidia.longest_period_worry_years >= 0.5 | cidia.longest_period_worry_months >= 6)
& (cidia.more_than_one_thing == 2 | cidia.different_worries == 1)
& (cidia.difficult_to_control == 3 | cidia.difficult_to_stop == 1 | cidia.couldnt_stop == 3)
& (cidia.functioning == 1),
yes = 1,
no = 0
))

ifelse(NA, yes = 1, no = 2)
dat[["cidia.functioning"]]
