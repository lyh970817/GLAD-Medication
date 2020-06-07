setwd("../")
library(ProjectTemplate)
library(tidyverse)
load.project()

# 1 Medications not selected beyond the first page not recoded to zeroes

# This person takes one mediction
sum(items_med[3, ] == 1)

# He is zero for items in the first page of the medication questions
which(items_med[3, ] == 0)

# But not the second page
which(items_med[3, ] %>% unlist() %>% is.na())

# 2 Side effects not selected not all recoded to zeroes

# We have this many taking the first antidepressant
sum(items_med[1] == 1)

# But only this many with non-missing values for its first side effect
sum(table(items_sef[1]))

# This recoding sets it correct
items_sef <- map_dfc(med_names, function(med) {
  items_med_col <- select(items_med, contains(med))
  items_sef %>%
    select(contains(med)) %>%
    modify(
      ~ ifelse(is.na(.x) & items_med_col == 1, 0, .x)
    ) %>%
    return()
})
# 3 Intolerence be zero if no side effect - two possibilities considered
# here, all NA or no to the side effect question.

items_intolerance <- dat_list[["med"]] %>%
  select(contains("ADD")) %>%
  select(-contains("OTHERADD")) %>%
  select(contains("STOP")) %>%
  # Get rid if "a different antidepressant"
  .[1:ncol(.) - 1]

med_names_sef <- get_name(colnames(items_sef), 3) %>% unique()

items_intolerance <- map2_df(items_intolerance, med_names_sef, function(col, med) {
  med_sef <- select(items_sef, contains(med))
  where <- (is_rowna(med_sef) | dat_list[["med"]][["MED.SE.BINARY"]] == 0) &
    dat_list[["med"]][[paste("MED.ADD", med, ".")]] == 1
  col[where] <- 0
  return(col)
})

# This is the same for both `items_intolerance`. So it looks like this has been recoded in Qualtrics?
sum(is_rowna(items_intolerance))


# 4 Why would there be NA in these question?

# Actually we might need to do a sweeping of all the NAs in the whole GLAD
# data set. The idea is if a everyone sees a question there shouldn't be NA
# in it.

# So it might be that these participants did not complete the
# questionnaire. So if they are NA they should be removed. Somehow we need
# to make use of the questions they did complete, but for now I'm removing
# all of them. This can be done for all quetsionnaires, if a participants
# is NA in one of the questions that every one sees the entire row should
# be set to NA.

b_test <- dat.med %>%
  select(contains("BEST")) %>%
  select(-contains("OTHER"))

apply(b_test, 2, function(x) {
  sum(is.na(x))
})

# 5 The distribution of PAD looks a bit odd, is it again because of NAs?

# NA in PAD also
test_pad <- dat_list[["pad"]]
map(test_pad[1:18], ~ sum(.x == "Seen but not answered", na.rm = T))

# But this does not affect the scoring because anyone who has more than one
# missing value is removed in `GLAD_score`

# 6 The overall side effect question should be reversed

# 7 People taking 0 in the 1-5 overall side effect question?
table(dat_list[["med"]][["MED.SE.HARM"]])


