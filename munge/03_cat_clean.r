cat_labels <- colnames(med.cat)

cache("cat_labels")

med_cat <- med.cat
colnames(med_cat) <- colnames(med_cat) %>%
  substr(1, 4) %>%
  toupper()

med_cat <- modify(med_cat, ~ str_sub(.x, end = 6) %>%
  toupper())

cache("med_cat")
