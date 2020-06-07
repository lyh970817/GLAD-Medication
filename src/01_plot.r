setwd("../")
library(ProjectTemplate)
load.project()

sef_rename <- sef_labels %>% setNames(sef_names)
cat_rename <- cat_labels %>% setNames(colnames(med_cat))

med_plot <- plot_dat %>%
  mutate(
    sef = recode(sef, !!!sef_rename) %>%
      fct_reorder(perct, .fun = mean, na.rm = T),
    cat = recode(cat, !!!cat_rename)
  ) %>%
  ggplot(aes(x = sef, y = perct, color = cat)) +
  geom_point(size = 7, alpha = 0.5) +
  labs(
    title = "Prevalence of Side Effects By Medication Category",
    subtitle = paste0(
      "n(total) = ", nrow(dat),
      "; n(female) = ", nrow(dat %>% filter(sex == "Female")),
      "; n(male) = ", nrow(dat %>% filter(sex == "Male")),
      "\n\nOnly percentages larger than zero and with CIs narrower than 0.4 are displayed"
    ),
    x = "Side Effect", y = "Percentage", color = "Medication Category",
    size = 15
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  theme(
    panel.grid.major.x = element_line(
      size = 0.5,
      linetype = "dashed",
      colour = "gray"
    ),
    axis.title.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 12),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom"
  ) +
  coord_flip()

ggsave("./graphs/med_plot.png", med_plot)
cache("med_plot")

med_plot_sex <- plot_dat_sex %>%
  mutate(
    sef = recode(sef, !!!sef_rename) %>%
      fct_reorder(perct, .fun = mean, na.rm = T),
    cat = recode(cat, !!!cat_rename)
  ) %>%
  ggplot(aes(x = sef, y = perct, shape = sex, color = cat)) +
  geom_point(size = 7, alpha = 0.5) +
  labs(
    title = "Prevalence of Side Effects By Medication Category",
    subtitle = paste0(
      "n(total) = ", nrow(dat),
      "; n(female) = ", nrow(dat %>% filter(sex == "Female")),
      "; n(male) = ", nrow(dat %>% filter(sex == "Male")),
      "\n\nOnly percentages larger than zero and with CIs narrower than 0.4 are displayed"
    ),
    x = "Side Effect", y = "Percentage", shape = "Sex", color = "Medication Category",
    size = 15
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  theme(
    panel.grid.major.x = element_line(
      size = 0.5,
      linetype = "dashed",
      colour = "gray"
    ),
    axis.title.y = element_blank(),
    axis.text.x = element_text(colour = "black", size = 12),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom"
  ) +
  coord_flip()

ggsave("./graphs/med_plot_sex.png", width = 30, height = 15, limitsize = FALSE)
cache("med_plot_sex")

perct_sef_plot <-
  ggplot(aes(
    y = sef_perct,
    x = fct_reorder(sef_labels, sef_perct),
    fill = sef_fill
  ),
  data = sef_perct_dat
  ) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # Remove NA factor level from legend
  scale_fill_hue(breaks = levels(sef_perct_dat[["sef_fill"]])) +
  theme(
    panel.grid.major.x = element_line(
      size = 0.5,
      linetype = "dashed",
      colour = "gray"
    ),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(
    title = "Percentage of Medication Instances with Side Effect",
    subtitle = "Computed by dividing side effect instances by total mediccation instances",
    y = ""
  ) +
  geom_text(
    aes(label = paste0(round(sef_perct, digits = 3) * 100, "%")),
    position = position_dodge(width = 0.9),
    hjust = -0.25, color = "black", size = 3
  ) +
  coord_flip(clip = "off")

ggsave("./graphs/perct_sef_plot.png", width = 30, height = 15, limitsize = FALSE)
cache("perct_sef_plot")
