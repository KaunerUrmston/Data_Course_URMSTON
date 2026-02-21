getwd()
list.files()

library(tidyverse)


dat <- read_csv("Data/BioLog_Plate_Data.csv", show_col_types = FALSE)


dat_long <- dat %>%
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    values_to = "Absorbance"
  ) %>%
  mutate(Time = as.numeric(str_remove(Time, "Hr_")))


dat_long <- dat_long %>%
  mutate(
    Type = case_when(
      `Sample ID` %in% c("Soil_1", "Soil_2") ~ "Soil",
      `Sample ID` %in% c("Clear_Creek", "Waste_Water") ~ "Water",
      TRUE ~ NA_character_
    )
  )


glimpse(dat_long)
table(dat_long$Type, useNA = "ifany")


dir.create("output", showWarnings = FALSE)


p1 <- dat_long %>%
  filter(Dilution == 0.1) %>%
  ggplot(aes(
    x = Time,
    y = Absorbance,
    color = Type,
    group = interaction(`Sample ID`, Rep, Well)
  )) +
  geom_line(alpha = 0.7, linewidth = 0.6) +
  facet_wrap(~ Substrate, scales = "free_y", ncol = 4) +
  labs(
    title = "Absorbance over time — Dilution 0.1",
    x = "Time (hours)",
    y = "Absorbance",
    color = "Type"
  ) +
  theme_bw() +
  theme(strip.text = element_text(size = 8))


print(p1)


ggsave("output/dilution_0.1_faceted.png", p1, width = 14, height = 10, dpi = 300)


list.files("output")

dat_long %>%
  distinct(Substrate) %>%
  filter(str_detect(Substrate, regex("itac", ignore_case = TRUE)))


library(gganimate)
library(tidyverse)


itac_mean <- dat_long %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(`Sample ID`, Type, Dilution, Time) %>%
  summarise(Mean_absorbance = mean(Absorbance, na.rm = TRUE), .groups = "drop")


itac_mean %>% count(Dilution, Time)


p2 <- ggplot(itac_mean, aes(
  x = Time,
  y = Mean_absorbance,
  color = `Sample ID`,
  group = `Sample ID`
)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Dilution) +
  labs(
    title = "Itaconic Acid utilization (replicate means)",
    x = "Time (hours)",
    y = "Mean absorbance"
  ) +
  theme_bw() +
  transition_reveal(Time)

anim <- animate(p2, renderer = gifski_renderer(), width = 900, height = 600)


anim_save("output/itaconic_acid_animation.gif", animation = anim)


list.files("output")
