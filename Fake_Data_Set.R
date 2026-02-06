library(tidyverse)
d <- read_csv("Fake_data.csv")
d <- d %>%
  mutate(
    femur_tibia_ratio = femur_length_mm / tibia_length_mm,
    forelimb_hindlimb_ratio = humerus_length_mm / femur_length_mm
  )

ggplot(d, aes(x = clade, y = femur_tibia_ratio)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.8) +
  labs(
    title = "Simulated data: Hindlimb proportion differs across dinosaur clades",
    x = "Clade",
    y = "Femur / Tibia ratio"
  ) +
  theme_minimal()
