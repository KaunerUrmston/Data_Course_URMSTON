library(tidyverse)
unicef <- read_csv("unicef-u5mr.csv")
head(unicef)
colnames(unicef)

unicef_tidy <- unicef %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  mutate(
    Year = str_remove(Year, "U5MR\\."),
    Year = as.numeric(Year)
  )
head(unicef_tidy)
glimpse(unicef_tidy)

plot1 <- unicef_tidy %>%
  ggplot(aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line() +
  facet_wrap(~ Continent) +
  labs(x = "Year", y = "U5MR")

plot1
ggsave("URMSTON_Plot_1.png", plot = plot1, width = 10, height = 6)

continent_means <- unicef_tidy %>%
  group_by(Continent, Year) %>%
  summarize(
    Mean_U5MR = mean(U5MR, na.rm = TRUE)
  )
head(continent_means)

plot2 <- continent_means %>%
  ggplot(aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line() +
  labs(x = "Year", y = "Mean U5MR")
plot2

ggsave("URMSTON_Plot_2.png", plot = plot2, width = 10, height = 6)

mod1 <- lm(U5MR ~ Year, data = unicef_tidy)

mod2 <- lm(U5MR ~ Year + Continent, data = unicef_tidy)

mod3 <- lm(U5MR ~ Year * Continent, data = unicef_tidy)

summary(mod1)
summary(mod2)
summary(mod3)

anova(mod1, mod2, mod3)
# mod3 is the best model because it has the highest R-squared value
# and includes interaction terms, allowing different trends across continents

unicef_model <- unicef_tidy %>%
  drop_na(U5MR)
nrow(unicef_model)

unicef_model <- unicef_model %>%
  mutate(
    pred1 = predict(mod1),
    pred2 = predict(mod2),
    pred3 = predict(mod3)
  )
head(unicef_model)

pred_long <- unicef_model %>%
  select(Continent, Year, pred1, pred2, pred3) %>%
  pivot_longer(
    cols = starts_with("pred"),
    names_to = "Model",
    values_to = "Prediction"
  )
head(pred_long)

pred_plot <- pred_long %>%
  ggplot(aes(x = Year, y = Prediction, color = Model)) +
  geom_line() +
  facet_wrap(~ Continent) +
  labs(x = "Year", y = "Predicted U5MR")
pred_plot

ggsave("URMSTON_Model_Predictions.png", plot = pred_plot, width = 10, height = 6)
