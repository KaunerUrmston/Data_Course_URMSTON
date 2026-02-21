getwd()
list.files()

library(tidyverse)

df <- read_csv("cleaned_covid_data.csv")
glimpse(df)

A_states <- df %>%
  filter(str_starts(Province_State, "A"))
A_states %>% count(Province_State)

p1 <- ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  labs(x = "Date", y = "Deaths", title = "Deaths over time (States starting with A)")

p1

df <- df %>% mutate(Last_Update = as.Date(Last_Update))
A_states <- df %>% filter(str_starts(Province_State, "A"))

state_max_fatality_rate <- df %>%
  group_by(Province_State) %>%
  summarize(
    Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Maximum_Fatality_Ratio))

state_max_fatality_rate <- df %>%
  group_by(Province_State) %>%
  summarize(
    Maximum_Fatality_Ratio = if (all(is.na(Case_Fatality_Ratio))) NA_real_
    else max(Case_Fatality_Ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Maximum_Fatality_Ratio))

state_max_fatality_rate <- state_max_fatality_rate %>%
  filter(!is.na(Maximum_Fatality_Ratio)) %>%
  mutate(Province_State = factor(Province_State, levels = Province_State))

p2 <- ggplot(state_max_fatality_rate,
             aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col() +
  labs(x = "State", y = "Maximum Case Fatality Ratio",
       title = "Peak COVID Case Fatality Ratio by State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p2

us_deaths_over_time <- df %>%
  group_by(Last_Update) %>%
  summarize(Total_US_Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(Last_Update)

p3 <- ggplot(us_deaths_over_time, aes(x = Last_Update, y = Total_US_Deaths)) +
  geom_line() +
  labs(x = "Date", y = "Total US Deaths (cumulative)",
       title = "Cumulative US COVID Deaths Over Time")

p3
