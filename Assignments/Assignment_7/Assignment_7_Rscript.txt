# Assignment 7
# Kauner Urmston

library(tidyverse)
library(here)

# Import the dataset using a relative path from the Assignment_7 project folder.
# Using relative paths ensures the script works on any computer without editing file paths.
religion_raw <- read_csv(here("Utah_Religions_by_County.csv"))

# Inspect the raw dataset to understand its structure before cleaning.
# This allows me to identify which columns are identifiers (County, Pop_2010)
# and which columns represent religious groups.
glimpse(religion_raw)
names(religion_raw)
head(religion_raw)

# The religion categories are currently stored as separate columns.
# To follow tidy data principles (one variable per column and one observation per row),
# I convert the religion columns into a long format using pivot_longer().
religion_tidy <- religion_raw %>%
  pivot_longer(
    cols = -c(County, Pop_2010, Religious),
    names_to = "Religion",
    values_to = "Proportion"
  )

# Check that the pivot worked correctly and that religion names appear as values
# in the Religion column rather than as column headers.
glimpse(religion_tidy)
unique(religion_tidy$Religion)

# Create a boxplot to explore how the proportion of each religion varies
# across Utah counties. This provides an overview of the distribution
# before investigating relationships with population.
ggplot(religion_tidy, aes(x = Religion, y = Proportion)) +
  geom_boxplot() +
  labs(
    title = "Distribution of religion proportions across Utah counties",
    x = "Religion",
    y = "Proportion of county population"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Question 1: Does county population correlate with religion proportions?
# To explore this visually, I create scatterplots comparing county population
# with the proportion of each religion.
ggplot(religion_tidy, aes(x = Pop_2010, y = Proportion)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Religion, scales = "free_y") +
  labs(
    title = "County population vs religion proportion",
    x = "County population",
    y = "Proportion of county population"
  )

# Calculate correlation coefficients to quantify the relationship between
# county population and religion proportion.
population_correlations <- religion_tidy %>%
  group_by(Religion) %>%
  summarize(
    correlation_with_population = cor(Pop_2010, Proportion)
  )

population_correlations

# Interpretation (Question 1)
# Most religious groups show little to no correlation between county population
# and the proportion of adherents. For example, LDS shows a correlation near zero,
# suggesting that counties with larger populations do not necessarily have higher
# or lower LDS proportions. This indicates that county population size does not
# strongly determine the distribution of most religious groups in Utah.

# Question 2: Does the proportion of a religion correlate with the proportion
# of non-religious people in the same county?

# First, create a dataset containing the non-religious proportion for each county.
nonreligious <- religion_tidy %>%
  filter(Religion == "Non-Religious") %>%
  select(County, nonreligious_proportion = Proportion)

# Join the non-religious values back to the main dataset so that each religion
# row also includes the non-religious proportion for that county.
religion_compare <- religion_tidy %>%
  filter(Religion != "Non-Religious") %>%
  left_join(nonreligious, by = "County")

# Inspect the joined dataset to confirm the new column was added correctly.
head(religion_compare)

# Create scatterplots comparing religion proportion with non-religious proportion.
# This helps visualize whether higher values of a religion correspond to higher
# or lower proportions of non-religious people.
ggplot(religion_compare, aes(x = Proportion, y = nonreligious_proportion)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Religion, scales = "free_x") +
  labs(
    title = "Religion proportion vs non-religious proportion by county",
    x = "Proportion of specific religion",
    y = "Proportion non-religious"
  )

# Calculate correlations between each religion and the non-religious proportion
# to quantify the relationship observed in the scatterplots.
nonreligious_correlations <- religion_compare %>%
  group_by(Religion) %>%
  summarize(
    correlation_with_nonreligious = cor(Proportion, nonreligious_proportion)
  )

nonreligious_correlations

# Interpretation (Question 2)
# The LDS religion shows a strong negative correlation with the proportion
# of non-religious individuals (approximately -0.87). This suggests that
# counties with higher LDS populations tend to have lower proportions of
# non-religious residents. Most other religions show weak correlations with
# the non-religious population, indicating little systematic relationship.