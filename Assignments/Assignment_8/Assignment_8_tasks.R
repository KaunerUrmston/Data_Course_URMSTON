# Assignment 8

library(tidyverse)
library(modelr)
library(easystats)
library(broom)
library(fitdistrplus)

mush <- read_csv("../../Data/mushroom_growth.csv")
glimpse(mush)

#The data set contains 216 observations of mushroom growth under varying environmental conditions. GrowthRate is the response variable, while Nitrogen, Light, Temperature, Humidity, and Species serve as predictors.

mush <- mush %>%
  mutate(
    Humidity = as.factor(Humidity),
    Species = as.factor(Species)
  )
#Humidity and Species were converted to factors to ensure they are treated as categorical variables in the models.

ggplot(mush, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~Humidity)
#The relationship between nitrogen and growth appears nonlinear, with a peak at intermediate nitrogen levels. Growth is consistently higher under high humidity conditions, suggesting both variables are important predictors.

model_1 <- lm(GrowthRate ~ Nitrogen, data = mush)
summary(model_1)
#Model 1 showed no significant linear relationship between nitrogen and growth, indicating a simple linear model is insufficient.

model_2 <- lm(GrowthRate ~ Nitrogen + I(Nitrogen^2), data = mush)
summary(model_2)
#Adding a quadratic term significantly improved the model, indicating a nonlinear relationship between nitrogen and growth.

model_3 <- lm(GrowthRate ~ Nitrogen + I(Nitrogen^2) + Humidity, data = mush)
summary(model_3)
#Including humidity substantially improved model performance, suggesting environmental conditions strongly influence growth.

model_4 <- lm(GrowthRate ~ (Nitrogen + I(Nitrogen^2)) * Humidity, data = mush)
summary(model_4)
#The interaction model allows nitrogen effects to differ by humidity level and provided the best fit.

mse_1
mse_2
mse_3
mse_4
#Model 4 had the lowest mean squared error and was selected as the best model for predicting growth.

new_data
#Predicted values show that growth peaks at intermediate nitrogen levels and is consistently higher under high humidity conditions.

ggplot(new_data, aes(x = Nitrogen, y = PredictedGrowth, color = Humidity)) +
  geom_line()
#The predicted curves confirm a hump-shaped relationship and highlight the strong positive effect of humidity.

