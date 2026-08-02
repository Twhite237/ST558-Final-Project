## Test code used in API for debugging
library(plumber)
library(tidyverse)
library(ggplot2)
library(tidymodels)
library(gt)
library(ggpubr)
library(corrplot)
library(caret)

### Read in Data

potabilityDf = read.csv("water_potability.csv",
                        col.names = c("ph",
                                      "hardness",
                                      "solids",
                                      "chloramines",
                                      "sulfate",
                                      "conductivity",
                                      "organicCarbon",
                                      "thm",
                                      "tubidity",
                                      "potability"))

## Clean and copy in recipe, workflow, model fitting from modeling file

## Recipe
potabilityDf = potabilityDf |> mutate(potability = as.factor(ifelse(potability == 0, FALSE, TRUE)))

preProcessingRecipe = recipe(potability ~ ., data = potabilityDf) |>
  step_naomit() |>
  step_normalize(all_numeric_predictors())

## Model Spec (mtry = 5 from tuned model)

rfModel = rand_forest(mtry = 5, min_n = 25) |>
  set_engine("ranger") |>
  set_mode("classification")

## Create workflow use the same recipe as the regression tree

rfWorkflow = workflow() |>
  add_recipe(preProcessingRecipe) |>
  add_model(rfModel)

## Fit

apiFits = rfWorkflow |> fit(data = potabilityDf)

completeDf <- potabilityDf |> drop_na()
truth_vec <- completeDf |> pull(potability)
pred_inputs <- completeDf |> select(-potability)
preds_vec <- predict(apiFits, new_data = pred_inputs) |> pull(.pred_class)

results <- tibble(
  truth = truth_vec,
  estimate = preds_vec
)

cm = (conf_mat(results, truth = truth, estimate = estimate))

autoplot(cm, type = "heatmap")
