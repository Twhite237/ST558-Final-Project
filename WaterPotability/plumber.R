#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

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





#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Display the head of the potability data
#* @get /head
function(){
  head(potabilityDf)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

#* Make a prediction based on the fit data
#* @param pH
#* @param hardness
#* @param solids
#* @param chloramines
#* @param sulfate
#* @param conductivity
#* @param organicCarbon
#* @param thm
#* @param tubidity
#* @get /predict
function(ph = 0,
         hardness = 0,
         solids = 0,
         chloramines = 0,
         sulfate = 0,
         conductivity = 0,
         organicCarbon = 0,
         thm = 0,
         tubidity = 0){
  newData = data.frame(ph = as.numeric(ph),
                       hardness = as.numeric(hardness),
                       solids = as.numeric(solids),
                       chloramines = as.numeric(chloramines),
                       sulfate = as.numeric(sulfate),
                       conductivity = as.numeric(conductivity),
                       organicCarbon = as.numeric(organicCarbon),
                       thm = as.numeric(thm),
                       tubidity = as.numeric(tubidity))
  return(predict(apiFits, new_data = newData))
}

#* Return information about the repo
#* @get /info
function(){
  
  lines <- c("https://github.com/Twhite237/ST558-Final-Project", "Author: Tyler White")
  paste(lines, collapse = " ")
}

#* Return Confusion Matrix
#* @serializer png
#* @get /confusionMatrix
function(){
  print(autoplot(cm, type = "heatmap"))
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
