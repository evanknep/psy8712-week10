# Script Settings And Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(caret)
library(haven) #I don't specifically recall using this package in lecture but I found it linked on the tidyverse docs


# Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav", user_na = FALSE) %>%
  mutate_all(as.numeric) %>%
  filter(!is.na(MOSTHRS)) %>%
  mutate(MOSTHRS = as.numeric((MOSTHRS))) %>%
  select(-c(HRS1,HRS2)) %>%
  select(which(colMeans(is.na(.)) < 0.75))  #Iterating over each column and returning a sum of the NA values (treat each as =1), then divide by number of samples in the column. Retain column if value is < 0.75

glimpse(gss_tbl)
#Analysis
ctrl <- trainControl(
  method = "repeatedcv", 
  number = 10,           
  repeats = 5,           
  p = 0.75,              
  savePredictions = TRUE 
)


gss_tbl %>%
  train(MOSTHRS ~ .,
        tuneGrid = expand.grid(
          alpha = 0:1,
          lambda = seq(0.0001, 1, length = 20)
        ),
        method = "lm",
        trControl = ctrl,
        preProcess = "medianImpute" )


train_models <- function(model, data) {
  model <- train(as.formula(MOSTHRS ~ .), 
                 data = data,
                 method = model, 
                 trControl = ctrl,
                 preProcess = "medianImpute")
  return(model)
}

train_models("glmnet", gss_tbl)

models <- lapply(c("lm", "glmnet", "ranger", "xgboost"), train_models, data = gss_tbl)

# Print model results

#Visualization
gss_tbl %>%
  ggplot(aes(x=MOSTHRS)) +
  geom_histogram()


#Publication

str(gss_tbl$MOSTHRS)
