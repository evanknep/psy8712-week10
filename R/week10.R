# Script Settings And Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(caret)
library(haven) #I don't specifically recall using this package in lecture but I found it linked on the tidyverse docs


# Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav", user_na = TRUE) %>%
  mutate_all(as.numeric) %>%
  filter(!is.na(MOSTHRS)) %>%
  mutate(MOSTHRS = as.numeric((MOSTHRS))) %>%
  select(-c(HRS1,HRS2)) %>%
  select(which(colMeans(is.na(.)) < 0.75))  #Iterating over each column and returning a sum of the NA values (treat each as =1), then divide by number of samples in the column. Retain column if value is < 0.75

#Analysis
ctrl <- trainControl(
  method = "cv", 
  number = 10,
  p = 0.75, 
  verboseIter = TRUE,
  savePredictions = TRUE 
)



# I had originally set it up with a tuneGrid like we had in datacamp, which was running fine, 
# but after force closing R (realized I had made a mistake like 5 minutes into processing)
# my tuneGrid now returns errors and prevents running
# Errors are all along the lines of : the tuning parameter grid should have columns intercept, mtry, seq, etc.,

train_models <- function(model, data) {
  model <- train(MOSTHRS ~ ., 
                 data = data,
                 method = model,
                 trControl = ctrl,
                 preProcess = "medianImpute")
  return(model)
                 }

train_models("lm", gss_tbl)
models <- lapply(c("lm", "glmnet", "ranger", "xgbTree"), train_models, data = gss_tbl)

# Print model results
summary(models)
#Visualization
gss_tbl %>%
  ggplot(aes(x=MOSTHRS)) +
  geom_histogram()


#Publication

str(gss_tbl$MOSTHRS)
