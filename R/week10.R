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
summary(models)


table1_tbl <- tibble(
  algo = c("OLS regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  cv_rsq = sapply(models, function(model) tail(model$results$Rsquared, 1)),
  ho_rsq = sapply(models, function(model) tail(model$resample$Rsquared, 1))) %>%
  mutate(across(where(is.numeric), ~ formatC(., format = "f", digits = 2))) %>%
  apply(2, function(x) str_remove(x, "^0+"))
  
print(table1_tbl)

#Visualization
gss_tbl %>%
  ggplot(aes(x=MOSTHRS)) +
  geom_histogram() 



#Publication
"Questions:
1. The model results were consistently around 0.6 R^2 for all models besides our OLS regression. There could be a few reasons that this is the case,
there may be some issues in our data with collinearity or columns with near zero variance that are skewing things in our OLS but 
were filtered out in our glmnet. If I had to guess I would say it might be an outlier issue. I noticed in our histogram of MOSTHRS that
there were a few people that reported extremely unrealistic numbers around 1000 hours. Our glmnet should be filtering this out but our OLS may not.

2. Our results were quite different between our k-fold and holdout (though I am unsure if I even incorporated the holdout component correctly). 
We see decreases in R^2 values using Xgboost, similar R^2 to k-fold in OLS regression (minor increase) and random forest, and a massive
increase in R^2 in our glmnet. The glmnet result being the odd outlier here, I would interpret these changes as an example of each 
model's generalizability. Just because a model fits well on the training set doesn't mean it peforms well on different data. xgboost showing the 
largest decrease is quite surprising to me considering it is one of the most popular models currently in use, but I think what we can interpret
from the increase in our lm and glmnet is that the relationships we're looking at here are primarily linear. xgboost incorporates nonlinear
analysis that may struggle with our linear data.

3. Which I would choose for a real world problem really depends on what exactly I am trying to accomplish. Linear vs nonlinear aside,
do I simply care about prediction accuracy or do I want to be able to interpret results and coefficients? For my own work looking at 
behavioral differences between neurotypical and atypical populations, I would want to be able to draw conclusions about what is 
driving group differences, and thus would want to be able to look at coefficients and value interpretibility. For that I would 
be choosing between lm and glm, but would ultimately choose glm because it seems to produce more robust results."

