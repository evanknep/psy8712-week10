# Script Settings And Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(caret)
library(haven) #I don't specifically recall using this package in lecture but I found it linked on the tidyverse docs


# Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav", user_na = FALSE) %>%
  filter(!is.na(MOSTHRS)) %>%
  select(-c(HRS1,HRS2)) %>%
  select(which(colMeans(!is.na(.)) < 0.75))  #Iterating over each column and returning a sum of the NA values (treat each as =1), then divide by number of samples in the column. Retain column if value is < 0.75




