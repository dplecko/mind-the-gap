
library(faircause)
library(xgboost)
library(ggplot2)
library(zeallot)
library(ricu)
library(icd)
library(data.table)

TeX <- latex2exp::TeX

Sys.setenv("RICU_CONFIG_PATH" = "~/fairness/mind-the-gap/config")
