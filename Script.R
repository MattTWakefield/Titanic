source("Install.R")
Install(c(
  #Data Wrangling
          "tidyverse",
          "forcats",
          "stringr",
          "caTools",
          #data assessment/visualizations
          "DT",
          "data.table",
          "pander",
          "ggplot2",
          "scales",
          "grid",
          "gridExtra",
          "corrplot",
          "VIM",
          "knitr",
          "vcd",
          "caret",
          #model
          "xgboost",
          "MLmetrics",
          "randomForest",
          "rpart",
          "rpart.plot",
          "car",
          "e1071",
          "ROCR",
          "pROC",
          "glemnet"))

train <- read_csv('./input/train.csv')
test  <- read_csv('./input/test.csv')
setwd("C:/KaggleTest")
  
