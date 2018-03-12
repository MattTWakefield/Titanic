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


train$set <- "train"
test$set  <- "test"
test$Survived <- NA
full <- rbind(train, test)

str(full)
dim(full)
lapply(full, function(x) length(unique(x)))
missing_values<-full%>%summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
  
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

#Useful data quality function for missing values

checkColumn = function(df,colname){
  
  testData = df[[colname]]
  numMissing = max(sum(is.na(testData)|is.nan(testData)|testData==''),0)
  
  
  if (class(testData) == 'numeric' | class(testData) == 'Date' | class(testData) == 'difftime' | class(testData) == 'integer'){
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = sum(is.infinite(testData)), 'avgVal' = mean(testData,na.rm=TRUE), 'minVal' = round(min(testData,na.rm = TRUE)), 'maxVal' = round(max(testData,na.rm = TRUE)))
  } else{
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = NA,  'avgVal' = NA, 'minVal' = NA, 'maxVal' = NA)
  }
  
}
checkAllCols = function(df){
  resDF = data.frame()
  for (colName in names(df)){
    resDF = rbind(resDF,as.data.frame(checkColumn(df=df,colname=colName)))
  }
  resDF
}


datatable(checkAllCols(full), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


miss_pct <- map_dbl(full, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))



