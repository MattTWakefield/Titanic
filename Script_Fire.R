#https://www.kaggle.com/hiteshp/head-start-for-data-scientist
setwd("F:/Titanic/Titanic/input")
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


#Replace missing Age cells with the mean Age of all passengers on the Titanic.

full <- full %>%
  mutate(
    Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
    `Age Group` = case_when(Age < 13 ~ "Age.0012", 
                            Age >= 13 & Age < 18 ~ "Age.1317",
                            Age >= 18 & Age < 60 ~ "Age.1859",
                            Age >= 60 ~ "Age.60Ov"))

#Use the most common code to replace NAs in the Embarked feature.
full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)), 'S')

#Extract an individual's title from the Name feature.
names <- full$Name

title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)

full$title<-title

full$title[full$title == 'Mlle']        <- 'Miss' 
full$title[full$title == 'Ms']          <- 'Miss'
full$title[full$title == 'Mme']         <- 'Mrs' 
full$title[full$title == 'Lady']          <- 'Miss'
full$title[full$title == 'Dona']          <- 'Miss'

## I am afraid creating a new varible with small data can causes a overfit
## However, My thinking is that combining below feauter into original variable may loss some predictive power as they are all army folks, doctor and nobel peoples 

full$title[full$title == 'Capt']        <- 'Officer' 
full$title[full$title == 'Col']        <- 'Officer' 
full$title[full$title == 'Major']   <- 'Officer'
full$title[full$title == 'Dr']   <- 'Officer'
full$title[full$title == 'Rev']   <- 'Officer'
full$title[full$title == 'Don']   <- 'Officer'
full$title[full$title == 'Sir']   <- 'Officer'
full$title[full$title == 'the Countess']   <- 'Officer'
full$title[full$title == 'Jonkheer']   <- 'Officer' 

full$FamilySize <-full$SibSp + full$Parch + 1 
full$FamilySized[full$FamilySize == 1] <- 'Single' 
full$FamilySized[full$FamilySize < 5 & full$FamilySize >= 2] <- 'Small' 
full$FamilySized[full$FamilySize >= 5] <- 'Big' 
full$FamilySized=as.factor(full$FamilySized)

##Engineer features based on all the passengers with the same ticket
ticket.unique <- rep(0, nrow(full))
tickets <- unique(full$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full$Ticket == current.ticket)
  
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}

#The independent variable, Survived, is labeled as a Bernoulli trial 
#where a passenger or crew member surviving is encoded with the value of 1. 
#Among observations in the train set, approximately 38% of passengers and crew survived.

full$ticket.unique <- ticket.unique

full$ticket.size[full$ticket.unique == 1]   <- 'Single'
full$ticket.size[full$ticket.unique < 5 & full$ticket.unique>= 2]   <- 'Small'
full$ticket.size[full$ticket.unique >= 5]   <- 'Big'
  
full <- full %>%
  mutate(Survived = case_when(Survived==1 ~ "Yes", 
                              Survived==0 ~ "No"))

crude_summary <- full %>%
  filter(set=="train") %>%
  select(PassengerId, Survived) %>%
  group_by(Survived) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

crude_survrate <- crude_summary$freq[crude_summary$Survived=="Yes"]

kable(crude_summary, caption="2x2 Contingency Table on Survival.", format="markdown")


#Exploratory Data Analysis

tbl_corr <- full %>%
  filter(set=="train") %>%
  select(-PassengerId, -SibSp, -Parch) %>%
  select_if(is.numeric) %>%
  cor(use="complete.obs") %>%
  corrplot.mixed(tl.cex=0.85)

#2.5.4.2 Mosaic Plot
tbl_mosaic <- full %>%
  filter(set=="train") %>%
  select(Survived, Pclass, Sex, AgeGroup=`Age Group`, title, Embarked, `FamilySize`) %>%
  mutate_all(as.factor)

mosaic(~Pclass+Sex+Survived, data=tbl_mosaic, shade=TRUE, legend=TRUE)

#2.5.4.3 Alluvial Diagram
Install("alluvial")

tbl_summary <- full %>%
  filter(set=="train") %>%
  group_by(Survived, Sex, Pclass, `Age Group`, title) %>%
  summarise(N = n()) %>% 
  ungroup %>%
  na.omit

alluvial(tbl_summary[, c(1:4)],
         freq=tbl_summary$N, border=NA,
         col=ifelse(tbl_summary$Survived == "Yes", "blue", "gray"),
         cex=0.65,
         ordering = list(
           order(tbl_summary$Survived, tbl_summary$Pclass==1),
           order(tbl_summary$Sex, tbl_summary$Pclass==1),
           NULL,
           NULL))

#Machine Learning

feauter1<-full[1:891, c("Pclass", "title","Sex","Embarked","FamilySized","ticket.size")]
response <- as.factor(train$Survived)
feauter1$Survived=as.factor(train$Survived)



###For Cross validation purpose will keep 20% of data aside from my orginal train set
##This is just to check how well my data works for unseen data

set.seed(500)
ind=createDataPartition(feauter1$Survived,times=1,p=0.8,list=FALSE)
train_val=feauter1[ind,]
test_val=feauter1[-ind,]



round(prop.table(table(train$Survived)*100),digits = 1)

round(prop.table(table(train_val$Survived)*100),digits = 1)

round(prop.table(table(test_val$Survived)*100),digits = 1)

#Decision Tree

##Random forest is for more better than Single tree however single tree is very easy to use and illustrate
set.seed(1234)
Model_DT=rpart(Survived~.,data=train_val,method="class")


rpart.plot(Model_DT,extra =  8,fallen.leaves = T)

PRE_TDT=predict(Model_DT,data=train_val,type="class")
confusionMatrix(PRE_TDT,train_val$Survived)

#####Accuracy is 0.8375
####Not at all bad using Single tree and just 3 feauters

##There is chance of overfitting in Single tree, So I will go for cross validation using '10 fold techinque'

set.seed(1234)
cv.10 <- createMultiFolds(train_val$Survived, k = 10, times = 10)

# Control
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     index = cv.10)



train_val <- as.data.frame(train_val)

##Train the data
Model_CDT <- train(x = train_val[,-7], y = train_val[,7], method = "rpart", tuneLength = 30,
                   trControl = ctrl)

##Check the accurcay
##Accurcay using 10 fold cross validation of Single tree is 0.8139 
##Seems Overfitted earlier using Single tree, there our accurcay rate is 0.83

# check the variable imporatnce, is it the same as in Single tree?
rpart.plot(Model_CDT$finalModel,extra =  3,fallen.leaves = T)

##Yes, there is no change in the imporatnce of variable




###Lets cross validate the accurcay using data that kept aside for testing purpose
PRE_VDTS=predict(Model_CDT$finalModel,newdata=test_val,type="class")
confusionMatrix(PRE_VDTS,test_val$Survived)

col_names <- names(train_val)

train_val[col_names] <- lapply(train_val[col_names] , factor)
test_val[col_names] <- lapply(test_val[col_names] , factor)

####

set.seed(1234)


rf.1 <- randomForest(x = train_val[,-7],y=train_val[,7], importance = TRUE, ntree = 1000)
rf.1


varImpPlot(rf.1)

####Random Forest accurcay rate is 82.91 which is 1% better than the decison  tree
####Lets remove 2 redaundant varibles and do the modeling again

train_val1=train_val[,-4:-5]
test_val1=test_val[,-4:-5]

set.seed(1234)
rf.2 <- randomForest(x = train_val1[,-5],y=train_val1[,5], importance = TRUE, ntree = 1000)
rf.2

varImpPlot(rf.2)


###Can see the Magic now, increase in accuracy by just removing 2 varibles, accuracy now is 84.03 

##Even though random forest is so power full we accept the model only after cross validation


set.seed(2348)
cv10_1 <- createMultiFolds(train_val1[,5], k = 10, times = 10)

# Set up caret's trainControl object per above.
ctrl_1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv10_1)



set.seed(1234)
rf.5<- train(x = train_val1[,-5], y = train_val1[,5], method = "rf", tuneLength = 3,
             ntree = 1000, trControl =ctrl_1)

rf.5

View(train_val%>%
       group_by(Pclass, title, Survived)%>%
       summarise(count = n()))%>%
  filter(title %in% c("Mr","Officer"))





