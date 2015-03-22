---
title: "PML_WriteUP"
author: "Anisha"
date: "22 March 2015"
output: html_document
---

1. The purpose of the research here is to predict the value of the manner in which the exercise was done.
2. This is represented by the 'classe' variable in the data.
3. I have used three different models for prediction.
4. Highest accuracy was got using the randomforest method hence that method was used in the final result.

<span style="color:magenta">Loading library and data sets</span>
```{r, results='hide'}
library(caret)
library(randomForest)
library(rpart) 
library(rpart.plot) 
training_set <- read.csv("/Users/anishajha/RLearn/PML_CourseProject/train.csv", na.strings=c("NA","#DIV/0!", ""))
testing_set<-read.csv("/Users/anishajha/RLearn/PML_CourseProject/test.csv",na.strings=c("NA","#DIV/0!", ""))
```

<span style="color:magenta">Defining the training set,validation set and test set</span>
```{r }
dim(training_set)[2]

#We use threshold to determine the number of columns which have more than a certain threshold of NA's. We remove them.
thresh<-0.8*dim(training_set)[1]
good_columns<-!apply(training_set,2,function(x) {sum(is.na(x))>thresh || sum(x=="")>thresh})
training_set<-training_set[,good_columns]
testing_set<-testing_set[,good_columns]
training_set<-training_set[6:dim(training_set)[2]]
testing_set<-testing_set[6:dim(testing_set)[2]]

#We also remove the variables which contribute very little to the model because their values are either very near to 0 or very close together

badColumns <- nearZeroVar(training_set, saveMetrics = TRUE)

training_set <- training_set[, badColumns$nzv==FALSE]
testing_set <- testing_set[, badColumns$nzv==FALSE]

#We create a data partition for the training set and the test set.
inTrain <- createDataPartition(y=training_set$classe,p=0.85,list=FALSE)
training<-training_set[inTrain,]
validation <- training_set[-inTrain,]
```

```{r}
#First model we calculate with lda
model_first <- train(classe ~ .,data=training,method="lda")
#summary(model_first)
#model_first
#model_first$finalModel
predictions<-predict(model_first,newdata=validation)
confusionMatrix(predictions,validation$classe)
```
<span style="color:magenta">In the first model we observe an accuracy of about 71%</span>
```{r}
#Second Model using rpart
model2 <- rpart(classe ~ ., data=training, method="class")
# Predicting:
prediction_rpart <- predict(model2, validation, type = "class")
# Plot of the Decision Tree
rpart.plot(model2, main="Classification Tree", extra=102, under=TRUE, faclen=0)
summary(prediction_rpart)
confusionMatrix(prediction_rpart,validation$classe)
```
<span style="color:magenta">In the second model we observe and accuracy of about 75%</span>
```{r}
#Third model using random forest
model_rf <- randomForest(classe ~. , data=training, method="class")
# Predicting:
prediction_rf <- predict(model_rf, validation, type = "class")
# Test results on subTesting data set:
confusionMatrix(prediction_rf, validation$classe)
varImpPlot(model_rf,main = "Importance of Top 40 Variables", n.var=40,color="blue",cex=0.6,lcolor="brown",pch=5)
```

<span style="color:magenta">
In the third model we observe an accuracy of 99%
Final Prediction using RandomForest
</span>
```{r}
predictfinal <- predict(model_rf, testing_set, type="class")
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(predictfinal)
```
