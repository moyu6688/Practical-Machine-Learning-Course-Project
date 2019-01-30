---
title: "Practical Machine Learning Course Project Part 1: Human Activity Recognition"
author: Yu Mo
output: 
  html_document:
    keep_md: true
---

# Summary
This study is conducted to predict the quality of excersice from movements. It is found that the SVM method is the best, with overall acuracy of 0.91.   

# 1 Pre-processing
####Data Source 
Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013. Read more: http://groupware.les.inf.puc-rio.br/har#ixzz5dqwupRwF

####Load data

```r
training.data <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"), na.strings=c("NA","#DIV/0!",""))
testing.data <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"), na.strings=c("NA","#DIV/0!",""))
dim(training.data);dim(testing.data)
```

```
## [1] 19622   160
```

```
## [1]  20 160
```
* There are 19622 observations in the traing dataset, and 20 observations in the testing dateset. 

####Create the training and test dataset (25% for testing)

```r
library(caret)
set.seed(100)
inTrain = createDataPartition(training.data$classe, p = 0.6)[[1]]
training = training.data[ inTrain,-1]  #also get rid of column, user ID
testing = training.data[-inTrain,-1]
```
####Get rid of columns with NA values 

```r
training<-training[ , colSums(is.na(training)) == 0]
```

#2 Model Building 
####Build model using the Support Vector Machine (SVM), the Ramdon Forest (RF), gradient boosting machine (GBM), and the Linear Discriminant Analysis (LDA). This step required packages caret, randomForest, gbm. 

```r
library(randomForest)
library(gbm)
model.svm<- train(classe ~ ., data = training, method = "svmLinear")
model.rf<-randomForest(classe ~ ., data = training, ntrees=500)
model.gbm<-gbm(classe ~ ., data = training,n.trees=500)
model.lda<- train(classe ~ ., data = training, method = "lda")

pred.svm<- as.character(predict(model.svm, testing))
pred.rf<- as.character(predict(model.rf, testing))
pred.gbm<- predict(model.gbm, testing,n.trees=500)
pred.gbm2<-attributes(pred.gbm)$dimnames[[2]][apply(pred.gbm, 1, which.max)]
pred.lda<- as.character(predict(model.lda, testing))
```
####Use ensembling method with RF with the reuslts from the RF, SVM, GBM and LDA models. 

```r
predDF <- data.frame(pred.svm,pred.gbm,pred.lda,pred.rf, classe =testing$classe)
model.em<-randomForest(classe ~ ., data =predDF,ntrees=500)
pred.em<- predict(model.em, predDF)
```

#3 Compare the results
####Accuracy of the different models

```r
accuracy<-data.frame(SVM=confusionMatrix(table(pred.svm, testing$classe))$overall[1],
RF=confusionMatrix(table(pred.rf, testing$classe))$overall[1] ,  
GBM=confusionMatrix(table(pred.gbm2, testing$classe))$overall[1] , 
LDA=confusionMatrix(table(pred.lda, testing$classe))$overall[1] , 
EM=confusionMatrix(table(pred.em, predDF$classe))$overall[1])
accuracy
```

```
##                SVM        RF       GBM       LDA EM
## Accuracy 0.9102728 0.9989804 0.9905684 0.8568697  1
```

The SVM and LDA methods give very good predictions, and the RF and the EM methods are probably overfitting. SVM is selected as our final model. 

####Performance of the SVM method   

```r
confusionMatrix(table(pred.svm, testing$classe))
```

```
## Confusion Matrix and Statistics
## 
##         
## pred.svm    A    B    C    D    E
##        A 2160  152    7    0    1
##        B   63 1285  114    7    1
##        C    8   81 1232   98    3
##        D    1    0   15 1104   76
##        E    0    0    0   77 1361
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9103          
##                  95% CI : (0.9037, 0.9165)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.8863          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9677   0.8465   0.9006   0.8585   0.9438
## Specificity            0.9715   0.9708   0.9707   0.9860   0.9880
## Pos Pred Value         0.9310   0.8741   0.8664   0.9231   0.9465
## Neg Pred Value         0.9870   0.9635   0.9788   0.9726   0.9874
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2753   0.1638   0.1570   0.1407   0.1735
## Detection Prevalence   0.2957   0.1874   0.1812   0.1524   0.1833
## Balanced Accuracy      0.9696   0.9086   0.9356   0.9222   0.9659
```

#4 Predict the 20 Cases

```r
predict(model.svm, newdata=testing.data)
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

