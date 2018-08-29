## chamadas para os agoritos de machining learning.


library("plyr")
library("dplyr")
library("ggplot2")
library("scales")
library("chron")
library("class")
library("gmodels")
library("party")
library("rpart")
library("lattice")
library("caret")
library("e1071")
library("plotROC")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

## gerando conjuntos de treinamento e test Decision Tree

for(count in 1:nrow(aux2)) {
  if(aux2$resourcePoints[count] <= 0.00005) {
    aux2$resourcePoints[count] = 0;
  } else if (aux2$resourcePoints[count] > 0.00005 && aux2$resourcePoints[count] <= 0.0005) {
    aux2$resourcePoints[count] = 1;
  } else if (aux2$resourcePoints[count] > 0.0005 && aux2$resourcePoints[count] <= 0.005) {
    aux2$resourcePoints[count] = 2;
  } else if (aux2$resourcePoints[count] > 0.005 && aux2$resourcePoints[count] <= 0.05) {
    aux2$resourcePoints[count] = 3;
  } else if (aux2$resourcePoints[count] > 0.05 && aux2$resourcePoints[count] <= 0.1) {
    aux2$resourcePoints[count] = 4;
  } else if (aux2$resourcePoints[count] > 0.1 && aux2$resourcePoints[count] <= 0.2) {
    aux2$resourcePoints[count] = 5;
  } else if (aux2$resourcePoints[count] > 0.3 && aux2$resourcePoints[count] <= 0.4) {
    aux2$resourcePoints[count] = 6;
  } else if (aux2$resourcePoints[count] > 0.4 && aux2$resourcePoints[count] <= 0.5) {
    aux2$resourcePoints[count] = 6;
  }
}

aux2 %>% filter(resourcePoints>0.0005) %>% nrow()

aux2 <- rbind(cranckFeatureSelectedNegatives,cranckFeatureSelected)
aux2 <- select(aux2, c(4,6,76,77,78,79,80,82,83,84,85))



RowsFalse <- filter(aux2, aux2$rotulo==FALSE)
aux2 <- filter(aux2, aux2$rotulo!=FALSE)
count <- nrow(RowsFalse)
aux2 <- head(aux2,n = nrow(RowsFalse))
aux2 <- rbind(aux2,RowsFalse)
RowsFalse=0


set.seed(321)
form <- "rotulo ~ ResearchPoints + resourcePoints + timePassFromLastStatus + AvgTimeBetWeenStatus + NumbDaysPlay + EndGameFlag + NumberofStatus + AvgGameTimePlayedPerDay"
folds <- split(aux2, cut(sample(1:nrow(aux2)),5))
errs <- rep(NA, length(folds))

for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- rpart(form , train, method = "class")
  tmp.predict <- predict(tmp.model, newdata = test, type = "class")
  conf.mat <- table(test$rotulo, tmp.predict)
  print(conf.mat)
  errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))



## gerando conjuntos de treinamento e test Naive Byers

TrainData <- aux2[,c(1,2,3,4,5,7,8,9,10,11)] 
TrainClasses <- aux2[,c(6)]

for (count in 1:nrow(TrainData)) {
  if(TrainData$EndGameFlag[count] == TRUE) {
    TrainData$EndGameFlag[count] = 1
  } else {
    TrainData$EndGameFlag[count] = 0
  }
}

TrainClasses <- factor(TrainClasses, labels = c("TRUE", "FALSE"))


fit.nb <- train(
  x=TrainData, y=TrainClasses, method = "nb",
  trControl = trainControl(method = "cv", number = 10))


fit.rpart <- train(
  x=TrainData, y=TrainClasses, method = "rpart",
  trControl = trainControl(method = "cv", number = 10))


fit.knn <- train(
  x=TrainData, y=TrainClasses, method = "knn",
  trControl = trainControl(method = "cv", number = 10))


fit.nnet <- train(
  x=TrainData, y=TrainClasses, method = "nnet",
  trControl = trainControl(method = "cv", number = 10))
