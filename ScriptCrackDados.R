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

generateTempoEntreSessoes <- function(x) {
  
}



cranckStatus <- read.csv("D:\\R projetos\\Cranck dados e codigo\\cranckStatus 00")
cranckFeatureSelected <- read.csv("D:\\R projetos\\Cranck dados e codigo\\CranckFeatureSelected.txt")
cranckFeatureSelectedNegatives <- read.csv("D:\\R projetos\\Cranck dados e codigo\\cranckFeaturesWithNegatives.txt")
drops <- c("X","X.1","X.2")
cranckFeatureSelected <- cranckFeatureSelected[, !(names(cranckFeatureSelected) %in% drops)]
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives[, !(names(cranckFeatureSelectedNegatives) %in% drops)]

write.csv(x = cranckFeatureSelected,file = "D:\\R projetos\\Cranck dados e codigo\\CranckFeatureSelected.txt")
write.csv(x = cranckFeatureSelectedNegatives,file = "D:\\R projetos\\Cranck dados e codigo\\cranckFeaturesWithNegatives.txt")

cranckFeatureSelectedNegatives <- rbind(cranckFeatureSelected,cranckFeatureSelectedNegatives)

cranckStatus$GameID[2]
cranckStatus <- group_by(cranckStatus,GameID)
cranckStatus <- arrange(cranckStatus,cranckStatus$RealTime)
cranckStatus <- filter(cranckStatus,cranckStatus$GameID!="")
cranckFeatureSelected <- cranckStatus
cranckFeatureSelected <- filter(cranckFeatureSelected,cranckFeatureSelected$GameID!="")
cranckFeatureSelected <- filter(cranckFeatureSelected,n(cranckFeatureSelected$GameID)>2)
cranckFeatureSelected <- transform(cranckFeatureSelected,timePassFromLastStatus = 0)

cranckFeatureSelected <- filter(cranckFeatureSelectedNegatives, cranckFeatureSelectedNegatives$rotulo==TRUE)

## drivando tempo entre sessoes

count = 1;
count1 = 1;
limite = 1;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1 <- arrange(aux1,aux1$RealTime)
limite = nrow(aux1)
if (limite>1) {
  for(count1 in 2:limite) {
    if(aux1$GameID[count1-1]==aux1$GameID[count1]) {
      
      aux1$timePassFromLastStatus[count1] = difftime(aux1$RealTime[count1],aux1$RealTime[count1-1],units='secs')
    }
  }
  aux2 <- rbind(aux2,aux1)
  print(nrow(cranckFeatureSelectedNegatives))
}
aux2 <- aux1
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  count = 1
  aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]==cranckFeatureSelectedNegatives$GameID)
  cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]!=cranckFeatureSelectedNegatives$GameID) 
  aux1 <- arrange(aux1,aux1$RealTime)
  limite = nrow(aux1)
  if (limite>1) {
    for(count1 in 2:limite) {
      if(aux1$GameID[count1-1]==aux1$GameID[count1]) {
        
        aux1$timePassFromLastStatus[count1] = difftime(aux1$RealTime[count1],aux1$RealTime[count1-1],units='secs')
      }
    }
    aux2 <- rbind(aux2,aux1)
    print(nrow(cranckFeatureSelectedNegatives))
  }
}



cranckFeatureSelectedNegatives <- aux2


## gerivando tempo entre sessoes


cranckFeatureSelected$AvgTimeBetWeenStatus <- cranckFeatureSelected$AvgTimeBetWeenStatus/60

cranckFeatureSelectedNegatives <- transform(cranckFeatureSelectedNegatives,rotulo = TRUE)
cranckFeatureSelected <- transform(cranckFeatureSelected,rotulo = TRUE)
cranckFeatureSelectedNegatives <- cranckFeatureSelected
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(rotulo==FALSE)
cranckFeatureSelectedNegatives <- group_by(cranckFeatureSelectedNegatives,GameID)
cranckFeatureSelectedNegatives <- arrange(cranckFeatureSelectedNegatives,cranckFeatureSelectedNegatives$RealTime)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>%  filter(n_distinct(RealTime)>2)
rows = nrow(cranckFeatureSelectedNegatives);

## gerando valores negativos

count = 1;
count1 = 1;
limite = 1;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1 <- arrange(aux1,aux1$RealTime)
limite = as.integer(runif(1,1,nrow(aux1)))
for(count1 in 1:limite) {
  aux1$rotulo[count1] = FALSE
}
aux2 <- aux1
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  count = 1
  aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]==cranckFeatureSelectedNegatives$GameID)
  cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]!=cranckFeatureSelectedNegatives$GameID) 
  limite = as.integer(runif(1,1,nrow(aux1)))
  aux1 <- arrange(aux1,aux1$RealTime)
  for(count1 in 1:limite) {
    aux1$rotulo[count1] = FALSE;
  }
  aux2 <- rbind(aux2,aux1)
  print(nrow(cranckFeatureSelectedNegatives))
}
aux2 <- filter(aux2,aux2$rotulo==FALSE)
cranckFeatureSelectedNegatives <- aux2
cranckFeatureSelectedNegatives <- rbind(cranckFeatureSelectedNegatives,cranckFeatureSelected)


## gerando valores negativos



## gerar tempo de entre as sessoes medio

cranckFeatureSelectedNegatives$AvgTimeBetWeenStatus <- cranckFeatureSelectedNegatives$AvgTimeBetWeenStatus/60


count = 1;
count1 = 1;
limite = 1;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1<- aux1 %>% group_by(GameID) %>%
    transform(AvgTimeBetWeenStatus = mean(timePassFromLastStatus))
aux2 <- aux1
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  count = 1
  if(nrow(cranckFeatureSelectedNegatives)==0) {
    break();
  }
  aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]==cranckFeatureSelectedNegatives$GameID)
  cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]!=cranckFeatureSelectedNegatives$GameID)
  aux1<- aux1 %>% group_by(GameID) %>%
    transform(AvgTimeBetWeenStatus = mean(timePassFromLastStatus))
  aux2 <- rbind(aux2,aux1)
  print(nrow(cranckFeatureSelectedNegatives))
}

cranckFeatureSelectedNegatives <- aux2

## gerando avg time cranked

cranckFeatureSelected$TimeCranked <- cranckFeatureSelected$AvgTimeBetWeenStatus/60


count = 1;
count1 = 1;
limite = 1;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1<- aux1 %>% group_by(GameID) %>%
  transform(AvgTimeBetWeenStatus = mean(timePassFromLastStatus))
aux2 <- aux1
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  count = 1
  if(nrow(cranckFeatureSelectedNegatives)==0) {
    break();
  }
  aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]==cranckFeatureSelectedNegatives$GameID)
  cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]!=cranckFeatureSelectedNegatives$GameID)
  aux1<- aux1 %>% group_by(GameID) %>%
    transform(AvgTimeBetWeenStatus = mean(timePassFromLastStatus))
  aux2 <- rbind(aux2,aux1)
  print(nrow(cranckFeatureSelectedNegatives))
}

cranckFeatureSelectedNegatives <- aux2


## gerar quantidade de status/sessoes de jogo


count = 1;
count1 = 1;
limite = 1;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1<- transform(aux1,NumberofStatus = nrow(aux1))
aux2 <- aux1
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  count = 1
  if(nrow(cranckFeatureSelectedNegatives)==0) {
    break();
  }
  aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
  cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
  aux1<- transform(aux1,NumberofStatus = nrow(aux1))
  aux2 <- rbind(aux2,aux1)
  print(nrow(cranckFeatureSelectedNegatives))
}

cranckFeatureSelectedNegatives <- aux2

##gerando avg gameTime Per Day played


count = 1;
count1 = 1;
limite = 1;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1 <- arrange(aux1, desc(aux1$GameTime))
aux1 <- transform(aux1,AvgGameTimePlayedPerDay = aux1$GameTime[1]/aux1$NumbDaysPlay)
aux2 <- aux1
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  count = 1
  if(nrow(cranckFeatureSelectedNegatives)==0) {
    break();
  }
  aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
  cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
  aux1 <- arrange(aux1, desc(aux1$GameTime))
  aux1 <- transform(aux1,AvgGameTimePlayedPerDay = aux1$GameTime[1]/aux1$NumbDaysPlay)
  aux2 <- rbind(aux2,aux1)
  print(nrow(cranckFeatureSelectedNegatives))
}

cranckFeatureSelectedNegatives <- aux2


## gerando END Game flag

cranckFeatureSelectedNegatives <- transform(cranckFeatureSelectedNegatives, EndGameFlag = FALSE)
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  if(cranckFeatureSelectedNegatives$HelmTotalJumps[count] > 0) {
    cranckFeatureSelectedNegatives$EndGameFlag[count] = TRUE
  }
}

## gerando numero de dias jogados


cranckStatus <- cranckStatus %>%
                          filter(GameID!='') %>%
                          transform(DateInDays=as.Date(RealTime, format = "%Y-%m-%d"))


count = 1;
count1 = 1;
limite = 1;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1<- aux1 %>% group_by(GameID) %>%
  transform(NumbDaysPlay = n_distinct(DateInDays))
aux2 <- aux1
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  count = 1
  aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]==cranckFeatureSelectedNegatives$GameID)
  cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]!=cranckFeatureSelectedNegatives$GameID)
  aux1 <- arrange(aux1,aux1$RealTime)
  aux1<- aux1 %>% group_by(GameID) %>%
    transform(NumbDaysPlay = n_distinct(DateInDays))
  aux2 <- rbind(aux2,aux1)
  print(nrow(cranckFeatureSelectedNegatives))
}
cranckFeatureSelectedNegatives <- aux2



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

aux2 <- select(cranckFeatureSelectedNegatives, c(4,6,76,77,78,79,80,82,83,84,85))



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

## ploting models

fit.knn$pred

ggplot(fit.knn$pred[selectedIndices, ], 
       aes(m = M, d = factor(obs, levels = c("R", "M")))) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()



cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>%  filter(rotulo == FALSE)


cranckStatus %>% group_by(GameID) %>%
    arrange(RealTime) %>%
    filter(n_distinct(RealTime) > 2)

cranckStatus %>% mutate(ResearchPoints = ResearchAntiMatter+
                           ResearchBattery+
                           ResearchSystem+
                           ResearchCrank+
                           ResearchCrankBot+
                           ResearchDuranium+
                           ResearchSolarPanel+
                           ResearchScanner+
                           ResearchPhasers+
                           ResearchPlasma+
                           ResearchHelm+
                           ResearchScrapMetal+
                           ResearchPhotonTorp+
                           ResearchScrapCannon+
                           ResearchHull+
                           ResearchShields+
                           ResearchDecrypter) %>%
                    arrange(desc(ResearchPoints)) %>%
                    select(ResearchPoints) %>%
                    head(1) %>%
                    summarise(ResearchPoints)

cranckStatus <- mutate(cranckStatus,ResearchPoints = ResearchAntiMatter+
                                   +                                                      ResearchBattery+
                                   +                                                      ResearchSystem+
                                   +                                                      ResearchCrank+
                                   +                                                      ResearchCrankBot+
                                   +                                                      ResearchDuranium+
                                   +                                                      ResearchSolarPanel+
                                   +                                                      ResearchScanner+
                                   +                                                      ResearchPhasers+
                                   +                                                      ResearchPlasma+
                                   +                                                      ResearchHelm+
                                   +                                                      ResearchScrapMetal+
                                   +                                                      ResearchPhotonTorp+
                                   +                                                      ResearchScrapCannon+
                                   +                                                      ResearchHull+
                                   +                                                      ResearchShields+
                                   +                                                      ResearchDecrypter)



  


cranckStatus <- mutate(cranckStatus,resourcePoints = cranckStatus$PowerGenerated+
                         cranckStatus$ScrapMetalAmountFabricated+
                         cranckStatus$BatteryAmountFabricated+
                         cranckStatus$CrankBotAmountFabricated+
                         cranckStatus$DuraniumAmountFabricated+
                         cranckStatus$SolarPanelAmountFabricated+
                         cranckStatus$ItemAntiMatterAmount+
                         cranckStatus$ItemPhotonTorpAmount+
                         cranckStatus$ItemQuantumComputerAmount)


cranckStatus$PowerGenerated <- normalize(x = cranckStatus$PowerGenerated)
cranckStatus$ScrapMetalAmountFabricated <- normalize(x = cranckStatus$ScrapMetalAmountFabricated)
cranckStatus$BatteryAmountFabricated <- normalize(x = cranckStatus$BatteryAmountFabricated)
cranckStatus$CrankBotAmountFabricated <- normalize(x = cranckStatus$CrankBotAmountFabricated)
cranckStatus$DuraniumAmountFabricated <- normalize(x = cranckStatus$DuraniumAmountFabricated)
cranckStatus$SolarPanelAmountFabricated <- normalize(x = cranckStatus$SolarPanelAmountFabricated)
cranckStatus$ItemAntiMatterAmount <- normalize(x = cranckStatus$ItemAntiMatterAmount)
cranckStatus$ItemPhotonTorpAmount <- normalize(x = cranckStatus$ItemPhotonTorpAmount)
cranckStatus$ItemQuantumComputerAmount <- normalize(x = cranckStatus$ItemQuantumComputerAmount)

cranckStatus$resourcePoints <- cranckStatus$resourcePoints/9






