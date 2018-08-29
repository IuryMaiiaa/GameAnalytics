## derivação de variaveis do Cranck

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

cranckStatus <- read.csv("D:\\R projetos\\Cranck dados e codigo\\06")
cranckFeatureSelected <- read.csv("D:\\R projetos\\Cranck dados e codigo\\CranckFeatureSelected.txt")
cranckFeatureSelectedNegatives <- cranckFeatureSelected
cranckFeatureSelected <- 0
cranckFeatureSelectedNegatives <- read.csv("D:\\R projetos\\Cranck dados e codigo\\cranckFeaturesWithNegatives.txt")
drops <- c("X","X.1","X.2")
cranckFeatureSelected <- cranckFeatureSelected[, !(names(cranckFeatureSelected) %in% drops)]
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives[, !(names(cranckFeatureSelectedNegatives) %in% drops)]

write.csv(x = aux2,file = "D:\\R projetos\\Cranck dados e codigo\\CranckFeatureSelected.txt")
write.csv(x = cranckFeatureSelectedNegatives,file = "D:\\R projetos\\Cranck dados e codigo\\cranckFeaturesWithNegatives.txt")
write.csv(x = aux2,file = "D:\\R projetos\\Cranck dados e codigo\\cranckFeaturesWithTimeCranked.csv")
write.csv(x = aux2,file = "D:\\R projetos\\Cranck dados e codigo\\cranckFeaturesWithoutTimeCranked.csv")

cranckFeatureSelected %>% group_by(GameID) %>% summarise(n())

## derivando tempo entre sessoes

count = 1;
count1 = 1;
limite = 1;
cranckFeatureSelectedNegatives["timePassFromLastStatus"] <- 0;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1 <- arrange(aux1,aux1$RealTime)
limite = nrow(aux1)
if (limite>1) {
  for(count1 in 2:limite) {
    if(aux1$GameID[count1-1]==aux1$GameID[count1]) {
      
      aux1$timePassFromLastStatus[count1] = difftime(aux1$RealTime[count1],aux1$RealTime[count1-1],units='mins')
    }
  }
  aux2 <- aux1
  print(nrow(cranckFeatureSelectedNegatives))
} else {
  if (aux2==0) {
    aux2 <- aux1
  } else {
    aux2 <- rbind(aux2,aux1)
  }
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
        
        aux1$timePassFromLastStatus[count1] = difftime(aux1$RealTime[count1],aux1$RealTime[count1-1],units='mins')
      }
    }
    aux2 <- rbind(aux2,aux1)
    print(nrow(cranckFeatureSelectedNegatives))
  } else {
    if (aux2==0) {
      aux2 <- aux1
    } else {
      aux2 <- rbind(aux2,aux1)
    }
  }
}



cranckFeatureSelectedNegatives <- aux2
aux2 <- 0
aux1 <- 0
## derivando tempo entre sessoes
cranckFeatureSelectedNegatives$GameTime <- cranckFeatureSelectedNegatives$GameTime/60;

## derivando sessoes

count = 1;
count1 = 2;
limite = 1;
aux3 <- 0
cranckFeatureSelectedNegatives["Session"] <- 0;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1 <- arrange(aux1,aux1$RealTime)
limite = nrow(aux1)
SessaoCount = 0;
if (limite>1) {
  for(count1 in 2:limite) {
    if((aux1$GameTime[count1-1]-aux1$GameTime[count1]) + 10 >= aux1$timePassFromLastStatus[count1] ) {
      #aqui adicionar um status a uma sessao
      aux1$Session[count1] = SessaoCount;
      if (count1 == limite) {
        if(aux3==0) {
          aux3 <- aux1[count1,]
        } else {
          aux3 <- rbind(aux3,aux1[count1,])
        }
      }
    } else {
      #aqui fecha uma sessão para o inicio de uma outra
      SessaoCount = SessaoCount + 1;
      aux1$Session[count1] = SessaoCount;
      if(aux3==0) {
        aux3 <- aux1[count1-1,]
      } else {
        aux3 <- rbind(aux3,aux1[count1-1,])
      }
    }
  }
  print(nrow(cranckFeatureSelectedNegatives))
} else {
  aux1$Session[count1] = SessaoCount;
  SessaoCount = SessaoCount + 1;
  if(aux3==0) {
    aux3 <- aux1[limite,]
  } else {
    aux3 <- rbind(aux3,aux1[limite,])
  }
}
aux2 <- aux1
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  count = 1
  SessaoCount=1
  aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]==cranckFeatureSelectedNegatives$GameID)
  cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]!=cranckFeatureSelectedNegatives$GameID) 
  aux1 <- arrange(aux1,aux1$RealTime)
  count1 = 1;
  limite = nrow(aux1)
  if (limite>1) {
    for(count1 in 2:limite) {
      if((aux1$GameTime[count1-1]-aux1$GameTime[count1]) + 10 >= aux1$timePassFromLastStatus[count1] ) {
        #aqui adicionar um status a uma sessao
        aux1$Session[count1] = SessaoCount;
        if (count1 == limite) {
          if(aux3==0) {
            aux3 <- aux1[count1,]
          } else {
            aux3 <- rbind(aux3,aux1[count1,])
          }
        }
      } else {
        #aqui fecha uma sessão para o inicio de uma outra
        SessaoCount = SessaoCount + 1;
        aux1$Session[count1] = SessaoCount;
        if(aux3==0) {
          aux3 <- aux1[count1-1,]
        } else {
          aux3 <- rbind(aux3,aux1[count1-1,])
        }
      }
    }
    aux2 <- rbind(aux2,aux1)
    print(nrow(cranckFeatureSelectedNegatives))
  } else {
    aux1$Session[count1] = SessaoCount;
    SessaoCount = SessaoCount + 1;
    aux2 <- rbind(aux2,aux1)
    if(aux3==0) {
      aux3 <- aux1[limite,]
    } else {
      aux3 <- rbind(aux3,aux1[limite,])
    }
  }
}



cranckFeatureSelectedNegatives <- aux2

## derivando sessoes




## sessoes limpando as sessoes

count = 1;
count1 = 1;
limite = 1;
aux3 <- 0;
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  count = 1
  SessaoCount=1
  aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]==cranckFeatureSelectedNegatives$GameID)
  cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]!=cranckFeatureSelectedNegatives$GameID) 
  aux1 <- arrange(aux1,aux1$RealTime)
  count1 = 1;
  limite = nrow(aux1)
  if (limite>1) {
    for(count1 in 1:nrow(aux1)) {
      count1=1
      aux2 <- aux1 %>% filter(aux1$Session[count1]==aux1$Session)
      aux1 <- aux1 %>% filter(aux1$Session[count1]!=aux1$Session) 
      aux2 <- arrange(aux2,aux2$RealTime)
      aux2 <- tail(aux2,1)
      if(aux3==0) {
        aux3 <- aux2
      } else {
        aux3 <- rbind(aux3,aux2)
      }
    }
    print(nrow(cranckFeatureSelectedNegatives))
  }else {
    if(aux3==0) {
      aux3 <- aux1
    } else {
      aux3 <- rbind(aux3,aux1)
    }
  }
}

cranckFeatureSelectedNegatives <- aux3

## sessoes limpando as sessoes


## gerar quantidade de status/sessoes de jogo

count = 1;
count1 = 1;
limite = 1;
cranckFeatureSelectedNegatives["NumberOfSessions"] <- 1;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1<- transform(aux1,NumberOfSessions = nrow(aux1))
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
aux2<-0
aux1<-0

## gerar quantidade de status/sessoes de jogo

## gerando numero de dias jogados


cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>%
  filter(GameID!='') %>%
  transform(DateInDays=as.Date(RealTime, format = "%Y-%m-%d"))



count = 1;
count1 = 1;
limite = 1;
cranckFeatureSelectedNegatives["NumberOfDays"] <- 1;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1 <- arrange(aux1,aux1$RealTime)
limite <- nrow(aux1)
numberOfDays <- 1
for( count in 1:limite) {
  if (count == 1) {
    numberOfDays = as.integer((aux1$GameTime[count]/60)/24) + 1;
  } else if (aux1$DateInDays[count]!=aux1$DateInDays[count-1]) {
    numberOfDays = numberOfDays + as.integer(((aux1$GameTime[count-1] - aux1$GameTime[count])/60)/24) + 1;
  }
}
aux1$NumberOfDays <- numberOfDays;
numberOfDays <- 1
aux2 <- aux1
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  count = 1
  aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]==cranckFeatureSelectedNegatives$GameID)
  cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[count]!=cranckFeatureSelectedNegatives$GameID)
  aux1 <- arrange(aux1,aux1$RealTime)
  limite <- nrow(aux1)
  numberOfDays <- 1
  for( count in 1:limite) {
    if (count == 1) {
      numberOfDays = as.integer((aux1$GameTime[count]/60)/24) + 1;
    } else if (aux1$DateInDays[count1]!=aux1$DateInDays[count1-1]) {
      numberOfDays = numberOfDays + as.integer(((aux1$GameTime[count-1] - aux1$GameTime[count])/60)/24) + 1;
    }
  }
  aux1$NumberOfDays <- numberOfDays;
  numberOfDays <- 1
  aux2 <- aux1
}
cranckFeatureSelectedNegatives <- aux2
aux1<-0
aux2<-0
## gerando numero de dias jogados


## gerando valores negativos


cranckFeatureSelectedNegatives["Rotulo"] <- 1;
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


## gerar tempo de entre as sessoes médio


count = 1;
count1 = 1;
limite = 1;
aux1 <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]==cranckFeatureSelectedNegatives$GameID)
cranckFeatureSelectedNegatives <- cranckFeatureSelectedNegatives %>% filter(cranckFeatureSelectedNegatives$GameID[1]!=cranckFeatureSelectedNegatives$GameID) 
aux1<- aux1 %>% group_by(GameID) %>%  transform(AvgTimeBetWeenStatus = mean(timePassFromLastStatus))
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
aux2<-0
aux1<-0

## gerar tempo de entre as sessoes médio







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


##gerando avg gameTime Per Day played


## gerando END Game flag

cranckFeatureSelectedNegatives <- transform(cranckFeatureSelectedNegatives, EndGameFlag = FALSE)
for (count in 1:nrow(cranckFeatureSelectedNegatives)) {
  if(cranckFeatureSelectedNegatives$HelmTotalJumps[count] > 0) {
    cranckFeatureSelectedNegatives$EndGameFlag[count] = TRUE
  }
}


## gerando END Game flag

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

## gerando numero de dias jogados


## criando seção por dia baseado no dia jogado.

count = 1;
count1 = 1;
limite = 1;
aux1 <- cranckStatus %>% filter(cranckStatus$GameID[1]==cranckStatus$GameID)
cranckStatus <- cranckStatus %>% filter(cranckStatus$GameID[1]!=cranckStatus$GameID) 
aux2 <- aux1 %>% filter(aux1$DateInDays[1]==aux1$DateInDays)
aux1 <- aux1 %>% filter(aux1$DateInDays[1]!=aux1$DateInDays)
aux2 <- arrange(aux2,desc(aux2$GameTime))



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


## criando status baseado no dia jogado.

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

## gerando avg time cranked
