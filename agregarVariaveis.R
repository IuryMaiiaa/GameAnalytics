## Agregando Variaveis.


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
cranckFeatureSelected$ScrapMetalAmountFabricated <- normalize(x = cranckFeatureSelected$ScrapMetalAmountFabricated)
cranckStatus$BatteryAmountFabricated <- normalize(x = cranckStatus$BatteryAmountFabricated)
cranckStatus$CrankBotAmountFabricated <- normalize(x = cranckStatus$CrankBotAmountFabricated)
cranckStatus$DuraniumAmountFabricated <- normalize(x = cranckStatus$DuraniumAmountFabricated)
cranckStatus$SolarPanelAmountFabricated <- normalize(x = cranckStatus$SolarPanelAmountFabricated)
cranckStatus$ItemAntiMatterAmount <- normalize(x = cranckStatus$ItemAntiMatterAmount)
cranckStatus$ItemPhotonTorpAmount <- normalize(x = cranckStatus$ItemPhotonTorpAmount)
cranckStatus$ItemQuantumComputerAmount <- normalize(x = cranckStatus$ItemQuantumComputerAmount)

cranckStatus$resourcePoints <- cranckStatus$resourcePoints/9
