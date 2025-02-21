#Titus Karuri
#Unit 7

library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(e1071)

#part 1
Titanic = read.csv("titanic_train.csv")

Titanic$Survived <- as.factor(Titanic$Survived)
titanicClean = Titanic %>% filter(!is.na(Age) & !is.na(Pclass))
set.seed(4)
trainIndices = sample(seq(1:length(titanicClean$Age)), round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]

model = naiveBayes(x= trainTitanic[,c("Pclass","Age")],y= trainTitanic$Survived)

Thirty1= data.frame(Age = 30, Pclass = 1)
Thirty2= data.frame(Age = 30, Pclass = 2)
Thirty3= data.frame(Age = 30, Pclass = 3)

predict(model,Thirty1,type = "raw")
predict(model,Thirty2, type = "raw")
predict(model,Thirty3, type = "raw")

predict(model,Thirty1)
predict(model,Thirty2)
predict(model,Thirty3)

testTitanic$Survived <- as.factor(testTitanic$Survived)
NB_prediction <- predict(model,testTitanic)

CM <- confusionMatrix(NB_prediction,testTitanic$Survived)
CM

#part 1-2
Titanic$Survived <- as.factor(Titanic$Survived)
titanicClean = Titanic %>% filter(!is.na(Age) & !is.na(Pclass))
set.seed(7)
trainIndices = sample(seq(1:length(titanicClean$Age)), round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]

model = naiveBayes(x= trainTitanic[,c("Pclass","Age")],y= trainTitanic$Survived)

testTitanic$Survived <- as.factor(testTitanic$Survived)
NB_prediction <- predict(model,testTitanic)

CM <- confusionMatrix(table(NB_prediction,testTitanic$Survived))
CM

Titanic$Survived <- as.factor(Titanic$Survived)
titanicClean = Titanic %>% filter(!is.na(Age) & !is.na(Pclass))
set.seed(8)
trainIndices = sample(seq(1:length(titanicClean$Age)), round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]

model = naiveBayes(x= trainTitanic[,c("Pclass","Age")],y= trainTitanic$Survived)

testTitanic$Survived <- as.factor(testTitanic$Survived)
NB_prediction <- predict(model,testTitanic)

CM <- confusionMatrix(table(NB_prediction,testTitanic$Survived))
CM

Titanic$Survived <- as.factor(Titanic$Survived)
titanicClean = Titanic %>% filter(!is.na(Age) & !is.na(Pclass))
set.seed(9)
trainIndices = sample(seq(1:length(titanicClean$Age)), round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]

model = naiveBayes(x= trainTitanic[,c("Pclass","Age")],y= trainTitanic$Survived)

testTitanic$Survived <- as.factor(testTitanic$Survived)
NB_prediction <- predict(model,testTitanic)

CM <- confusionMatrix(table(NB_prediction,testTitanic$Survived))
CM


iterations = 100
accs <- numeric(iterations)
sensitivity_1 <- numeric(iterations)
specificity_1 <- numeric(iterations)

for(i in 1:iterations)
{
  set.seed(i)
  trainIndices = sample(seq(1:length(titanicClean$Age)), round(.7*length(titanicClean$Age)))
  trainTitanic = titanicClean[trainIndices,]
  testTitanic = titanicClean[-trainIndices,]
  
  model = naiveBayes(x= trainTitanic[,c("Pclass","Age")],y= trainTitanic$Survived)
  testTitanic$Survived <- as.factor(testTitanic$Survived)
  NB_prediction <- predict(model,testTitanic)
  
  CM <- confusionMatrix(table(NB_prediction,testTitanic$Survived))
  accs[i] <- CM$overall["Accuracy"]
  sensitivity_1[i] <- CM$byClass["Sensitivity"]
  specificity_1[i] <- CM$byClass["Specificity"]
}

mean_accs <- mean(accs)
mean_sensitivity <- mean(sensitivity_1)
mean_specificity <- mean(specificity_1)

print(mean_accs)
print(mean_sensitivity)
print(mean_specificity)


Titanic$Survived <- as.factor(Titanic$Survived)
titanicClean = Titanic %>% filter(!is.na(Age) & !is.na(Pclass))
set.seed(4)
trainIndices = sample(seq(1:length(titanicClean$Age)), round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]

model = naiveBayes(x= trainTitanic[,c("Pclass","Age","Sex")],y= trainTitanic$Survived)
testTitanic$Survived <- as.factor(testTitanic$Survived)
NB_prediction <- predict(model,testTitanic)

CM <- confusionMatrix(NB_prediction,testTitanic$Survived)
CM


iterations_2 = 100
accs_2 <- numeric(iterations)
sensitivity_2 <- numeric(iterations)
specificity_2 <- numeric(iterations)
for(i in 1:iterations_2)
{
  set.seed(i)
  trainIndices = sample(seq(1:length(titanicClean$Age)), round(.7*length(titanicClean$Age)))
  trainTitanic = titanicClean[trainIndices,]
  testTitanic = titanicClean[-trainIndices,]
  
  model = naiveBayes(x= trainTitanic[,c("Pclass","Age","Sex")],y= trainTitanic$Survived)
  testTitanic$Survived <- as.factor(testTitanic$Survived)
  NB_prediction <- predict(model,testTitanic)
  
  CM <- confusionMatrix(table(NB_prediction,testTitanic$Survived))
  accs_2[i] <- CM$overall["Accuracy"]
  sensitivity_2[i] <- CM$byClass["Sensitivity"]
  specificity_2[i] <- CM$byClass["Specificity"]
}

mean_accs_2 <- mean(accs_2)
mean_sensitivity_2 <- mean(sensitivity_2)
mean_specificity_2 <- mean(specificity_2)

print(mean_accs_2)
print(mean_sensitivity_2)
print(mean_specificity_2)


iterations_3 = 100
accs_3 <- numeric(iterations_3)
sensitivity_3 <- numeric(iterations_3)
specificity_3 <- numeric(iterations_3)
splitPerc = .7
for(i in 1:iterations_3)
{
  
  trainIndices = sample(1:dim(iris)[1],round(splitPerc * dim(iris)[1]))
  train_Iris = iris[trainIndices,]
  test_Iris = iris[-trainIndices,]
  model = naiveBayes(train_Iris[,c("Sepal.Length","Sepal.Width")],as.factor(train_Iris$Species),laplace = 1)
  NB_prediction_Iris <- predict(model,test_Iris[,c("Sepal.Length","Sepal.Width")])
  CM = confusionMatrix(NB_prediction_Iris,as.factor(test_Iris$Species))
  accs_3[i] <- CM$overall["Accuracy"]
  sensitivity_3[i] <- mean(CM$byClass[,"Sensitivity"])
  specificity_3[i] <- mean(CM$byClass[,"Specificity"])
}

mean_accs_3 <- mean(accs_3)
mean_sensitivity_3 <- mean(sensitivity_3)
mean_specificity_3 <- mean(specificity_3)

print(mean_accs_3)
print(mean_sensitivity_3)
print(mean_specificity_3)



