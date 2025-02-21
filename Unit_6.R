#Titus Karuri
#Unit 6
library(class)
library(caret)
library(e1071)

read.csv("titanic_train.csv")
Titanic = read.csv("titanic_train.csv")
set.seed(7)
splitPerc = .673

Titanic$Survived <- as.factor(Titanic$Survived)
Titanic$Pclass <- as.factor(Titanic$Pclass)
Titanic$Age <- as.numeric(Titanic$Age)
Titanic$Sex <- as.factor(Titanic$Sex)

t <- Titanic$Age[is.na(Titanic$Age)] <- median(Titanic$Age, na.rm = TRUE) 

trainIndices = sample(1:dim(Titanic)[1], round(splitPerc * dim(Titanic)[1]))  

train = Titanic[trainIndices,]
test = Titanic[-trainIndices,]

preprocValues <- preProcess(train[,c("Pclass","Age")],method = c("center","scale"))
train_set <- predict(preprocValues,train[,c("Pclass","Age")])
test_set <- predict(preprocValues,test[,c("Pclass","Age")])

classifications = knn(train_set,test_set,train$Survived, prob = TRUE, k = 5)
table(classifications, test$Survived)
confusionMatrix(table(classifications,test$Survived))

#Part 1-2

titanic_data <- Titanic[,c("Age","Pclass","Survived")]
titanic_data$Age <- scale(titanic_data$Age)

titanic_x <- titanic_data[,c("Age","Pclass")]
titanic_y <- titanic_data$Survived

age <- 20

predict_df <- data.frame(Age = rep(age,3), Pclass = factor(1:3,levels = c(1,2,3)))

knn_classification <- knn(titanic_x,predict_df,titanic_y,k = 5, prob = TRUE)

survival_prob <- attr(knn_classification,"prob")

predicted_results <- data.frame(Age = age, Pclass = 1:3, Survival_Probability = survival_prob)

print(predicted_results)

#Part 1-3

titanic_data_2 <- Titanic[,c("Survived", "Pclass","Age","Sex")]

train_male <- train %>% filter(Sex== "male")
train_female <- train %>% filter(Sex== "female")

test_male <- test %>% filter(Sex == "male")
test_female <- test %>% filter(Sex == "female")

train_male_x <- train_male[, c("Age", "Pclass")]
test_male_x <- test_male[, c("Age", "Pclass")]
train_male_y <- train_male$Survived
test_male_y <- test_male$Survived

train_female_x <- train_female[, c("Age", "Pclass")]
test_female_x <- test_female[, c("Age", "Pclass")]
train_female_y <- train_female$Survived
test_female_y <- test_female$Survived

preproc <- preProcess(train_male_x, method = c("center", "scale"))
train_male_x <- predict(preproc, train_male_x)
test_male_x <- predict(preproc, test_male_x)

preproc_female <- preProcess(train_female_x, method = c("center", "scale"))
train_female_x <- predict(preproc_female, train_female_x)
test_female_x <- predict(preproc_female, test_female_x)

knn_male <- knn(train_male_x,test_male_x,train_male_y,k=5)

knn_female <- knn(train_female_x,test_female_x,train_female_y, k=5)

confusionMatrix(knn_male,test_male_y)
confusionMatrix(knn_female,test_female_y)

#part 2

set.seed(6)
splitPerc = .7

iris_data <- iris[,c("Sepal.Length", "Sepal.Width", "Species")]

train_Iris_Indices = sample(1:dim(iris_data)[1], round(splitPerc * dim(iris_data)[1]))

train_x = iris[train_Iris_Indices,]
test_x = iris[-train_Iris_Indices,]

train_iris <- train_x[,c("Sepal.Length", "Sepal.Width")]
test_iris <- test_x[,c("Sepal.Length","Sepal.Width")]

train_y <- train_x$Species
test_y <- test_x$Species

k_value <- 1:90
accs <- numeric(length(k_value))

for (i in seq_along(k_value)) 
  {
  
  k <- k_value[i]
  classifications <- knn(train_iris,test_iris,train_y,k=k)
  accs[i] <- mean(classifications == test_y)
}

mean_accs <- mean(accs)
print(mean_accs)

df = data.frame(k=k_value,Accuracy = accs)

ggplot(df, aes(x=k,y = Accuracy)) + geom_line(color = "blue") +
  geom_point(color = "red") + labs(title = " K(x axis) vs KNN accuracy", x ="Number of K",
                                   y ="Accuracy")

best_k <- k_value[which.max(accs)]

irisVersVirg = iris %>% filter(iris$Species == "versicolor" | iris$Species == "virginica")




