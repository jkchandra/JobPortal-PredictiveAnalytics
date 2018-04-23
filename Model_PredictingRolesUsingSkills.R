#########################################################################################################
# BC2407 Semester Analytics Project: Model Predicting Roles Using Skills
# Team Number 8
# Members: Christopher Gerard Toh, Teo Tian Shun Kenneth, Lim De Quan, Jonathan Kevin Chandra
# Datasets: final_title_skills.csv
# Library: e1071, caTools
########################################################################################################


library(e1071)
library(caTools)
setwd("C:/Users/jkchandra/Desktop/BC2407/Semester Project")
set.seed(1337)
source("functionsFile.R")
df1 <- read.csv("final_title_skills.csv")

#Splitting Train and Test Set based on 70% Train 30% Test
train.split <- sample.split(Y = df1$jobtitle, SplitRatio = 0.7)
train <- subset(df1, train.split == T)
test <- subset(df1, train.split == F)


# BASELINE CLASSIFIER ===================================================================
#baseline model accuracy(based on probability of always choosing most frequent roles Top 1 and Top 5
trainFreq <- summary(train$jobtitle)
testFreq <- summary(test$jobtitle)
testFreq[1][1]/length(test$jobtitle)
## 5.3% MR = 94.7%
(testFreq[1][1]+testFreq[2][1]+testFreq[3][1]+testFreq[4][1]+testFreq[5][1])/length(test$jobtitle)
## 21% MR = 79%


#SUPPORT VECTOR MACHINE CLASSIFIER =====================================================================
m1 <- svm( train$jobtitle~., train, kernel = "linear",scale = FALSE , 
           probability = TRUE, cross = 10)
#m1 <- svm( train$jobtitle~., train, kernel = "polynomial",scale = FALSE , 
#           probability = TRUE, cross = 10)
#m1 <- svm( train$jobtitle~., train, kernel = "radial",scale = FALSE , 
#           probability = TRUE, cross = 10)
#m1 <- svm( train$jobtitle~., train, kernel = "sigmoid",scale = FALSE , 
#           probability = TRUE, cross = 10)

#Find the probability of each row
predicted <- predict(m1, newdata = test, probability = TRUE)
results <- data_frame(actual = test$jobtitle)
results_prob <- attr(predicted, "probabilities")

#Getting the top 1 to 5 most probable job titles from the list of probabilities
results$max1 <- colnames(results_prob)[apply(results_prob, 1, which.max)]
results$max2 <- colnames(results_prob)[apply(results_prob, 1, which.max2)]
results$max3 <- colnames(results_prob)[apply(results_prob, 1, which.max3)]
results$max4 <- colnames(results_prob)[apply(results_prob, 1, which.max4)]
results$max5 <- colnames(results_prob)[apply(results_prob, 1, which.max5)]

#Set the score for test set accuracy
results$scoretop1 <- ifelse((predicted == results$actual),1,0)
results$scoretop5 <- ifelse((results$max1 == results$actual | results$max2 == results$actual|
                            results$max3 == results$actual| results$max4 == results$actual|
                            results$max5 == results$actual),1,0)
#Top 1 Test Set Accuracy
sum(results$scoretop1)/length(results$scoretop1)
##30% MR = 70%

#Top 5 Test Set Accuracy
sum(results$scoretop5)/length(results$scoretop5)
##58.3% MR = 41.7%



#NAIVE BAYES CLASSIFIER ======================================================================
m2 <- naiveBayes(jobtitle~., data = train)
predicted2 <- predict(m2, newdata = test, type = "raw")

results2 <- data_frame(actual = test$jobtitle)
results_prob2 <- predicted2

#Getting the top 1 to 5 most probable job titles from the list of probabilities
results2$max1 <- colnames(results_prob2)[apply(results_prob2, 1, which.max)]
results2$max2 <- colnames(results_prob2)[apply(results_prob2, 1, which.max2)]
results2$max3 <- colnames(results_prob2)[apply(results_prob2, 1, which.max3)]
results2$max4 <- colnames(results_prob2)[apply(results_prob2, 1, which.max4)]
results2$max5 <- colnames(results_prob2)[apply(results_prob2, 1, which.max5)]

#Set the score for test set accuracy
results2$scoretop1 <- ifelse((predicted2 == results2$actual),1,0)
results2$scoretop5 <- ifelse((results2$max1 == results2$actual | results2$max2 == results2$actual|
                                results2$max3 == results2$actual| results2$max4 == results2$actual|
                                results2$max5 == results2$actual),1,0)
#Top 1 Test Set Accuracy
sum(results2$scoretop1)/length(results2$scoretop1)
##0% MR = 100%
#Top 5 Test Set Accuracy
sum(results2$scoretop5)/length(results2$scoretop5)
##7% MR = 93%

