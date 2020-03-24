## HOMEWORK 9
# Load Data & Packages
install.packages("kernlab")
install.packages("e1071")
install.packages("gridExtra")
library(kernlab)
library(ggplot2)
library(e1071)
library(gridExtra)
df <- airquality
dfNA <- na.omit(df)
dfNA
# Create SVM Setup
randIndex <- sample(1:dim(dfNA)[1])
summary(randIndex)
cutPoint2_3 <- floor(nrow(dfNA)*2/3)
cutPoint2_3
trainData <- dfNA[randIndex[1:cutPoint2_3],]
trainData
testData <- dfNA[randIndex[(cutPoint2_3+1):nrow(dfNA)],]
testData
# Build model using KSVM
colnames(dfNA)
svmOut <- ksvm(Ozone ~ Solar.R + Wind + Temp + Month + Day, data=trainData)
svmOut
# Test the model and calc RMSE
svmPred <- predict(svmOut, testData)
svmPred
RMSE = function(m, o){
    sqrt(mean((m - o)^2))}
outRMSE <- RMSE(svmPred, testData$Ozone)
outRMSE
# Plot
ozoneErr <- testData$Ozone - svmPred
dfTesRes <- data.frame(testData, "Error"=ozoneErr)
dfTesRes
p1 <- ggplot(dfTesRes, aes(x=Temp, y=Wind,)) + geom_point(aes(size=Error, color=Error)) + ggtitle("KSVM Plot for Ozone")
p1
ozLm <- lm(Ozone ~ Solar.R + Temp + Month + Wind, data=trainData)
ozLmPred <- predict(ozLm, testData)
outOzLmRMSE <- RMSE(ozLmPred, testData$Ozone)
outOzLmRMSE
ozLmErr <- testData$Ozone - ozLmPred
dfLmRes <- data.frame(testData, "Error"=ozLmErr)
p2 <- ggplot(dfLmRes, aes(x=Temp, y=Wind,)) + geom_point(aes(size=Error, color=Error)) + ggtitle("LM Plot for Ozone")
p2
ozSVM <- svm(Ozone ~ Solar.R + Temp + Month + Wind, data=testData)
ozSvPred <- predict(ozSVM, testData)
ozSvRMSE <- RMSE(ozSvPred, testData$Ozone)
ozOzErr <- testData$Ozone - ozSvPred
dfSvRes <- data.frame(testData, "Error"=ozOzErr)
p3 <- ggplot(dfSvRes, aes(x=Temp, y=Wind,)) + geom_point(aes(size=Error, color=Error)) + ggtitle("SVM Plot for Ozone")
grid.arrange(p1, p2, p3, ncol=2)
# Good Ozone
goodOz <- as.integer(dfNA$Ozone>=mean(dfNA$Ozone))
# Improve Prediction
dfOz <- data.frame(dfNA, "goodOzone"=goodOz)
TrainOz <- dfOz[randIndex[1:cutPoint2_3],]
TestOz <- dfOz[randIndex[(cutPoint2_3+1): nrow(dfOz)],]
ozKSVM <- ksvm(goodOzone ~ Solar.R + Temp + Month + Wind, data=TrainOz)
ozKSVM
# Testing
ozPredKSVM <- predict(ozKSVM, TestOz)
ozPredKSVM_Round <- round(ozPredKSVM)
ozCorrect <- ozPredKSVM == TestOz$goodOzone
ozCorrect
sum(ozCorrect)/length(ozCorrect)
# Plot it
p4 <- data.frame(dfNA, "Prediciton"= ozPredKSVM, "Correct"=ozCorrect)
p5 <- ggplot(p4, aes(x=Temp, y=Wind,)) + geom_point(aes(shape=factor(ozPredKSVM), size=!Correct, color=factor(goodOz))) + scale_size_discrete(name="Incorrect")+ scale_shape(name="Prediction")+ scale_color_discrete(name="goodOzone") + ggtitle("KVSM Model")
p5
# naivebayes
TrainOz$goodOzone <- as.factor(TrainOz$goodOzone)
bayOz <- naiveBayes(goodOzone ~ Solar.R + Temp + Month + Wind, data = TrainOz)
bayOzPred <- predict(bayOz, TrainOz)
bayOzCorr <- bayOzPred == TrainOz$goodOzone
bayOzPlot <- data.frame(TestOz, "Prediction"=bayOzPred, "Correct"=bayOzCorr)
p6 <- ggplot(bayOz, aes(x=Temp, y=Wind,))+geom_point(aes(shape=factor(Prediction), size=!Correct, color=factor(goodOz)))+scale_size_discrete(name="Incorrect")+scale_shape(name="Prediction")+scale_color_discrete(name="goodOzone")+ggtitle("Naive Bay")
# everything
grid.arrange(p2, p3, p5, p6, ncol=2)



