## Mario Ruiz Homework 8

## Step 1 & 2
library("gdata")
library("readxl")
df <- read_excel("Google Drive File Stream/My Drive/Syracuse/IST 687 - Intro to Data Science/Week 8/mlr01.xls")
View(df)
cNames <- c("Babies", "Adults","Precipitation", "Winter")
colnames(df) <- cNames
View(df)
## Step 3
str(df)
## Step 4
library("ggplot2")
babyAdults <- ggplot(df, aes(x = Adults, y= Babies)) + geom_point()
babyAdults + stat_smooth(method = "lm", col = "blue")
##
babyWeather <- ggplot(df, aes(x = Precipitation, y= Babies)) + geom_point()
babyWeather + stat_smooth(method = "lm", col = "blue")
##
babyWinter <- ggplot(df, aes(x = Winter, y= Babies)) + geom_point()
babyWinter + stat_smooth(method = "lm", col = "blue")
## Step 5 
winter <- lm(formula = Babies ~ Winter, data = df)
summary(winter)
##
winterP <- lm(formula = Babies ~ Winter + Precipitation, data = df)
summary(winterP)
##
babies <- lm(formula = Babies ~ ., data = df)
summary(babies)
## Questions - Which is the best model? The "babies" model as it returns an adjusted R-Squared of 0.955
## Questions - Which of the predictors are statistically significant? winter model - winter is significant Pr- 0.03 
## winterP model - Precipitation is significant Pr 0.008
## babies model - All the values are considered significant 
## Questions - Which would be the most parsimonious? 
adults <- lm(formula = Babies ~ Adults, data = df)
summary(adults)
##
adultsP <- lm(formula = Babies ~ Adults + Precipitation, data = df)
summary(adultsP)
## We should consider the adults variation as the r-squared is .861, where adding precipitation only slightly increased r-squared to .878

