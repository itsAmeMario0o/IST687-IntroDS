## Part 1
printVecInfo <-function(vecThis){
    cat("Mean:", mean(vecThis), "\n")
    cat("Middle:", median(vecThis), "\n")
    cat("Low:", min(vecThis), "\n")
    cat("High:", max(vecThis), "\n")
    cat("Standard Deviation:", sd(vecThis), "\n")
    cat("Quantile:", quantile(vecThis, 0.05),"--", quantile(vecThis, 0.95), "\n")
    }
printVecInfo
vec <- c(1,2,3,4,5,6,7,8,9,10,50)

## Part 2
jar <- c(rep("red", 50), rep("blue", 50))
sum(jar=="red")

jsamp <- sample(jar, 10, replace = TRUE)
sum(jsamp == "red")
sum(jsamp == "red") / length(jsamp)

ssize = 10
replicateThis <- replicate(20, sum(sample(jar, ssize, replace = TRUE) == "red") /ssize)
hist(replicateThis)

ssizeHundred = ssize*10
ssizeHundred

replicateAgain <- replicate(100, sum(sample(jar, ssizeHundred, replace = TRUE) == "red") /ssize)
hist(replicateAgain)

#Part 3
atemp <- airquality

atempClean <- atemp[rowSums(is.na(atemp)) <= 0,]

printVecInfo(atempClean$Ozone)
hist(atempClean$Ozone)

printVecInfo(atempClean$Wind)
hist(atempClean$Wind)

printVecInfo(atempClean$Temp)
hist(atempClean$Temp)