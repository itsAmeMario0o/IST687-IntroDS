## Init myCars with data from mtcars
myCars <- mtcars
myCars
## Organize HP - Decending
hpDown <- myCars[order(myCars$hp, decreasing = TRUE),]
hpDown
## Get highest HP
highHP <- hpDown['hp'][1,]
highHP
## Get car name associated with highHP
carNameHP <- row.names(hpDown[1,])
carNameHP
## Marry Car Name and HP
joinHPName <- data.frame(carNameHP, highHP)
joinHPName
## Organize MPG - Decending
mpgDown <-myCars[order(myCars$mpg, decreasing = TRUE),]
mpgDown
## Get highest MPG
highMPG <- mpgDown['mpg'][1,]
highMPG
## Get car name associated with highMPG
carNameMPG <- row.names(mpgDown[1,])
carNameMPG
## Marry Car Name and MPG
joinMPGName <- data.frame(carNameMPG, highMPG)
joinMPGName
## Calculate the best car based on MPG and HP
## Calculate a percentile of both dimensions = bestCar
## Clean up output
mpgDimension <- (myCars$mpg - min(myCars$mpg)) / (myCars$mpg - max(myCars$mpg))
## - SCALE FUNCTION 
mpgScale <- scale(myCars$mpg)
mpgScale
##
mpgDimension
hpDimension <- (myCars$hp - min(myCars$hp)) / (myCars$hp - max(myCars$hp))
hpDimension
mergeDimensions <- mpgDimension + hpDimension
bestCar <- cbind(myCars, mpgDimension, hpDimension, mergeDimensions)
bestCar
cleanBestCar <- bestCar[1,]
cleanBestCar

bV279z