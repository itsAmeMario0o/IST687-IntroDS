## Load Airquality data set
my.data <- airquality
my.data

## Remove NAs - first see what NAs are available
colSums(is.na(my.data))
## Remove NAs
cleandata <- na.omit(my.data)
## Verify
colSums(is.na(cleandata))

## Histograms for each variable
library(ggplot2)
ozoneViz <- ggplot(cleandata, aes(x=Ozone)) + geom_histogram(bins = 10, color="black", fill="red")
ozoneViz + ggtitle("Ozone Information")

windViz <- ggplot(cleandata, aes(x=Wind)) + geom_histogram(bins = 10, color="black", fill="blue")
windViz + ggtitle("Wind Information")

solarViz <- ggplot(cleandata, aes(x=Solar.R)) + geom_histogram(bins = 10, color="black", fill="yellow")
solarViz + ggtitle("Solar Information")

tempViz <- ggplot(cleandata, aes(x=Temp)) + geom_histogram(bins = 10, color="black", fill="green")
tempViz + ggtitle("Temp Information")

monthViz <- ggplot(cleandata, aes(x=Month)) + geom_histogram(bins = 12, color="black", fill="orange")
monthViz + ggtitle("Month Information")

dayViz <- ggplot(cleandata, aes(x=Day)) + geom_histogram(bins = 31, color="black", fill="purple")
dayViz + ggtitle("Day Information")

## Boxplot for Ozone
ozoneBox <- ggplot(cleandata, aes(x=factor(0), y=Ozone)) + geom_boxplot() + coord_flip()
ozoneBox + ggtitle("Ozone Boxplot")

##Boxplot for Wind
windBox <- ggplot(cleandata, aes(x=factor(0), y=Wind)) + geom_boxplot() + coord_flip()
windBox + ggtitle("Wind Boxplot")

## Dated since 1973
dated <- paste("1973", cleandata$Month, cleandata$Day, sep = "-")
## Sanity check
dated
## Format date
dated.v2 <- as.Date(dated)
dated.v2
## Revise with newly formated date
dfdated.v2 <- cbind(cleandata, dated.v2)
dfdated.v2

## Line chart for ozone
ozLine <- ggplot(dfdated.v2, aes(x=dated.v2, y=Ozone)) + geom_line(size=2, color="green")
ozLine + ggtitle("Ozone Line Chart")

## Line chart for temp
teLine <- ggplot(dfdated.v2, aes(x=dated.v2, y=Temp)) + geom_line(size=2, color="red")
teLine + ggtitle("Temp Line Chart")

## Line chart for wind
wiLine <- ggplot(dfdated.v2, aes(x=dated.v2, y=Wind)) + geom_line(size=2, color="blue")
wiLine + ggtitle("Wind Line Chart")

## Line chart for solar.r
ozLine <- ggplot(dfdated.v2, aes(x=dated.v2, y=Solar.R)) + geom_line(size=2, color="black")
ozLine + ggtitle("Solar.R Line Chart")

## Combine all into one chart
## Normalize - Min/Max
normOz <- (cleandata$Ozone - min(cleandata$Ozone))/(max(cleandata$Ozone)-min(cleandata$Ozone))
normOz
normTe <- (cleandata$Temp - min(cleandata$Temp))/(max(cleandata$Temp)-min(cleandata$Temp))
normTe
normWi <- (cleandata$Wind - min(cleandata$Wind))/(max(cleandata$Wind)-min(cleandata$Wind))
normWi
normSo <- (cleandata$Solar.R - min(cleandata$Solar.R))/(max(cleandata$Solar.R)-min(cleandata$Solar.R))
normSo
## Create new df with normalized data
dfNorm <- data.frame(normOz, normTe, normWi, normSo, "Date"=dfdated.v2$dated.v2, "Month"=dfdated.v2$Month, "Day"=dfdated.v2$Day)
## Plot it
everything <- ggplot(dfNorm, aes(x=dated.v2)) + geom_line(aes(y=normOz), size=1, color="blue") + geom_line(aes(y=normTe), size=1, color="red") + geom_line(aes(y=normWi), size=1, color="purple") + geom_line(aes(y=normSo), size=1, color="green")
everything + ggtitle("Normalized Everything") + ylab("Normalized via Min/Max")
## Heatmap
hmDates <- c(dfdated.v2$dated.v2, dfdated.v2$dated.v2, dfdated.v2$dated.v2, dfdated.v2$dated.v2 )
hmDay <- c(dfdated.v2$Day, dfdated.v2$Day, dfdated.v2$Day, dfdated.v2$Day)
hmVars <- c(dfNorm$normOz, dfNorm$normTe, dfNorm$normWi, dfNorm$normSo)
hmCat <- c(rep("Ozone",111), rep("Temp",111), rep("Wind",111), rep("Solar.R",111))
dfHM <- data.frame(hmDates,hmDay,hmVars,hmCat)
hm <- ggplot(dfHM, aes(x-hmDay, y=hmCat, fill=hmVars)) + geom_tile()
##
## FIX 
hm + scale_fill_gradient(name = "Normal", low ="green", high = "yellow") + ggtitle("Air Quality Heat Map") + ylab("Category") + xlab("Day")
##

scatData <- ggplot(dfHM, aes(x = normWi, y = normTe )) + geom_point(aes(size=normOz, color=normSo))
scatData + scale_color_gradient(low = "blue", high = "orange")


### SCATTER PLOT FOR THE WIN!