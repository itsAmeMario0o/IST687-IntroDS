#####    Load/Read Data    #####
install.packages("gdata")
library("gdata")
zipdf <- read.xls("Downloads/MedianZIP_2_2.xlsx")
zipdf

#Cleaning / Sanity Check
# zipClean <- zipdf[-1, ]
# zipNew <-colnames(zipdf) <- c("zip", "median", "mean", "pop")
# zipNew

zipdf$median <- as.numeric(zipdf$median)
zipdf$mean <- as.numeric(zipdf$mean)
zipdf$pop <- as.numeric(zipdf$pop)

zipdf$median
zipdf$mean
zipdf$pop

install.packages("zipcode")
library("zipcode")
data(zipcode)
zipnum <- zipcode
zipnum$zip <- as.numeric(zipcode$zip)
zipAlldf <- merge( zipdf, zipnum, all.x = TRUE)
# Sanity Check
zipAlldf
# Remove HI + AK + DC
sum(zipAlldf$state == "HI")
sum(zipAlldf$state == "AK")
sum(zipAlldf$state == "DC")
statesExcluded <- zipAlldf [!(zipAlldf$state == "HI" | zipAlldf$state == "AK" | zipAlldf$state == "DC"),]
# Sanity Check
str(statesExcluded)

##### Income per State #####
averageMed <- data.frame("MedianIncome" = tapply(statesExcluded$median,statesExcluded$state, mean))
statesIncomes <- data.frame("state.abb" = row.names(averageMed), averageMed)
stateNames <- data.frame(state.abb, state.abb)
statesIncomes2 <- merge(statesIncomes, stateNames, all.x = TRUE)
statesIncomes2$state.abb <- tolower(statesIncomes2$state.abb)
colnames(statesIncomes2) <- c("StateAbb", "MedianIncome", "States")
##
library(ggplot2)
library(ggmap)
map.Income <- ggplot(statesIncomes2, aes(map_id=State)) + geom_map(map = statesExcluded, color="black", aes( fill=statesIncomes2)) + scale_fill_gradient(low = "black", high = "blue") + expand_limits(x = us$long, y = us$lat) + coord_map()+ ggtitle("Median Income US",)
map.Income

##### Population per State #####

statesPop <- data.frame("Population" = statesExcluded$pop, "States" = statesExcluded$state, "City" = statesExcluded$city)
statesPop
map.Population <- ggplot(statesPop, aes(map_id=statesExcluded$state)) + geom_map(map = statesExcluded, color="black", aes(fill=base2010)) + scale_fill_gradient(name="Population", low = "blue", high = "red") + expand_limits(x= us$long, y=us$lat) + coord_map() + ggtitle("Population Map")
map.Population

##### Income per zip ######
zipIncome <- merge(statesExcluded, stateNames, by.x = "state", by.y = "state.abb")
zipIncome$state <- tolower(zipIncome$state.abb.1)
zipIncome$state
map.zipIncome <- ggplot(zipIncome, aes(map_id=state.abb)) + geom_map(map = us, fill="black", color="white") + geom_point(aes(x=longitude, y=latitude, color=median)) + expand_limits(x = us$long, y = us$lat) + coord_map() + ggtitle("Zip Income")
map.zipIncome

#### Zip Density ####
map.zip <- ggplot(zipAlldf, aes(map_id=state.name)) + geom_map(map = us, fill="black", color="blue") + stat_density2d(aes(x=longitude, y=latitude, color=stat(level))) + scale_color_gradient(low = "blue", high = "orange") + expand_limits(x = us$long, y = us$lat) + coord_map() + ggtitle("Density Map")
map.zip


##### Focus NY #####
zipNYdf <- zipAlldf[(zipAlldf$state == "NY"),]
xlim <- c(min(zipNYdf$longitude), max(zipNYdf$longitude))
ylim <- c(min(zipNYdf$latitude), max(zipNYdf$latitude))



