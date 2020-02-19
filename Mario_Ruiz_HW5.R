## Step 1- Load dataset
install.packages("jsonlite")
install.packages("RCurl")
library("jsonlite")
library("RCurl")

myData <- "https://opendata.maryland.gov/resource/pdvh-tf2u.json" 
restResult <- getURL(myData)
results <- fromJSON(restResult)

# Sanity check to make sure data is pulled and in results
results

## Step 2- Data prep
theseCols <- c("CASE_NUMBER","BARRACK", "ACC_DATE", "ACC_TIME", "ACC_TIME_CODE", "DAY_OF_WEEK", "ROAD",
               "INTERSECT_ROAD", "DIST_FROM_INTERSECT", "DIST_DIRECTION", "CITY_NAME", "COUNTY_CODE",
               "COUNTY_NAME", "VEHICLE_COUNT", "PROP_DEST", "INJURY", "COLLICSION_WITH_1", "COLLISION_WITH_2")

colnames(results) <- theseCols
## I should remap results to another vector so I can modify the new vector while maintaining the previous
results$DAY_OF_WEEK <- gsub(" ","", results$DAY_OF_WEEK)
str(results)
# Step 3 - Using SQL
install.packages("sqldf")
library("sqldf")
nrow(sqldf('SELECT CASE_NUMBER FROM results WHERE INJURY="YES"'))
sqldf('SELECT DAY_OF_WEEK, COUNT(DAY_OF_WEEK) as INJURIES FROM results WHERE INJURY="YES"
      GROUP BY DAY_OF_WEEK ORDER BY INJURIES desc')
## Step 4 - Using tapply
tapply(results$DAY_OF_WEEK == "SUNDAY", results$DAY_OF_WEEK, sum)
tapply(results$INJURY == "YES", results$DAY_OF_WEEK, sum)
injuryPerDay <- tapply(results$INJURY == "YES", results$DAY_OF_WEEK, sum)
injuryPerDay
