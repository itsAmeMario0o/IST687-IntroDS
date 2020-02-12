## Homework 3

## STEP 1
## Creating readStates function

## Define function name and input

readStates <- function(inputFile)
{
    ## Import the file using read.csv and save as FileDataFrame
    FileDataFrame <- read.csv(url(inputFile))
    
    ## Return FileDataFrame
    return(FileDataFrame)
}

## Import Census data set from URL using readStates and save as CensusFrame
CensusFrame <- readStates('http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv')


## STEP 2

## Removing extra rows at the top of the file and saving back to the file

CensusFrame <- CensusFrame[-1:-8,]

## Last five variables are trash so remove them too and save back onto orginal frame

CensusFrame <- CensusFrame[,1:5]

## Remove last 5 rows also

CensusFrame <- CensusFrame[-52:-58,]

## Save the desired column names to update the data frame
cnames <- c('stateName','base2010','base2011','Jul2010','Jul2011')

## Rename the columns using the saved names
colnames(CensusFrame) <- cnames

## Verify there is only 5 columns with desired names
colnames(CensusFrame)

## Remove extra dots from the beginning of the row names

testFrame <- CensusFrame

CensusFrame$stateName <- gsub("\\.",'', CensusFrame$stateName)

## getting rid of the commas

CensusFrame$base2010 <- gsub(",",'', CensusFrame$base2010)
CensusFrame$base2011 <- gsub(",",'', CensusFrame$base2011)
CensusFrame$Jul2010 <- gsub(",",'', CensusFrame$Jul2010)
CensusFrame$Jul2011 <- gsub(",",'', CensusFrame$Jul2011)

## Converting to integers
CensusFrame$base2010 <- as.numeric(gsub(" ",'', CensusFrame$base2010))
CensusFrame$base2011 <- as.numeric(gsub(" ",'', CensusFrame$base2011))
CensusFrame$Jul2010 <- as.numeric(gsub(" ",'', CensusFrame$Jul2010))
CensusFrame$Jul2011 <- as.numeric(gsub(" ",'', CensusFrame$Jul2011))

## Confirming type
str(CensusFrame)

## Re-ordering row indexes
rownames(CensusFrame) <- NULL

## STEP 3
## Saving as a dataframe

dfStates = data.frame(CensusFrame)

## Taking the mean of Jul2010
mean(dfStates$Jul2011)

##STEP 4
## Returning the state with the highest population

dfStates[which.max(dfStates$Jul2011),1]

## Sorting the States in increasing order

sortedStates = dfStates[order(dfStates$Jul2011),]

sortedStates

## STEP 5 
## Writing a function that returns percentage of values below a given value

## Intialize a function that accepts a vector of values and a number n
cuPer <- function( distrubtion, n){
    ## Initalize a count variable
    x = 0
    
    #find the total length of the vector
    tot = length(distrubtion)
    
    ## Iteriate through the values of population
    for(pop in distrubtion){
        ## If the pop value is less than the input value n, increase the count by 1
        if (pop < n){
            x = x + 1
        }
    }
    ## Divide the count of values before n by the total length of the vector
    percent = x / tot
    
    ## Return the percent of values in the vector below the input n
    return(percent)
}

## Save a variable as the mean of Jul2011
meanJul2011 = mean(dfStates$Jul2011)

##
cuPer(dfStates$Jul2011, meanJul2011 )
