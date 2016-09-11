#libraries to include
library("ggplot2")
library("plyr")
library("MASS")

cleanupData <- function (df){
    # For future use for cleanup of the data if needed

} # end of cleanupData

addColumnVal <- function(df, newCol, origCol, key, val, ignore.case=FALSE){
    # Function addColumVal
    # Update value in certain rows of column newCol
    # when substring <key> is found in row i of column <origCol>
    # the value in row i for column <newCol> is set to <val>, if not that value remains as is.

    df[,newCol] <- ifelse(grepl(key, df[,origCol], ignore.case), val, df[,newCol]) 
    return(df) 
}

addDateColumns <- function(df){
    # Function addDateColumns
    # Add several date columns:
    # doy (day of the year)
    # dow (day of the week)
    # dom (day of the month)
    # nSinceStart (days since start (1-1-2006)
    # month
    # season
    # year

    # Add 'Datum' column for easy use
    df['Datum'] <- NA
    df$Datum <- df$PLEEG_DATUM

    # Add month column to data set for further analysis
    df['month'] <- NA
    keys <- c('-01-', '-02-','-03-', '-04-','-05-','-06-', '-07-', '-08-','-09-', '-10-','-11-','-12-')
    vals <- c(1,2,3,4,5,6,7,8,9,10,11,12)
    myMonths <- data.frame(keys, vals)
    for (i in seq(nrow(myMonths))){
        df <- addColumnVal(df, 'month', 'Datum', myMonths[i,1], myMonths[i,2])
    }

    # Add season column to data set for further analysis
    df['season'] <- NA
    keys <- c('-01-', '-02-','-03-', '-04-','-05-','-06-', '-07-', '-08-','-09-', '-10-','-11-','-12-')
    vals <- c(1,1,2,2,2,3,3,3,4,4,4,1)
    mySeasons <- data.frame(keys, vals)
    for (i in seq(nrow(mySeasons))){
        df <- addColumnVal(df, 'season', 'Datum', mySeasons[i,1], mySeasons[i,2])
    }
    
    # Add day column to data set for further analysis
    df['day'] <- NA
    df$day <- df$PLEEG_DATUM
    df$day <- gsub("^\\d{4}-","",df$day)

    # Add year column to data set for further analysis
    df['year'] <- NA
    df$year <- df$Datum
    df$year <- as.integer(gsub("-\\d{2}-\\d{2}","",df$year))

    # Add dom (day of the month column to data set for further analysis
    df['dom'] <- NA
    df$dom <- df$Datum
    df$dom <- as.integer(gsub("^\\d{4}-\\d{2}-","",df$dom))

    # Add DOY column to dataset for further analysis
    df['doy'] <- NA
#    # Is there a better way to do this?
    sumDaysPerMonth <- c(0,31,59, 90, 120, 151,181, 212, 243, 273, 304, 334 )
    sumDaysPerMonthLeap <- c(0,31,60, 91, 121, 152,182, 213, 244, 274, 305, 335 )
    df$doy <- ifelse(df$year%%4 == 0, sumDaysPerMonthLeap[df$month] + df$dom, sumDaysPerMonth[df$month] + df$dom)

    df['nSinceStart'] <- NA
#    # Is there a better way to do this?
    df$nSinceStart <- (df$year - 2006) * 365 +  df$doy
    df$nSinceStart <- ifelse(df$year > 2008, df$nSinceStart + 1, df$nSinceStart)
    df$nSinceStart <- ifelse(df$year > 2012, df$nSinceStart + 1, df$nSinceStart)

    # Note: nSinceStart = 1 is January 1 2006. This is a sunday.
    # So dow=1 (sun), dow=2 (mon), dow=3 (tues), dow=4 (wed), dow=5 (thu), dow=6 (fri), dow=0 (sat)
    df['dow'] <- NA
    df$dow <- df$nSinceStart %% 7

    return(df)

} 

addMainCatColumn <- function(df){
    # Function addMainCatColumn

    # Adding column mainCat to the dataframe for easy selection of crime categories
    df['mainCat'] <- 0 
    #1: moord en doodslag
    #2: verkrachting/ zedendelicten/zedenzaak/aanranding
    #3: Zware mishandeling
    #4: Geweld tegen.../Openlijke geweldpleging.../mishandeling
    #5: Huiselijk geweld
    #6: Straatroof/diefstal met geweld
    #7: Inbraak/Insluiping/Woninginbraak
    #8: Diefstal uit/Diefstal
    #9: Diefstal van/autodiefstal etc
    #10: Discriminatie/Vremdelingenhaat
    #11: harddrugs
    #12: softdrugs
    #13: Onder invloed...(?)
    #14: Fraude/...fraude
    #0: everything else
    #Note: order in list matters. For instance 'diefstal will also match 'diefstal met geweld' and other kinds. Need to set those catehories later than setting diefstal category.
    keys <- c('moord','verkrachting','aanranding','zeden','mishandeling','diefstal','zware mishandeling','geweld tegen','openlijke','huiselijk','straatroof','diefstal met geweld','inbraak ','insluiping','woninginbraak','diefstal van','diefstal overige','diefstal brom-','voertuigendiefstal','Vaartuigdiefstal','Autodiefstal','discriminatie','vreemdelingenhaat','onder invloed','softdrugs','wiet','hash','hennep','coffeeshop','harddrugs','ghb','hero','coca','synthetische drugs', 'drugsdelicten','drugsrunners','fraude')
    vals <- c(1,2,2,2,4,8,3,4,4,5,6,6,7,7,7,9,9,9,9,9,9,10,10,13,12,12,12,12,12,11,11,11,11,11,11,11,14)

    myCats <- data.frame(keys, vals)
    for (i in seq(nrow(myCats))){
        df <- addColumnVal(df, 'mainCat', 'OMSCHRIJVING_DELICT',  myCats[i,1], myCats[i,2] , ignore.case=TRUE)
    }
    # Tell dataframe that column mainCat is a factor
    df$mainCat <- as.factor(df$mainCat)

    # Give mainCat levels a name:
    levels(df$mainCat) <- c('overig', 'moord_doodslag', 'verkrachting', 'zware_mishandeling','simpel_geweld', 'huiselijk_geweld','straatroof', 'inbraak', 'diefstal', 'voertuigdiefstal', 'discriminatie','harddrugs','softdrugs','onder_de_invloed','fraude')

    return(df)
    
} 

setUpData <- function(myData){
    # Function setUpData
    # Add mainCat and several date columns to the dataset

    myData <- addMainCatColumn(myData)
    myData <- addDateColumns(myData)

# In the full dataset weather parameter names are: 'measure-'par'.x' for par values 
# on the same day as when the crime took place
# or 'measure-'par'day-'n, where n=1,2,3,4,5,6 or 7 for values on days before the crime took place
# or 'measure-'par'day+1' for value on the day after the crime took place.
# Note that R changes '-' in the column name to '.' when reading in the csv file.!!!!

# replace 'measure.' in measure.windDirection.x etc by ''
    names(myData) <- gsub('measure.','',names(myData))
# replace '.x' in windDirection.x etc by ''
    names(myData) <- gsub('.x','', names(myData))

    return(myData)
}

readInData <- function(dataDir, dataName){
    # Function readInData
    #Read in the data set from file given directory 
    #dataDir and file name dataName
    dataFile <- paste(dataDir,"/",dataName, sep="")
    df = read.csv(dataFile)

    # Big dataset. It also takes about 20 minutes to read in. And a while to the analysis 
    # whith such a big dataset. 
    # For the moment we are not going to need the values of the 7 days before and day after the crime.
    # Remove the columns like windrichting_day-n and windrichting_day+1 from the dataset
    # Should better be skipped when reading in, but I do not know how to do that.
    myData <- df[,!grepl("_day",names(df))] 
    # Make sure the big dataset is out of memory (is this needed?)
    df <- NULL
    return(myData)
}

startUp <- function(){

    # read in the data
    dataDir <- "~/Projects/datalab/CrimeWeather/data"
    dataName <- "fullTestAvgTemperature.csv"
#   Note: it takes about 20 min to readin the fullWODC dataset. And slows down my workstation for quit a while
#   while reading 
#    dataName <- "fullWODCDataSetMeteo.csv"

    myData <- readInData(dataDir, dataName)
    names(myData)

    # add mainCat and date columns to the dataset 
    myData <- setUpData(myData)

    summary(myData)

    # List of weather parameters that we want to investigate
#    weatherPars <- c('averageTemperature', 'minTemperature', 'maxTemperature', 'precipitationAmount', 'windDirection','radiationAmount', 'sunshineDuration','relativeHumidity')
    weatherPars <- c('averageTemperature')

    otherDeps <- c('year', 'season', 'month','dom','dow','doy','nSinceStart')

    # define datalab directory 
    datalabDir <- "~/Projects/datalab/CrimeWeather/output"
    # create it if it does not exist. Gives warning message if already exists
    # That is the reason that shwWarnings is set to FALSE
    dir.create(file.path(datalabDir), showWarnings=FALSE)

    # Return the dataset, datalab directory, list of weather parameters and list of other paramters
    return(list(myData, datalabDir, weatherPars, otherDeps))

} # end of startUp function
