source("~/Projects/datalab/CrimeWeather/Tools/Rscript/fitting.R")
source("~/Projects/datalab/CrimeWeather/Tools/Rscript/plotting.R")

cleanupDF <- function(df, X, XMin){
    # Remove unwanted values for column X
    # For example: fail values for T: -99
    L = df[X] > XMin 
    dfOK <- df[L,]
    return(dfOK)
}

splitDataset <- function(df, splitCol){
    extraString <- list() 
    df[splitCol] <- as.factor(df[[splitCol]])
    splitList <- split(df, df[splitCol])
#    splitList <- append(splitList, splitMonthList)
    for (df in splitList){
        extraString <- append(extraString, paste("_",splitCol,"-",df[1,splitCol],sep=""))
    }
    return(list(splitList,extraString))
}

crimeVsX <- function(df, X, XMin, catVal, extString, baseDir){
    # Function crimeVsX
    # Create plot of number of crimes VS X
    # <extString> is an extra identifying string and can be used to indicate for which factor and which level 
    # the plot is produced

    # cleanup the dataset
    #dfOK <- cleanupDF(df, X, XMin)
    dfOK <- df

    nCrimes <- count(dfOK, X)
    yLab<-"number of crimes"
    xLab<-X
    mainS <-  paste("number of crimes vs",X)
    subS <- paste("Crime category:",catVal, extString)
#    plotName <- paste(baseDir,"/crimes_VS_",X,"_for_crimeCat_",catVal,extString, sep="")
#    #jpeg(file = paste(plotName,".jpg",sep=""))
#    pdf(file = paste(plotName,".pdf",sep=""))
    plotYVsX(nCrimes, doXOnly=TRUE, xLab=xLab, yLab=yLab, mainS=mainS, subS=subS)
#    dev.off()
} # end of crimeVSX

createPredDf <- function(fitResult,X, xMin, xMax, xStep){
    # Use the fitresult to create a prediction dataset with <XPred> and <YPred>

    XPred <- seq(xMin, xMax, by = xStep)
    new.data <- data.frame(XPred)
    colnames(new.data)[1] <- X

    # Create YPred based using fit result and XPred as input
    YPred <- predict(fitResult, newdata = new.data, type = "response")

    # Create prediction dataset
    predictionDF <- data.frame(XPred, YPred)

    return(predictionDF)
}

produceDFCrimesperday <- function(df, X){
    # Produce a dataset with 'number of crimes per day (freq) and X

    # Count number of entries (crimes) for a certain date. The result is a 
    # dataframe with columns 'freq' and 'Datum;
    numperday = count(df,'Datum')
    # The large data set <df> includs all Dutch cities. 
    # On one day crimes maybe committed in more than one city.
    # Each city has it's own X value. So serveral X values per day are possible.
    # Take the mean of X on a certain day
    formule <- as.formula(paste(X,"~ Datum"))
    meanXperday <- aggregate(formule, df, mean)

    # Create the total dataset by merging the <numperday> and the <meanXperday> datasets.
    # columns: 'Datum', 'freq',X
    total <- merge(numperday,meanXperday,by='Datum')
    return(total)
}


numberCrimesPerDayDFAndFit <- function(df, X){
    # Getting number of crimes per day VS <X>
    # Only produce plots the trueData
    print("")
    print("")
    print("total datastes!")

    total <- produceDFCrimesperday(df, X)
    # Perform lin fit
    significance10 <- performLMFit(total,X,'freq', "lin")
    fit10 <- significance10[[1]]
    sfit10 <- significance10[[2]]

    # Get xMin, xMax and xStep from dataset itself
    xMin <- round(floor(min(total[X])),digits=-1)
    xMax <- round(ceiling(max(total[X])),digits=-1)
    range <- xMax - xMin
    nBins <- 20
    xStep <- range/nBins

    predDF10 <- createPredDf(fit10,X, xMin, xMax, xStep)

#    Also try to fit a second order polynomial model:
    significance20 <- performLMFit(total,X,'freq', "poly2")
    # Create Prediction dataset
    fit20 <- significance20[[1]]
    sfit20 <- significance20[[2]]
    predDF20 <- createPredDf(fit20,X, xMin, xMax, xStep)

#    Also try to fit a third order polynomial model:
    significance30 <- performLMFit(total,X,'freq', "poly3")
    fit30 <- significance30[[1]]
    sfit30 <- significance30[[2]]
    predDF30 <- createPredDf(fit30,X, xMin, xMax, xStep)

    kindsList <- list("lin", "poly2", "poly3")
    significanceList <- list(significance10, significance20, significance30)
    predictionList <- list(predDF10, predDF20, predDF30)
    
    print("")
    print("")
    return(list(total, kindsList, significanceList, predictionList))
}

numberCrimesPerDayVsX <- function(df, X, baseDir, catVal, extraString, isTrueData){
    # Get dataset with number of crimes per day VS X
    # And fit lin fit, poly2 fit and poly3 fit
    values <- numberCrimesPerDayDFAndFit(df, X)
    total <- values[[1]]
    kindsList <- values[[2]]
    significanceList <- values[[3]]
    predictionDFList <- values[[4]]

    if (isTRUE(isTrueData)){
        # Only produce a plot when it is the true dataset and not a sample dataset

        # set plot labels and plotName fot the number of crimes per day plots
        yLab<-"number of crimes per day"
        xLab<-X
        mainS = paste("number of crimes per day vs",X)

        # Loop over the different fit values
        for (i in 1:length(kindsList)){
            # kind of fit preformed:
            kind <- kindsList[i] 
            subS = paste("Crime category:",catVal, extraString, " and fit kind: ", kind," fit")
            # prediction dataset for plotting fit:
            predDF <- predictionDFList[[i]]
            # fit parameters:
            significance <- significanceList[[i]]
            fit <- significance[[1]]

            oldP <- par(mfrow=c(2,2))
            plotName = paste(baseDir, "/crimesperday_VS_",X,"_and_resisuals_for_crimeCat_",catVal,extraString,"_",kind,"fit", sep="")
#            jpeg(file = paste(plotName,".jpg",sep=""))
            pdf(file = paste(plotName,".pdf",sep=""))

            # Plot crimes per day VS X
            plotYVsX(total[[X]], Y=total[['freq']], xLab=xLab, yLab=yLab, mainS=mainS, subS=subS, plotFit=TRUE, fit=fit, fitKind=kind, predX=predDF[['XPred']],predY=predDF[['YPred']])

            # I do not quit understand what I am seeing in the residual plots for crimes per day VS X
            # residuals  Vs fitted values
            subS = paste("Crime category:",catVal, extraString, "and fit kind:  ",kind,"fit")
            plotResidualsVsFitted(fit, subS)
            # residuals
            subS = paste("Crime category:",catVal, extraString,"and fit kind:  ",kind,"fit")
            plotResiduals(fit, subS)

            par(oldP)
            dev.off()
        }
    }
    return(values)
}

getMeanSdSlice <- function(df, binCol, binVal, y, x){
    # Obtain a slice of the data <df> for which entries in column <binCol>  have value <binVal>
    # In the returned dataset 'x' is the mean value of <x> in the bin,  and 'y' is the lambda value
    # of a poisson fit to the <y> distribution of the fit. Also the two standard deviations are part
    # of the returned dataset

    subDf<-subset(df,df[[binCol]] == binVal)
    num <- nrow(subDf)

    # get the mean and the standard deviation for the slice for column X 
    meanX <- mean(subDf[[x]]) 
    sdX <- sd(subDf[[x]]) 
    # get lambda and standard deviation using poisson fit for Y:
    lambdaY <- 0
    sdLambdaY <- 0
    if(num > 0){ 
      mle <- fitdistr(subDf[[y]], densfun="poisson")
      lambdaY <- mle$estimate
      sdLambdaY <- mle$sd
    }

    meanSd <- list(lambdaY,sdLambdaY, meanX, sdX) 
    return(meanSd)
}

getMeanSd <- function(df,binCol,valCol){
    # Obtain a binned dataset. For each bin one 'x' value (meanX) and 
    # one 'y' value (lambdaY) is obtained. Also the sdX and sdLambdaY
    # will be part of the returned dataset.

    # Get dataframe with only unique entries for column <binCol>
    cDF <- df[!duplicated(df[binCol]),]
    # number of unique entries
    n <- nrow(cDF)

    #initialize vectors lambdaY, sdLambdaY, meanX and sdX
    lambdaY <- vector(length=n)
    sdLambdaY <- vector(length=n)
    meanX <- vector(length=n)
    sdX <- vector(length=n)
   
    # Loop over bins and get for each bin one 'x' value (meanX) and one 'y' value (lambdaY)
    # Also the standard deviation for 'x' and 'y' are obtained. 
    for (i in 1:n){
        # bin i with bin value binVal
        binVal <- cDF[i, binCol]
        # obtain dataset for this bin
        meanSd <- getMeanSdSlice(df, binCol, binVal, "freq", valCol)
        # Fill entrie in the vectors
        lambdaY[i] <- meanSd[[1]]
        sdLambdaY[i] <- meanSd[[2]]
        meanX[i] <- meanSd[[3]]
        sdX[i] <- meanSd[[4]]
    }
    # binned dataset
    meanSd <- data.frame(lambdaY, sdLambdaY, meanX, sdX)
    return(meanSd)
}


# TODO: what to do about binning? Allow for varying bins? How?
produceDFCrimesperdaySliced <- function(df, X, nBreaks=29, labels = 0:28){
    # Produce a sliced dataset

    # Add a bin column to dataset df
#    df$bin <- cut(df[[X]], breaks = nBreaks, labels = labels)
    # Using variable binning. TODO: How to handily do this ? Also for other weather parameters?
    myBreaks <- c(-Inf,-6,-2,0,2,4,6,8,10,12,14,16,18,20,24,Inf)
    df$bin <- cut(df[[X]], breaks = myBreaks)

    # Use the bin column to obtain the sliced dataset
    meanSd <- getMeanSd(df, 'bin', X)
    return(meanSd)
}


numberCrimesPerDaySlicedDfAndFit <- function(total, X){

    print("")
    print("")
    print("Sliced datastes!")
    sliced <- produceDFCrimesperdaySliced(total, X, nBreaks=29, labels = 0:28)
    #print(summary(sliced))
    significance1 <- performLMFit(sliced,'meanX','lambdaY', "lin")
    fit1 <- significance1[[1]]
    sfit1 <- significance1[[2]]

    # Get xMin, xMax and xStep from dataset itself
    xMin <- round(floor(min(sliced['meanX'])),digits=-1)
    xMax <- round(ceiling(max(sliced['meanX'])),digits=-1)
    range <- xMax - xMin
    nBins <- 20
    xStep <- range/nBins
    predDF1 <- createPredDf(fit1,'meanX', xMin, xMax, xStep)

#    Also try to fit a second order polynomial model:
    significance2 <- performLMFit(sliced,'meanX','lambdaY', "poly2")
    # Create Prediction dataset
    fit2 <- significance2[[1]]
    sfit2 <- significance2[[2]]
    predDF2 <- createPredDf(fit2,'meanX', xMin, xMax, xStep)

#    Also try to fit a third order polynomial model:
    significance3 <- performLMFit(sliced,'meanX','lambdaY', "poly3")
    fit3 <- significance3[[1]]
    sfit3 <- significance3[[2]]
    predDF3 <- createPredDf(fit3,'meanX', xMin, xMax, xStep)

    kindsList <- list("lin", "poly2", "poly3")
    significanceList <- list(significance1, significance2, significance3)
    predictionList <- list(predDF1, predDF2, predDF3)
    
    print("")
    print("")

    return(list(sliced, kindsList, significanceList, predictionList))
}

numberCrimesPerDayVsXSliced <- function(total, X, baseDir, catVal,extraString, isTrueData){

    # Get dataset with number of crimes per day VS X
    # And fit lin fit, poly2 fit and poly3 fit
    values <- numberCrimesPerDaySlicedDfAndFit(total, X)
    sliced <- values[[1]]
    kindsList <- values[[2]]
    significanceList <- values[[3]]
    predictionDFList <- values[[4]]

    if (isTRUE(isTrueData)){
        # Only produce a plot when it is the true dataset and not a sample dataset

        # set plot labels and plotName fot the number of crimes per day plots
        yLab<-"number of crimes per day"
        xLab<-X
        mainS = paste("number of crimes per day vs",X)
#        subS = paste("Crime category:",catVal,extraString)
        arrowX <- sliced[['meanX']]
        print(arrowX)
        arrowY0 <- sliced[['lambdaY']]-sliced[['sdLambdaY']]
        arrowY1 <- sliced[['lambdaY']]+sliced[['sdLambdaY']]

        # Loop over the different fit values
        for (i in 1:length(kindsList)){
            # kind of fit preformed:
            kind <- kindsList[i] 
            subS = paste("Crime category:",catVal, extraString, " and fit kind: ", kind," fit")
            # prediction dataset for plotting fit:
            predDF <- predictionDFList[[i]]
            # fit parameters:
            significance <- significanceList[[i]]
            fit <- significance[[1]]

            # Overal plot name (fit plot and two residual plots on one page)
            oldP <- par(mfrow=c(2,2))
            plotName = paste(baseDir, "/crimesperday_VS_",X,"Sliced_and_resisuals_for_crimeCat_",catVal,extraString,"_",kind,"fit", sep="")
#            jpeg(file = paste(plotName,".jpg",sep=""))
            pdf(file = paste(plotName,".pdf",sep=""))

            # Plot crimes per day VS X sliced
            plotYVsX(sliced[['meanX']], Y=sliced[['lambdaY']], xLab=xLab, yLab=yLab, mainS=mainS, subS=subS, plotFit=TRUE, fit=fit, fitKind = kind, predX=predDF[['XPred']], predY=predDF[['YPred']], plotError=TRUE, arrowX=arrowX, arrowY0=arrowY0, arrowY1=arrowY1)

            # I do not quit understand what I am seeing in the residual plots for crimes per day VS X
            # residuals  Vs fitted values
            subS = paste("Crime category:",catVal,extraString, "and fit kind:  ",kind,"fit")
            plotResidualsVsFitted(fit, subS)
            # residuals
            subS = paste("Crime category:",catVal,extraString, "and fit kind:  ",kind,"fit")
            plotResiduals(fit, subS)

            par(oldP)
            dev.off()
        }
    }
    return(values)
}


otherDeps <- function(df, baseDir){
    # Function otherDeps
    # It runs crimeVsX function to see if there are other dependencies 
    # than the weatherPar under consideration
    # other paramteters under consieration:
    # doy, dow, nSinceStart, dom, month, season or year

    otherDepsDir <- paste(baseDir,'/otherDeps',sep="")
    dir.create(file.path(otherDepsDir), showWarnings = FALSE)
    catVal <- df[1,'mainCat']

    XMin = -99
    # Loop over all date parameters and make sure that the plots appear in one file
    extString <- ""
    plotName <- paste(otherDepsDir,"/crimes_VS_different_pars_for_crimeCat_",catVal,extString, sep="")
    #jpeg(file = paste(plotName,".jpg",sep=""))
    pdf(file = paste(plotName,".pdf",sep=""))
    # 4 plots per page:
    oldP <- par(mfrow=c(2,2))
    print("doy:")
    crimeVsX(df, 'doy', XMin, catVal, "_doy", otherDepsDir)
    # Day of the week
    print("dow:")
    crimeVsX(df, 'dow', XMin, catVal, "_dow", otherDepsDir)
    # Day since start (1-1-2006)
    print("nSinceStart:")
    crimeVsX(df, 'nSinceStart', XMin, catVal, "_nSinceStart", otherDepsDir)
    # Day of the month
    print("dom:")
    crimeVsX(df, 'dom', XMin, catVal, "_dom", otherDepsDir)
    # month
    print("month:")
    crimeVsX(df, 'month', XMin, catVal, "_month", otherDepsDir)
    # Not sure how usefull season as X will be. Only 4 points
    # season
    print("season:")
    crimeVsX(df, 'season', XMin, catVal, "_season", otherDepsDir)
    # year
    print("year:")
    crimeVsX(df, 'year', XMin, catVal, "_year", otherDepsDir)

    par(oldP)
    dev.off()

} # end of otherDeps

selectCity <- function(df, city){
    # Function selectCity
    # Create a subset of <df> for 'PLEEG_PLAATS' = city

    L = df['PLEEG_PLAATS'] == city 
    dfOK <- df[L,]
    return(dfOK)
} # end of selectCity

selectYear <- function(df, year){
    # Function selectYear
    # Creates a subset of <df> for entries of 'PLEEG_DATUM' that contain <year>

    dfOK <- df[grep(year, df$PLEEG_DATUM), ]
    return(dfOK)
} # end of selectYear

selectLevel <- function(df, splitCol){
    # Function selectLevel
    # Split dataset <df> into subsets for each level of splitCol

    splitDfList <- split(df, df[splitCol])
    return(splitDfList)
} # end of selectLevel

yearTest2 <- function(df, baseDir, weatherPar){
    # Function yearTest2
    # Makes a subset for each year in the dataset <df>
    # And run crimeVsX function to see if dual mode depends on particular years

    catVal <- df[1,'mainCat']

    # define the years directory as a sub directory in the catVal directory
    yearDir <- paste(baseDir,'/years',sep="")
    # create it if it doesn't already exist
    dir.create(file.path(yearDir), showWarnings = FALSE)

    # list of year datasets
    yearList <- selectLevel(df, 'year')
    # Loop over all years in the list and make sure that the plots appear in one file
    extString <- "_for_different_years"
    plotName <- paste(yearDir,"/crimes_VS_",weatherPar,"_for_crimeCat_",catVal,extString, sep="")
    #jpeg(file = paste(plotName,".jpg",sep=""))
    pdf(file = paste(plotName,".pdf",sep=""))
    # 4 plots per page:
    oldP <- par(mfrow=c(2,2))
    for (yearDF in yearList){
        year <- yearDF[1,'year']
        print(paste("Processing year: ", year,sep=""))
        XMin = -99
        # A string that gives extra information for in the filename of 
        #the plot that is produced in function test:
        extString <- paste("_year-",year,sep="")
        crimeVsX(yearDF, weatherPar, XMin, catVal, extString, yearDir)
    }          
    par(oldP)
    dev.off()
} # end of yearTest2

yearTest <- function(df, baseDir, weatherPar){
    # Function yearTest
    # Makes a subset for each year in the list
    # And run crimeVsX function to see if dual mode depends on particular years

    catVal <- df[1,'mainCat']
    #Create a list of years
    years <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
    # define the years directory as a sub directory in the catVal directory
    yearDir <- paste(baseDir,'/years',sep="")
    # create it if it doesn't already exist
    dir.create(file.path(yearDir), showWarnings = FALSE)

    # Loop over all years in the list and make sure that the plots appear in one file
    extString <- "_for_different_years"
    plotName <- paste(yearDir,"/crimes_VS_",weatherPar,"_for_crimeCat_",catVal,extString, sep="")
    #jpeg(file = paste(plotName,".jpg",sep=""))
    pdf(file = paste(plotName,".pdf",sep=""))
    # 4 plots per page:
    oldP <- par(mfrow=c(2,2))
    # Loop over the years
    for (year in years){
        print(paste("Processing year: ", year,sep=""))
        # Create subset for year <year>
        yearDF <- selectYear(df, year)
        XMin = -99
        # A string that gives extra information for in the filename of 
        #the plot that is produced in function test:
        extString <- paste("_year-",year,sep="")
        crimeVsX(yearDF, weatherPar, XMin, catVal, extString, yearDir)
    }          
    par(oldP)
    dev.off()
} # End of yearTest

cityTest <- function(df, baseDir, weatherPar){
    # Function cityTest
    # Makes a subset for each city in the list
    # And run crimeVsX function to see if dual mode depends on particular cities

    catVal <- df[1,'mainCat']
    #Create a list of cities
    cities <- c("Amsterdam", "Rotterdam", "Diemen", "Utrecht", "Groningen", "Maastricht")
    # define the cities directory as a sub directory in the catVal directory
    cityDir <- paste(baseDir,'/cities',sep="")
    # create it if it doesn't already exist
    dir.create(file.path(cityDir), showWarnings = FALSE)

    # Loop over all cities in the list and make sure that the plots appear in one file
    extString <- "_for_different_cities"
    plotName <- paste(cityDir,"/crimes_VS_",weatherPar,"_for_crimeCat_",catVal,extString, sep="")
    #jpeg(file = paste(plotName,".jpg",sep=""))
    pdf(file = paste(plotName,".pdf",sep=""))
    # 4 plots per page:
    oldP <- par(mfrow=c(2,2))
    # Loop over the cities
    for (city in cities){
        XMin = -99
        print(paste("Processing city: ", city,sep=""))
        # A string that gives extra information for in the filename of 
        #the plot that is produced in function test:
        extString <- paste("_city-",city,sep="")

        # Create subset for city <city>
        cityDF <- selectCity(df, city)
        # It makes only sense to make the plot if there are actually entries in the datset cityDF
        if (nrow(cityDF) > 0) {
            crimeVsX(cityDF, weatherPar, XMin, catVal, extString, cityDir)
        } else {
            print(paste("In cityTest: For crime category: ", catVal, " and city: ", city, " there are not enough entries to produce the plot"))
        }
    }          
    par(oldP)
    dev.off()
} # end of cityTest

test1 <- function(splitList, extraStrings, weatherPar, catVal, FLDir){
    # Function test1
    # Plot Number of crimes VS X for all datasets in the list.
    # Plot them all in the same file for better comparison.
    # If splitAll or splitXXX are not set to TRUE, then the list only 
    # contains the unsplit dataset. So only one plot appears in the file

    # define the test1 directory as a sub directory in the FL directory
    test1Dir <- paste(FLDir,'/test1',sep="")
    # create it if it doesn't already exist
    dir.create(file.path(test1Dir), showWarnings = FALSE)

    # Loop over (split) datasets in the splitList and make sure that the plots appear in one file
    extString <- "_for_different_datasets"
    plotName <- paste(test1Dir,"/crimes_VS_",weatherPar,"_for_crimeCat_",catVal,extString, sep="")
    #jpeg(file = paste(plotName,".jpg",sep=""))
    pdf(file = paste(plotName,".pdf",sep=""))
    # 4 plots per page:
    oldP <- par(mfrow=c(2,2))
    for (i in 1:length(splitList)){
        extraString <- extraStrings[[i]]
        myDF <- splitList[[i]]
        # Only usefull to continue if enough entries in dataset <myDF>
        if(nrow(myDF)> 10){
       
            # TODO: check if also true for other weather parameters
            XMin = -99
            # N crimes VS weatherPar
            crimeVsX(myDF, weatherPar, XMin, catVal, extraString, test1Dir)
        } else {
            print(paste("Not enough entries in dataset for weatherPar: ", weatherPar, " cat: ", catVal, "extraString: ", extraString))
        } # end of check nrows >10
    }   # end of loop over splitList
    par(oldP)
    dev.off()
} # end function test1 

runTests <- function(myData, datalabDir, weatherPars, doYearTest=FALSE, doCityTest=FALSE, checkOtherDeps=FALSE, splitAll=FALSE, splitYear=FALSE, splitMonth=FALSE, splitSeason=FALSE, splitDow=FALSE, splitDom=FALSE, doFirstLook=FALSE ) {

    # Loop over different weather parameters
    for (weatherPar in weatherPars){
        # For output to logfile/screen to see the progress
        print(paste("Looping over weather parameter",weatherPar))
       
        # define the weatherPar directory  as sub directory of the datalab directory
        baseDir <- paste(datalabDir,"/",weatherPar,sep="")
        # create it if it doesn't already exist
        dir.create(file.path(baseDir), showWarnings = FALSE)
       
        #Loop over different crime categories
        # Split the dataset based on the different levels of factor 'mainCat'
        # I produces a list of datasets. One dataset for each mainCat level
        splitDfList <- split(myData, myData$mainCat)
        # Loop over different crime categories
        for (catDf in splitDfList){
            # Get the value of mainCat dataset under consideration

            catVal <- catDf[1,'mainCat']

            # If requested also split dataset based on year, month, doy, dom or dow
            splitList <- list(catDf)
            extraStrings <- list("") 

            if (isTRUE(splitAll) | isTRUE(splitMonth)){
                returnList <- splitDataset(catDf, 'month')
                splitList <- append(splitList, returnList[[1]])
                extraStrings <- append(extraStrings, returnList[[2]])

            } 
            if (isTRUE(splitAll) | isTRUE(splitYear)){
                returnList <- splitDataset(catDf, 'year')
                splitList <- append(splitList, returnList[[1]])
                extraStrings <- append(extraStrings, returnList[[2]])
            } 
            if (isTRUE(splitAll) | isTRUE(splitSeason)){
                returnList <- splitDataset(catDf, 'season')
                splitList <- append(splitList, returnList[[1]])
                extraStrings <- append(extraStrings, returnList[[2]])
            } 
            if (isTRUE(splitAll) | isTRUE(splitDom)){
                returnList <- splitDataset(catDf, 'dom')
                splitList <- append(splitList, returnList[[1]])
                extraStrings <- append(extraStrings, returnList[[2]])
            } 
            if (isTRUE(splitAll) | isTRUE(splitDow)){
                returnList <- splitDataset(catDf, 'dow')
                splitList <- append(splitList, returnList[[1]])
                extraStrings <- append(extraStrings, returnList[[2]])
            }

            # define the catVal directory as a sub directory in the weatherPar directory
            # This serves a base directory for different output files
            baseDir <- paste(datalabDir,"/",weatherPar,"/",catVal,sep="")
            # create it if it doesn't already exist
            dir.create(file.path(baseDir), showWarnings=FALSE)
            print(paste("Looping over crime category: ", catVal))

            if(isTRUE(doCityTest)){
                # Loop over list of cities to create a subset for a particular city
                cityTest(catDf, baseDir, weatherPar)
            }          
            if(isTRUE(doYearTest)){
                # Loop over list of years to create a subset for a particular year
                #yearTest(catDf, baseDir, weatherPar)
                # split dataset <df> into subsets. One for each level of column <year>
                yearTest2(catDf, baseDir, weatherPar)
            }          

            if(isTRUE(checkOtherDeps)){
                otherDeps(catDf, baseDir)
            }

            if(isTRUE(doFirstLook)){
                # Have a first look at the data
                # define the FirstLook directory as a sub directory in the base directory
                FLDir <- paste(baseDir,'/FirstLook',sep="")
                # create it if it doesn't already exist
                dir.create(file.path(FLDir), showWarnings = FALSE)
  
                # run test1 to plot number of crimes VS weatherPar. One plot in the file case dataset is not split up.
                # Several plots (4 per page) if dataset is split up. 
                test1(splitList, extraStrings, weatherPar, catVal, FLDir)

                # Usually just catDf, unless splitYear, splitMonth, spltSeason, splitDom, splitDow, or splotAll is set
                # than splitList includes catDf and list of year/month/season/dom/dow splitup datasets 
                for (i in 1:length(splitList)){
                    extraString <- extraStrings[[i]]
                    myDF <- splitList[[i]]
                    # Only usefull to continue if enough entries in dataset <myDF>
                    if(nrow(myDF)> 10){
                        # Define file name for the logfile. Logfile will be used to write output to 
                        # ( for example fit results and error/warning messages)
                        logFileName <- paste(FLDir,"/firstLook_Par",weatherPar,"_Cat",catVal,extraString,".log",sep="")
        
                        # Uncomment following line if output is written to logfile
                        sink(logFileName, type=c("output", "message"))
        
                        # N crimes per day VS weatherPar
                        #dfOK <- cleanupDF(myDF, weatherPar, XMin)
                        dfOK <- myDF
                        values0 <- numberCrimesPerDayVsX(dfOK, weatherPar, FLDir, catVal, extraString, TRUE)
                        total <- values0[[1]]
                        significanceList0 <- values0[[3]]
                        summary(total)
                        # N crimes per day VS weatherPar sliced
                        values1 <- numberCrimesPerDayVsXSliced(total, weatherPar, FLDir, catVal, extraString, TRUE)
                        significanceList1 <- values1[[3]]
        
                        # Uncomment  following line to switch off writing to logfile
                        sink()
                    } else {
                        print(paste("Not enough entries in dataset for weatherPar: ", weatherPar, " cat: ", catVal, "extraString: ", extraString))
                    } # end check enough entries in <myDF>
                } # end loop over splitList
            } # end isTRUE(firstLook)

#            # Perform Poisson regression
#            if(isTRUE(maxFit)){
#                # Perform Poisson regression
#
#                # define the maxFit directory as a sub directory in the base directory
#                maxFitDir <- paste(baseDir,'/maxFit',sep="")
#                # create it if it doesn't already exist
#                dir.create(file.path(maxFitDir), showWarnings = FALSE)
#
#                XMin = -99
#                dfOK <- cleanupDF(catDf, weatherPar, XMin)
#                dfOK <- catDf
#                # Create dataset with number of crimes per day ('freq'), 'Datum' and X columns
#                total <- produceDFCrimesperday(dfOK, weatherPar)
#                summary(total)
#                # replace NA by 0. Is this needed?
#                total[is.na(total)] <- 0
#
#                # TODO: At the moment we only fit a lineait fit. Should we also implement polynomial fit
#                # of second and third degree??????
#                # Uncomment following line if output is written to logfile
#                logFileName <- paste(maxFitDir,"/maxFit_Par",weatherPar,"_Cat",catVal,".log",sep="")
#                #sink(logFileName, type=c("output", "message"))
#                fit <- performMaxLFit(total,weatherPar)
#
#                # TODO: get xMin, xMax and xStep from dfOK???????
#                xMin = -20
#                xMax = 30
#                xStep = 0.5
#                # Create Prediction dataset
#                predDF <- createPredDf(fit,weatherPar, xMin, xMax, xStep)
#
#                plotName <- paste(maxFitDir,"/MLFitresult_catVal_",catVal,".jpg", sep="")
#                jpeg(file = plotName)
#                xLab <- weatherPar
#                yLab <- "number of crimes per day"
#                plotCol <- "lightgrey"
#                plot(total[[weatherPar]], total[['freq']], cex = 1, col = plotCol, pch = 19, ylab = yLab, xlab = xLab)
#                # overlay fit using the prediction dataset
#                fitCol <- "green"
#                lines(predDF[['XPred']], predDF[['YPred']], col = fitCol, lwd = 2)
#                dev.off()
#                #sink()
#                
#            }
#            if(isTRUE(sampling)){
#                #TODO: still to be tested
#                significance1 <- significanceList[[4]]
#                performSampling(myData, XMin, significance1, weatherPar, catVal, sampleSize=20)
#            }

        } # end of crime category loop
    } # end of weatherPars loop
} # end of main


