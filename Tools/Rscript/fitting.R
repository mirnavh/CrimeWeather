printFitResultsGLM <- function(sfit, kind){
    # Print the fit results from glm fit to screen/logfile

    print("")
    print("")
    print(paste("printing fit result for ",kind," fit:"))
    print("")
    print("coefs: ")
    print(sfit$coef)
    print("")
#    print("residuals:")
#    print(sfit$residuals)
    print("")
    print("covariance matrix (conv.unscaled:")
    print(sfit$conv.unscaled)
    print("")
    print("degrees of freedom :")
    print(sfit$df)
    print("")
    print(paste("sigma (residual standard error): ", sfit$sigma))
    print(paste("r squared : ", sfit$r.squared))
    print(paste("adjusted r squared : ", sfit$adj.r.squared))
    print(paste("f statistic: ", sfit$fstatistic))
    f <- sfit$fstatistic
#    p <- pf(f[1], f[2], f[3], lower.tail = F)
#    attributes(p) <- NULL
#    print(paste("P value: ", p))
    print("")
    print("")
}

printFitResultsLM <- function(sfit, kind){
    # Print the fit results from lm fit to screen/logfile

    print("")
    print("")
    print(paste("printing fit result for ",kind," fit:"))
    print("")
    print("coefs: ")
    print(sfit$coef)
    print("")
#    print("residuals:")
#    print(sfit$residuals)
    print("")
    print("covariance matrix (conv.unscaled:")
    print(sfit$conv.unscaled)
    print("")
    print("degrees of freedom :")
    print(sfit$df)
    print("")
    print(paste("sigma (residual standard error): ", sfit$sigma))
    print(paste("r squared : ", sfit$r.squared))
    print(paste("adjusted r squared : ", sfit$adj.r.squared))
    print(paste("f statistic: ", sfit$fstatistic))
    f <- sfit$fstatistic
    p <- pf(f[1], f[2], f[3], lower.tail = F)
    attributes(p) <- NULL
    print(paste("P value: ", p))
    print("")
    print("")
}

performLMFit <- function(df, X,Y, kind="lin"){
    # Perform lm fit
    # Default is a linear relation between <Y> and <X>
    # Also a second order or a third order polynomial can be choosen.

    # Are we fitting lineair, poly2 or poly3 relation between Y and X?
    if(kind == 'lin'){
       formuleS <- paste(Y,"~ ", X)
    } else if(kind == 'poly2'){
       formuleS <- paste(Y,"~ poly(", X,",2)")
    } else if(kind == 'poly3'){
       formuleS <- paste(Y,"~ poly(", X,",3)")
    }
    formule <- as.formula(formuleS)

    fit0 = lm(formule, data =df)
    typeFitObject <- typeof(fit0)
    # Get fit parameters
    sfit0 <- summary(fit0)
    coefs0 <- coef(sfit0) 
    cc0 <- cor(df[X], df[Y])

    # Print fit parameters. So they can be written to a logfile.
    printFitResultsLM(sfit0, kind)

    #return the fit result
    # Note: coefs0 can also be obtained from sfit0
    significance0 <- list(fit0, sfit0, cc0, coefs0)
    return(significance0)
}

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

performMaxLFit <- function(df,X, kind="lin"){
    # Model: 
    # lambda_i = b0 + b1*X_i (lin) or 
    # lambda_i = b0 + b1*X_i + b2*X_i^2 (poly2) or
    # lambda = b0 + b1*X_i + b2*X_i^2 + b3*X_i^3 (poly3)
    # freq_i ~ Poisson(lambda_i)
    # freq_i ~ QuasiPoisson(lambda_i)

    if(kind == 'lin'){
       formuleS <- paste("freq","~ ", X)
    } else if(kind == 'poly2'){
       formuleS <- paste("freq","~ poly(", X,",2)")
    } else if(kind == 'poly3'){
       formuleS <- paste("freq","~ poly(", X,",3)")
    }
    formule <- as.formula(formuleS)

    # Note: link = "identity": lambda ~ X; link = "log": log(lambda) ~ X; link = "sqrt": sqrt(lambda) ~ X
    # We need (probably) link - "identity"
    crimesGLM <- glm(formule, family = poisson(link="identity"), data=df)
    crimesGLMQ <- glm(formule, family = quasipoisson(link="identity"), data=df)
    print("summary of fit using poisson")
    print(summary(crimesGLM))
    print("summary of fit using quasi-poisson")
    print(summary(crimesGLMQ))
    return(crimesGLM)
}


poissonRegression <- function(df, weatherPar, baseDir){
    # Perform Poisson regression

    # define the maxFit directory as a sub directory in the base directory
    maxFitDir <- paste(baseDir,'/maxFit',sep="")
    # create it if it doesn't already exist
    dir.create(file.path(maxFitDir), showWarnings = FALSE)
    catVal <- df[1,'mainCat']

    XMin = -99
#    dfOK <- cleanupDF(df, weatherPar, XMin)
    # Create dataset with number of crimes per day ('freq'), 'Datum' and X columns
    total <- produceDFCrimesperday(dfOK, weatherPar)
    summary(total)
    # replace NA by 0. Is this needed?
    total[is.na(total)] <- 0

    # TODO: At the moment we only fit a lineait fit. Should we also implement polynomial fit
    # of second and third degree??????
    # Uncomment following line if output is written to logfile
    logFileName <- paste(maxFitDir,"/maxFit_Par",weatherPar,"_Cat",catVal,".log",sep="")
    sink(logFileName, type=c("output", "message"))
    fit <- performMaxLFit(total,weatherPar)
    fit2 <- performMaxLFit(total,weatherPar, kind="poly2")
    fit3 <- performMaxLFit(total,weatherPar, kind="poly3")

#    The below line has no use.
#    printFitResultsGLM(fit, "lin")

    # TODO: get xMin, xMax and xStep from dfOK???????
    xMin = -20
    xMax = 30
    xStep = 0.5
    # Create Prediction dataset
    predDF <- createPredDf(fit,weatherPar, xMin, xMax, xStep)
    predDF2 <- createPredDf(fit2,weatherPar, xMin, xMax, xStep)
    predDF3 <- createPredDf(fit3,weatherPar, xMin, xMax, xStep)

    plotName <- paste(maxFitDir,"/MLFitresult_catVal_",catVal, sep="")
#    jpeg(file = paste(plotName,".jpg",sep=""))
    pdf(file = paste(plotName,".pdf",sep=""))

    mainS <- paste("Number of crimes per day VS ", weatherPar," for crime cat: ", catVal)
    subS <- paste("using: lin fit")
    subS2 <- paste("using: poly2 fit")
    subS3 <- paste("using: poly3 fit")
    xLab <- weatherPar
    yLab <- "number of crimes per day"
    plotCol <- "lightgrey"
    # plot lin fit
    plot(total[[weatherPar]], total[['freq']], cex = 1, col = plotCol, pch = 19, ylab = yLab, xlab = xLab, main=mainS, sub=subS)
    # overlay fit using the prediction dataset
    fitCol <- "green"
    lines(predDF[['XPred']], predDF[['YPred']], col = fitCol, lwd = 2)

    # plot poly2 fit
    plot(total[[weatherPar]], total[['freq']], cex = 1, col = plotCol, pch = 19, ylab = yLab, xlab = xLab, main=mainS, sub=subS2)
    # overlay fit using the prediction dataset
    fitCol <- "green"
    lines(predDF2[['XPred']], predDF2[['YPred']], col = fitCol, lwd = 2)

    # plot poly3 fit
    plot(total[[weatherPar]], total[['freq']], cex = 1, col = plotCol, pch = 19, ylab = yLab, xlab = xLab, main=mainS, sub=subS3)
    # overlay fit using the prediction dataset
    fitCol <- "green"
    lines(predDF3[['XPred']], predDF3[['YPred']], col = fitCol, lwd = 2)

    dev.off()

    plotName <- paste(maxFitDir,"/MLFitresult2_catVal_",catVal, sep="")
    pdf(file = paste(plotName,".pdf",sep=""))
    xLab <- weatherPar
    yLab <- "number of crimes per day"
    plotCol <- "lightgrey"
    plot(total[[weatherPar]], total[['freq']], cex = 1, col = plotCol, pch = 19, ylab = yLab, xlab = xLab)
    plot(fit)
    dev.off()
    sink()
                
}

runPoissonReg <- function(myData, datalabDir, weatherPars ) {
    # Loop over different weather parameters
    for (weatherPar in weatherPars){
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
            print(paste("Looping over crime category",catVal))

            # poisson regression for this weatherPar and this catVal:
            poissonRegression(catDf, weatherPar, baseDir)

        } # end of category loop
    } # end of weatherPar loop
} # end of runPoissonReg


