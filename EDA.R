# Exploratory Data Analysis

makeBoxPlots <- function(df, xList, directory){
    # Produce box and whisker  plots for every x in xlist.
    # 4 plots per page.

    # write boxplots of x to pdf file
    plotName <- "data_quality_boxplot_test.pdf"
    # 4 plots per page:
    pdf(file = plotName)
    oldP <- par(mfrow=c(2,2))

    for (x in xList){
        xlabel <- x
        title <- paste("boxplot of ",x)
        myPlot <- boxPlot(df,x xlabel, title)
        print(myPlot)
    } # end of loop over y list

    par(oldP)
    dev.off()

} # end of makeBoxPlots

makeHistograms <- function(df, xList, directory){
    # Produce histograms for every x Xlist.
    # 4 plots per page.

    # write histograms of x to pdf file
    plotName <- "data_quality_test.pdf"
    # 4 plots per page:
    pdf(file = plotName)
    oldP <- par(mfrow=c(2,2))

    for (x in xList){
        xlabel <- x
        title <- paste("histogram of ",x)
        # Is this reasonable?
        binwidth = max(df[x]) - min(df[x])/10.
        myPlot <- histPlot(df,x xlabel, title, binwidth)
        print(myPlot)
    } # end of loop over y list

    par(oldP)
    dev.off()

} # end of makeHistograms

makeScatterPlots <- function(df, xList, yList, directory){
    # Produce scatter plots of very y in Ylist VS every x in Xlist.
    # One pdf file per y
    # 4 plots per page.
    for (y in yList){
        # write plots of y vs x variables to pdf file
        plotName <- paste(y,"_vs_vars_test.pdf",sep="")
        pdf(file = plotName)
        # 4 plots per page:
        oldP <- par(mfrow=c(2,2))

        ylabel <- y
        for (x in xList){
            cxy <- cor(df[x],df[y])
            xlabel <- x
            title <- paste(y,"VS",x,"with corr: ",cxy)
            myPlot <- scatPlot(df,x,y, xlabel, ylabel,title)
            print(myPlot)
        } # end of loop over x list
        par(oldP)
        dev.off()
    } # end of loop over y list

} # end of makeScatterPlots

runEDATests <- function(df, yList, xList, directory){
    # Perform several EDA test to see what the data looks like

    EDADir <- paste(directory,'/EDA',sep="")
    # create it if it doesn't already exist
    dir.create(file.path(EDADir), showWarnings = FALSE)
    # Make histograms of all variables of interest. What does distribution look like. What about outliers or funny values?
    makeHistograms(df, xList, EDADir)

    # Make box plots of all variables of interest. What does distribution look like. What about outliers or funny values?
    makeBoxPlots(df, xList, EDADir)

    # Make scatter plots of all y variables of interest vs all xvalues VS of interest. Also print correlation.
    # Does a correlation exist between the x and y values?
    makeScatterPlots(df, xList, yList, EDADir)

# Are there other tests that need to be included?

} # end of runEDATests

