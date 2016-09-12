plotYVsX <- function(X, doXOnly=FALSE, Y="", xLab="", yLab="", mainS="", subS="", plotCol="black", plotFit=FALSE,fit="", fitCol="blue", fitKind="lin", predX="", predY="", plotErrors=FALSE, arrowX="", arrowY0="", arrowY1=""){
    # Plot <Y> VS <X> with x lable <xLab> (if set), y lable <yLab> (if set), main title <mainS> (if set)
    # and sub title <subS> (if set). If <doXOnly> is set  only <X> is plotted.
    # if <plotFit> is set the result from the fit is plotted.
    # if <plotErrors> is set the error bars in <Y> are plotted.
    # By default the plot is black, the fit is blue

    if(isTRUE(doXOnly)){
        plot(X, xlab=xLab, ylab=yLab, main=mainS, sub = subS, col=plotCol)
    } else {
        plot(X, Y, xlab=xLab, ylab=yLab, main=mainS, sub = subS, col=plotCol)
        # If fit was made, plot fit line:
        if(isTRUE(plotFit)){
            if (fitKind == "lin"){
                abline(fit, col=fitCol)
            } else if( fitKind == "poly2" | fitKind == "poly3") {
#                lines(predDF[['XPred']], predDF[['YPred']], col = fitCol, lwd = 2)
                lines(predX, predY, col = fitCol, lwd = 2)
            }
        }
        # If error bars are requested, plot error bars as arrows:
        if(isTRUE(plotErrors)){
            #print("plot errors")
            arrows(arrowX, arrowY0, arrowX, arrowY1, length=0.05, angle=90, code=3, col=plotCol)
        }
    }
}

plotResiduals <- function(fit, subS){
    # Plot only residuals
    xLab<-"residuals"
    mainS <-  "residuals"
    plotYVsX(residuals(fit), doXOnly=TRUE, xLab=xLab, mainS=mainS, subS=subS)
}
plotResidualsVsFitted <- function(fit, subS ){
    # Plot residuals VS fitted values
    yLab<-"residuals"
    xLab<-"fitted values"
    mainS <-  "residuals VS fitted values"
    plotYVsX(fitted(fit), Y=residuals(fit),  xLab=xLab, yLab=yLab, mainS=mainS, subS=subS)
}

plotHist <- function(X, plotName="", xLab="", mainS="", subS="", histCol="black", X0="", vCol="blue" ){
    # Plot histogram of <X> with x lable <xLab> (if set), main title <mainS> (if set)
    # and sub title <subS> (if set). If requested a vertical line is drown for value <X0>
    # by default the histogram is black and the vertical line is blue.
    # The histogram is plotted to file name <plotName>. <plotName> can include directory path
    jpeg(file = plotName)
    hist(X, xlab=xLab, main=mainS, sub=subS, col=histCol)
    if( X0 != ""){
        abline(v=X0, col=vCol)
    }
    dev.off()
}

scatPlot <- function(df, X, Y, xlabel, ylabel, title){
    # Produce scatter plot using ggplot

    myPlot <- ggplot(df, aes(x=X, y=Y)) + geom_points(color="red") + stat_smooth(methode="lm") 
    myPlot <- myPlot + xlab(xlabel) + ylab(ylabel) + ggtitle(title)

    return myPlot

} # end of scatPlot

boxPlot <- function(df,X, xlabel, title){
    # Produce box and whisker plot using ggplot

    myPlot <- ggplot(df, aes(x=X)) + geom_boxplot() 
    myPlot <- myPlot + xlab(xlabel) + ggtitle(title)

    return myPlot

} # end of boxPlot

histPlot <- function(df,X, xlabel, title, binwidth){
    # Produce histogram using ggplot

    myPlot <- ggplot(df, aes(x=X)) + geom_bar(binwidth=binwidth) 
    myPlot <- myPlot + xlab(xlabel) + ggtitle(title)

    return myPlot


} # end of histPlot
