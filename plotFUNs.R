
plotSetupGeneric <- function(inputDF,
                             yRange=c(0,1),
                             xRange=NULL,
                             cexLAB=1.5,cexAXIS=1.5,cexLWD=1.5,
                             plotType=c("CDF","PDF")[1],
                             logscaleTF=TRUE,
                             ptColor="darkgray",
                             xlabSTR="Exposure Level",
                             ylabSTR="Probability"){
  #cat("\nplotSetup say modelType = ",modelType,"\n",sep="")
  print("Inside of plotSetupGeneric")
  cexPCH <- ifelse(cexLWD<1,1,cexLWD*.75)
  print(c(logscaleTF=logscaleTF))
  if(!logscaleTF){
    print("Inside of plotSetupGeneric, logscale=FALSE")
    print(inputDF)
    if(is.null(xRange))plot(x=1,y=1,
                            type="n",bty="n",xlab="",ylab="",
                            ylim=yRange,
                            axes=FALSE)
    if(!is.null(xRange))plot(x=1,y=1,
                             type="n",bty="n",xlab="",ylab="",
                             ylim=yRange,xlim=xRange,
                             axes=FALSE)
    if(plotType=="CDF"){
      with(inputDF,{
        points(x=doses,y=yVals,cex=cexPCH)
      })
    }
    axis(side=1,cex.axis=cexAXIS,lwd=cexLWD)
    axis(side=2,cex.axis=cexAXIS,lwd=cexLWD,las=1)
  }
  if(logscaleTF){
    print("Inside of plotSetupGeneric, logscale=TRUE")
    print(inputDF)
    print(xRange)
    if(is.null(xRange))plot(x=1,y=1,
                            log='x',
                            type="n",bty="n",xlab="",ylab="",
                            ylim=yRange,
                            xlim=range(inputDF$doses,na.rm = TRUE),
                            axes=FALSE)
    if(!is.null(xRange))plot(x=1,y=1,
                             log='x',
                             type="n",bty="n",xlab="",ylab="",
                             ylim=yRange,xlim=xRange,
                             axes=FALSE)
    if(plotType=="CDF" & FALSE){
      with(inputDF,{
        points(x=10^logDose,y=yVals,cex=cexPCH)
        points(x=10^logDose,y=yVals,cex=cexPCH,pch=16,col="orange")
      })
    }
    logTicks <- axisTicks(usr = range(inputDF$logDose[inputDF$doses!=0])*c(0.95,1.05),log = TRUE)
    logTicks2 <- axisTicks(usr = par("usr")[1:2],log = TRUE)
    logTicks <- c(
      logTicks2[logTicks2<min(logTicks)],
      logTicks,
      logTicks2[logTicks2>max(logTicks)])
    axis(side=1,cex.axis=cexAXIS,lwd=cexLWD,at=logTicks,labels = sapply(logTicks,format))
    if(min(inputDF$doses)==0){
      axis(side=1,cex.axis=cexAXIS,lwd=cexLWD  ,at=10^head((inputDF$logDose)[inputDF$doses==0],1),labels = "0")
      axis(side=1,cex.axis=cexAXIS,lwd=cexLWD/2,at=c(min(10^(inputDF$logDose)),max(logTicks)),labels = FALSE,lty=2)
    }
    axis(side=2,cex.axis=cexAXIS,lwd=cexLWD,las=1)
  }
  mtext(side=1,outer=FALSE,line=par("mar")[1]-1.25,text = xlabSTR,
        at=10^mean(par("usr")[1:2]),
        cex = cexLAB,padj=0)
  mtext(side=2,outer=TRUE,line=-0.35,text = ylabSTR,cex = cexLAB,padj=1)
  #box(which = "outer", lty = "solid",col="gray")
  invisible()
}


SSDplotFUN <- function(testData,inputList,xlimVals=NULL){
  print("Inside SSDplotFUN")
  print(testData)
  normRange <- 10^qnorm(c(.001,.999),mean = mean(log10(testData$responses)),sd=sd(log10(testData$responses)))
  xlabBuild <- paste0(inputList$xLab," (",inputList$units,")")
  if(inputList$doseScale=="Measured"){
    xVals <- quantile(testData$responses,probs = seq(0.0001,0.9999,length=10000),type = 8)
    yVals <- seq(0.0001,0.9999,length=10000)
    pointIDs <- sapply(testData$responses,FUN = function(x)which.min(abs(x-xVals)))
    pointIDs[1] <- max(which(xVals==xVals[1]))
    if(!inputList$doGrps)inputDF <- data.frame(doses=xVals[pointIDs],yVals=yVals[pointIDs])
    if( inputList$doGrps)inputDF <- data.frame(doses=xVals[pointIDs],yVals=yVals[pointIDs],groups=testData$groups)
    plotSetupGeneric(inputDF = inputDF,
                     yRange=c(0,1),
                     cexLWD=inputList$lineSize,cexLAB=inputList$labelSize,cexAXIS=inputList$axisSize,
                     logscaleTF=FALSE,
                     xlabSTR="",
                     ylabSTR="")
    lines(x=xVals,y=yVals,col=lineColors[1],lwd=1.5*inputList$lineSize)
    if(inputList$doGrps){
      with(inputDF,{
        points(x=doses,y=inputDF$yVals,cex=inputList$lineSize,
                                    col="black",
                                    pch=pchOpens[match(groups,unique(groups))])
        points(x=doses,y=inputDF$yVals,cex=inputList$lineSize,col=colorList[match(groups,unique(groups))],pch=pchSolids)
        #legend("topleft",legend=unique(groups),col=colorList[match(unique(groups),unique(groups))],cex = inputList$axisSize)
      })
    }
  }
  if(inputList$doseScale=="Log"){
    xVals <- quantile(log10(testData$responses),probs = seq(0.0001,0.9999,length=10000),type=8)
    yVals <- seq(0.0001,0.9999,length=10000)
    pointIDs <- sapply(log10(testData$responses),FUN = function(x)which.min(abs(x-xVals)))
    pointIDs[1] <- max(which(xVals==xVals[1]))
    if(!inputList$doGrps)inputDF <-  data.frame(doses=10^(xVals[pointIDs]),logDose=xVals[pointIDs],yVals=yVals[pointIDs])
    if( inputList$doGrps)inputDF <-  data.frame(doses=10^(xVals[pointIDs]),logDose=xVals[pointIDs],yVals=yVals[pointIDs],groups=testData$groups)
    plotSetupGeneric(inputDF =inputDF,
                     yRange=c(0,1),xRange=xlimVals,
                     cexLWD=inputList$lineSize,cexLAB=inputList$labelSize,cexAXIS=inputList$axisSize,
                     logscaleTF=TRUE,
                     xlabSTR="",
                     ylabSTR="")
    lines(x=10^xVals,y=yVals,col=lineColors[1],lwd=1.5*inputList$lineSize)
    if(inputList$doGrps){
      print("doGrps")
      print(inputDF)
      points(x=inputDF$doses,y=inputDF$yVals,
             cex=inputList$lineSize,
             col="black",
             pch=pchOpens[match(inputDF$groups,unique(inputDF$groups))])
      points(x=inputDF$doses,y=inputDF$yVals,
               cex=inputList$lineSize,
               col=colorList[match(inputDF$groups,unique(inputDF$groups))],
               pch=pchSolids[match(inputDF$groups,unique(inputDF$groups))])
        print(par("usr"))
        print(inputList)
        if(FALSE)legend(x = list(x=min(inputDF$doses),y=1),legend=unique(inputDF$groups),
               col=colorList[match(unique(inputDF$groups),unique(inputDF$groups))],
               pch=16,
               cex = inputList$lineSize)
    }
    if(!inputList$doGrps){
      points(x=inputDF$doses,y=inputDF$yVals,
             cex=inputList$lineSize,
             col="black",pch=1)
      points(x=inputDF$doses,y=inputDF$yVals,
             cex=inputList$lineSize,
             col="gray",pch=16)
    }
  }
  abline(h=inputList$ECXvalue,lty=3,lwd=inputList$lineSize)
  mtext(side=1,outer=TRUE,line=-1.25,text = xlabBuild,cex = inputList$labelSize,padj=0)
  mtext(side=2,outer=TRUE,line=-0.35,text = "Probability",cex = inputList$labelSize,padj=1)
  if(inputList$speciesMargin>0 & inputList$speciesSize>0 & !inputList$doGrps){
    mtext(side=4,at=yVals[pointIDs],text = testData$species,
          las=1,family="serif",font = 3,cex=inputList$speciesSize)
  }
  #if(inputList$speciesMargin>0 & inputList$speciesSize>0 & inputList$doGrps & inputList$doGrays){
  #  mtext(side=4,at=yVals[pointIDs],text = testData$species,
  #        las=1,family="serif",font = 3,cex=inputList$speciesSize,
  #        col="black",adj=-0.005)
  #}
  if(inputList$speciesMargin>0 & inputList$speciesSize>0 & inputList$doGrps){
    mtext(side=4,at=yVals[pointIDs],text = testData$species,
          las=1,family="serif",font = 3,cex=inputList$speciesSize,
          col=colorList[match(inputDF$groups,unique(inputDF$groups))])
  }
  #xValsNorm <- 10^qnorm(seq(0.0001,0.9999,length=10000),mean = mean(log10(testData$responses)),sd=sd(log10(testData$responses)))
  #lines(x=xValsNorm,y=seq(0.0001,0.9999,length=10000),lwd=3,col="magenta")
  #box(which = "outer", lty = "solid",col="gray")

}
