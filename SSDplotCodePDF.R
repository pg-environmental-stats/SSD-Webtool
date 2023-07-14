### the Density plot version
print("Running PDF plot code")
print(fitOBJ$distName)
print("check 1")
if( posDistTF)yMax <- max(dFUN(x = seq(testData$responses[1],tail(testData$responses,1),length=100),fitOBJ$distPars$location,fitOBJ$distPars$scale))
print("check 2")
if(!posDistTF)yMax <- max(dFUN(x = 10^seq(log10(testData$responses[1]),log10(tail(testData$responses,1)),length=100),fitOBJ$distPars$location,fitOBJ$distPars$scale))
print("check 3")
print(c(yMax=yMax))
#xlims should be left from CDF plots
print(c(xlims=xlims))

if(!is.null(yMaxFORCE))yMax <- yMaxFORCE
par(mai=c(input$figH*.15, input$figW*.15, 0, input$figW*0)+0.1,
    omi=rep(0,4))
plotSetupGeneric(inputDF=input2plot,
                 yRange=c(0,yMax),
                 xRange=xlims,
                 cexLAB=cexLAB,cexAXIS=cexAXIS,cexLWD=cexLWD,
                 plotType=c("CDF","PDF")[2],
                 logscaleTF=!posDistTF,
                 ptColor="darkgray",
                 xlabSTR=paste0(input$xLab," (",input$units,")"),
                 ylabSTR="Distribution Density")
rug(x=testData$responses,side=1,lwd=cexLWD)
if(inputList$doGrps){
  for(iGrp in unique(testData$groups)){
    print(c(iGrp==iGrp))
    colorVal <- colorList[which(iGrp==unique(testData$groups))]
    with(subset(testData,groups==iGrp),{rug(x=responses,side=1,col=colorVal,lwd=cexLWD)})
  }
  #if(inputList$doLegend)legend(x=fitOBJ$fit[2],y=par("usr")[4],
  #       legend=unique(testData$groups),col=colorList[match(unique(testData$groups),unique(testData$groups))],
  #       pch=16,
  #       cex = input$hcxSize)
}
###
maxProbCDF <- pFUN(Inf,fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1])
maxProbCDF <- ifelse(maxProbCDF<1,maxProbCDF-0.0001,0.9999)
print(c(maxProbCDF=maxProbCDF))
dRange <- qFUN(p = c(0.0001,maxProbCDF),fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1])
print(c(dRange=dRange))
xVals <- seq(dRange[1],dRange[2],length=1000)
if(!posDistTF)lines(y=dFUN(xVals,fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1]),x=10^(xVals),col=lineColors[1],lwd=cexLWD*1.7)
if( posDistTF)lines(y=dFUN(xVals,fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1]),x=   (xVals),col=lineColors[1],lwd=cexLWD*1.7)

maxProbCDF <- pFUN(Inf,fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2])
maxProbCDF <- ifelse(maxProbCDF<1,maxProbCDF-0.0001,0.9999)
print(c(maxProbCDF=maxProbCDF))
dRange <- qFUN(p = c(0.0001,maxProbCDF),fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2])
xVals <- seq(dRange[1],dRange[2],length=1000)
if(!posDistTF)lines(y=dFUN(xVals,fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2]),x=10^(xVals),col=lineColors[2],lwd=cexLWD)
if( posDistTF)lines(y=dFUN(xVals,fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2]),x=   (xVals),col=lineColors[2],lwd=cexLWD)
if(input$doGrays){
  if(!posDistTF)lines(y=dFUN(xVals,fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2]),x=10^(xVals),col=lineColors[3],lwd=cexLWD,lty=2)
  if( posDistTF)lines(y=dFUN(xVals,fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2]),x=   (xVals),col=lineColors[3],lwd=cexLWD,lty=2)
}
maxProbCDF <- pFUN(Inf,fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3])
maxProbCDF <- ifelse(maxProbCDF<1,maxProbCDF-0.0001,0.9999)
print(c(maxProbCDF=maxProbCDF))
dRange <- qFUN(p = c(0.0001,maxProbCDF),fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3])
xVals <- seq(dRange[1],dRange[2],length=1000)
if(!posDistTF)lines(y=dFUN(xVals,fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3]),x=10^(xVals),col=lineColors[2],lwd=cexLWD)
if( posDistTF)lines(y=dFUN(xVals,fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3]),x=   (xVals),col=lineColors[2],lwd=cexLWD)
if(input$doGrays){
  if(!posDistTF)lines(y=dFUN(xVals,fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3]),x=10^(xVals),col=lineColors[3],lwd=cexLWD,lty=2)
  if( posDistTF)lines(y=dFUN(xVals,fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3]),x=   (xVals),col=lineColors[3],lwd=cexLWD,lty=2)
}
if(PCT.shift<=0.4){
  abline(v=fitOBJ$fit[2])
  text(x=fitOBJ$fit[2],y=par("usr")[4],
       labels = bquote(.(HCX.lab)==.(HCX.str)),
       cex = input$hcxSize,
       adj=c(1,-0.2),srt=90)
  decPlaces <- nchar(HCX.str)-regexpr(pattern = "\\.",HCX.str)
  LCL.str <- format(signif(fitOBJ$fit["LowerCL"],3),scientific = FALSE)
  decPlaces2 <- nchar(LCL.str)-regexpr(pattern = "\\.",LCL.str)
  if(decPlaces2>decPlaces){
    trimN <- decPlaces2-decPlaces
    LCL.str <- substring(LCL.str,1,nchar(LCL.str)-trimN)
  }
  abline(v=fitOBJ$fit["LowerCL"],col="gray")
  text(x=fitOBJ$fit["LowerCL"],y=par("usr")[4],
       labels = bquote(LCL==.(LCL.str)),
       cex = input$hcxSize,
       adj=c(1,-0.2),srt=90)
}
if(PCT.shift>0.4){
  abline(v=fitOBJ$fit[2])
  text(x=fitOBJ$fit[2],y=par("usr")[4],
       labels = bquote(.(HCX.lab)),
       cex = input$hcxSize,
       adj=c(1,-0.2),srt=90)
  abline(v=fitOBJ$fit["LowerCL"],col="gray")
  text(x=fitOBJ$fit["LowerCL"],y=par("usr")[4],labels = "LCL",adj=c(1,-0.2),srt=90)
}

if(posDistTF){
  ### If positive range distribution, convert to log-scale
  ### the input x is POSITIVE values!!!
  ### Note the equivalence.  If we know a positive value distribution, say log-normal
  ### dnorm(-2) = dlnorm(exp(-2))*exp(-2)
  dFUN.log <- function(x,...){
    dFUN(x,...)*x
  }
  print("check 1")
  yMax <- max(dFUN.log(x = 10^seq(log10(testData$responses[1]),log10(tail(testData$responses,1)),length=100),fitOBJ$distPars$location,fitOBJ$distPars$scale))
  print("check 3")
  print(c(yMax=yMax))
  print(c(xlimx=xlims))
  
  if(!is.null(yMaxFORCE))yMax <- yMaxFORCE
  par(mai=c(input$figH*.15, input$figW*.15, 0, input$figW*0)+0.1,
      omi=rep(0,4))
  plotSetupGeneric(inputDF=input2plot,
                   yRange=c(0,yMax),
                   xRange=xlims,
                   cexLAB=cexLAB,cexAXIS=cexAXIS,cexLWD=cexLWD,
                   plotType=c("CDF","PDF")[2],
                   logscaleTF=TRUE,
                   ptColor="darkgray",
                   xlabSTR=paste0(input$xLab," (",input$units,")"),
                   ylabSTR="Distribution Density")
  rug(x=testData$responses,side=1,lwd=cexLWD)
  if(inputList$doGrps){
    for(iGrp in unique(testData$groups)){
      print(c(iGrp==iGrp))
      colorVal <- colorList[which(iGrp==unique(testData$groups))]
      with(subset(testData,groups==iGrp),{rug(x=responses,side=1,col=colorVal,lwd=cexLWD)})
    }
    #if(inputList$doLegend)legend(x=fitOBJ$fit[2],y=par("usr")[4],
    #       legend=unique(testData$groups),col=colorList[match(unique(testData$groups),unique(testData$groups))],
    #       pch=16,
    #       cex = input$hcxSize)
  }
  maxProbCDF <- pFUN(Inf,fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1])
  maxProbCDF <- ifelse(maxProbCDF<1,maxProbCDF-0.0001,0.9999)
  print(c(maxProbCDF=maxProbCDF))
  dRange <- qFUN(p = c(0.0001,maxProbCDF),fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1])
  xVals <- exp(seq(log(dRange[1]),log(dRange[2]),length=1000))
  if(!posDistTF)lines(y=dFUN.log(xVals,fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1]),x=10^(xVals),col=lineColors[1],lwd=cexLWD*1.7)
  if( posDistTF)lines(y=dFUN.log(xVals,fitOBJ$distPars$location[1],fitOBJ$distPars$scale[1]),x=   (xVals),col=lineColors[1],lwd=cexLWD*1.7)
  
  maxProbCDF <- pFUN(Inf,fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2])
  maxProbCDF <- ifelse(maxProbCDF<1,maxProbCDF-0.0001,0.9999)
  print(c(maxProbCDF=maxProbCDF))
  dRange <- qFUN(p = c(0.0001,maxProbCDF),fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2])
  xVals <- exp(seq(log(dRange[1]),log(dRange[2]),length=1000))
  if(!posDistTF)lines(y=dFUN.log(xVals,fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2]),x=10^(xVals),col=lineColors[2],lwd=cexLWD)
  if( posDistTF)lines(y=dFUN.log(xVals,fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2]),x=   (xVals),col=lineColors[2],lwd=cexLWD)
  if(input$doGrays){
    if(!posDistTF)lines(y=dFUN.log(xVals,fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2]),x=10^(xVals),col=lineColors[3],lwd=cexLWD,lty=2)
    if( posDistTF)lines(y=dFUN.log(xVals,fitOBJ$distPars$location[2],fitOBJ$distPars$scale[2]),x=   (xVals),col=lineColors[3],lwd=cexLWD,lty=2)
  }
  maxProbCDF <- pFUN(Inf,fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3])
  maxProbCDF <- ifelse(maxProbCDF<1,maxProbCDF-0.0001,0.9999)
  print(c(maxProbCDF=maxProbCDF))
  dRange <- qFUN(p = c(0.0001,maxProbCDF),fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3])
  xVals <- exp(seq(log(dRange[1]),log(dRange[2]),length=1000))
  if(!posDistTF)lines(y=dFUN.log(xVals,fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3]),x=10^(xVals),col=lineColors[2],lwd=cexLWD)
  if( posDistTF)lines(y=dFUN.log(xVals,fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3]),x=   (xVals),col=lineColors[2],lwd=cexLWD)
  if(input$doGrays){
    if(!posDistTF)lines(y=dFUN.log(xVals,fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3]),x=10^(xVals),col=lineColors[3],lwd=cexLWD,lty=2)
    if( posDistTF)lines(y=dFUN.log(xVals,fitOBJ$distPars$location[3],fitOBJ$distPars$scale[3]),x=   (xVals),col=lineColors[3],lwd=cexLWD,lty=2)
  }
  if(PCT.shift<=0.4){
    abline(v=fitOBJ$fit[2])
    text(x=fitOBJ$fit[2],y=par("usr")[4],
         labels = bquote(.(HCX.lab)==.(HCX.str)),
         cex = input$hcxSize,
         adj=c(1,-0.2),srt=90)
    decPlaces <- nchar(HCX.str)-regexpr(pattern = "\\.",HCX.str)
    LCL.str <- format(signif(fitOBJ$fit["LowerCL"],3),scientific = FALSE)
    decPlaces2 <- nchar(LCL.str)-regexpr(pattern = "\\.",LCL.str)
    if(decPlaces2>decPlaces){
      trimN <- decPlaces2-decPlaces
      LCL.str <- substring(LCL.str,1,nchar(LCL.str)-trimN)
    }
    abline(v=fitOBJ$fit["LowerCL"],col="gray")
    text(x=fitOBJ$fit["LowerCL"],y=par("usr")[4],
         labels = bquote(LCL==.(LCL.str)),
         cex = input$hcxSize,
         adj=c(1,-0.2),srt=90)
  }
  if(PCT.shift>0.4){
    abline(v=fitOBJ$fit[2])
    text(x=fitOBJ$fit[2],y=par("usr")[4],
         labels = bquote(.(HCX.lab)),
         cex = input$hcxSize,
         adj=c(1,-0.2),srt=90)
    abline(v=fitOBJ$fit["LowerCL"],col="gray")
    text(x=fitOBJ$fit["LowerCL"],y=par("usr")[4],labels = "LCL",adj=c(1,-0.2),srt=90)
  }
}