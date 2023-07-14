# Do not use.  Plotting now performed inside or SSD.run.code.R


cexLAB <- input$labelSize # size of xlab and ylab
cexAXIS <- input$axisSize  # tick annotations size on axis
cexLWD <- input$lineSize  # line width
cexPCH <- ifelse(cexLWD<1,1,cexLWD*.75)  # plot symbol size



pageBreakPDF("Leave-One-Out\nAnalysis")
pageBreakPDF("A:\nLogistic\nLeave-One-Out")
xLimLower <- 10^floor(log10(min(c(
  exp(qlogis(.05,LOO.results.logis[[1]][[1]][5],LOO.results.logis[[1]][[1]][6]))/5,
  sort(unlist(lapply(LOO.results.logis,FUN=function(resList){
    linesMat <- resList[[2]]
    #print(head(linesMat[100:200,]))
    linesMat[round(linesMat[,1],3)==0.05,2]
  })))))))
print(testData)
xVals <- quantile(log10(testData$responses),probs = seq(0.0001,0.9999,length=10000),type=8)
yVals <- seq(0.0001,0.9999,length=10000)
pointIDs <- sapply(log10(testData$responses),FUN = function(x)which.min(abs(x-xVals)))
pointIDs[1] <- max(which(xVals==xVals[1]))
inputDF <-  data.frame(doses=10^(xVals[pointIDs]),logDose=xVals[pointIDs],yVals=yVals[pointIDs])
print(inputDF)
print(c(xLimLower,10^(max(ceiling(log10(testData$responses))))))
xlabBuild <- paste0(inputList$xLab," (",inputList$units,")")
plotSetupGeneric(inputDF=inputDF,
                 yRange=c(0,1),
                 xRange=c(xLimLower,10^(max(ceiling(log10(testData$responses))))),
                 cexLAB=cexLAB,
                 cexAXIS=cexAXIS,
                 cexLWD=cexLWD,
                 logscaleTF=TRUE,
                 xlabSTR="",
                 ylabSTR="")
mtext(side=1,outer=TRUE,line=-1.25,text = xlabBuild,cex = inputList$labelSize,padj=0)
mtext(side=2,outer=TRUE,line=-0.35,text = "Probability",cex = inputList$labelSize,padj=1)
if(FALSE){
  plot(x=testData$responses,y=(1:nrow(testData))/(nrow(testData)+1),
       log='x',xlim=c(xLimLower,10^(max(ceiling(log10(testData$responses))))),
       ylim=c(0,1),axes=FALSE,bty="n",
       xlab=concentrationLabel,ylab="Probability")
  majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
  axis(side=1,at=majorTicks,label=majorTicks)
  axis(side=1,at=10^as.vector(sapply(ceiling(par("usr")[1]):(ceiling(par("usr")[2])-1),FUN=function(x)x+log10(1:9))),
       tck=-.01,labels=F)
  axis(side=2,las=1)
}
#box(lwd=2)
lines(x=exp(qlogis(seq(.001,.999,length=200),LOO.results.logis[[1]][[1]][5],LOO.results.logis[[1]][[1]][6])),
      y=seq(.001,.999,length=200),col="black",lwd=3)
lines(y=LOO.results.logis[[1]][[2]][,1],x=LOO.results.logis[[1]][[2]][,2],col="blue",lwd=3)
lines(y=LOO.results.logis[[1]][[2]][,1],x=LOO.results.logis[[1]][[2]][,3],col="blue",lwd=3)

invisible(lapply(LOO.results.logis[-1],FUN=function(resList){
  lines(x=exp(qlogis(seq(.001,.999,length=200),resList[[1]][5],resList[[1]][6])),
        y=seq(.001,.999,length=200),col=do.call(rgb,c(as.list(col2rgb("gray")[,1]),maxColorValue=255,alpha=.5*255)))
  lines(y=resList[[2]][,1],x=resList[[2]][,2],col=do.call(rgb,c(as.list(col2rgb("cyan")[,1]),maxColorValue=255,alpha=.5*255)))
  lines(y=resList[[2]][,1],x=resList[[2]][,3],col=do.call(rgb,c(as.list(col2rgb("cyan")[,1]),maxColorValue=255,alpha=.5*255)))
  invisible()
}))
abline(h=HC.primary,lty=3)
mtext(side=1,outer=TRUE,text="Distribution: logis",line=-1,adj=0)
hcX <- exp(qlogis(HC.primary,LOO.results.logis[[1]][[1]][5],LOO.results.logis[[1]][[1]][6]))
mtext(side=3,at=hcX,text=bquote(HC[.(round(100*HC.primary))]),adj=1.1,las=2)
mtext(side=3,at=hcX/3,text=bquote(HC[.(round(100*HC.primary))]/3),adj=1.1,las=2)
mtext(side=3,at=hcX/5,text=bquote(HC[.(round(100*HC.primary))]/5),adj=1.1,las=2)
abline(v=hcX/c(1,2,3,5),col="gray")



yLimUpper <- max(unlist(lapply(LOO.results.logis,FUN=function(resList){
  distPars <- resList[[1]][5:6]
  dlogis(distPars[1],distPars[1],distPars[2])
})))
plotSetupGeneric(inputDF=inputDF,
                 plotType="PDF",
                 yRange=c(0,yLimUpper),
                 xRange=c(xLimLower,10^(max(ceiling(log10(testData$responses))))),
                 cexLAB=cexLAB,
                 cexAXIS=cexAXIS,
                 cexLWD=cexLWD,
                 logscaleTF=TRUE,
                 xlabSTR="",
                 ylabSTR="")
mtext(side=1,outer=TRUE,line=-1.25,text = xlabBuild,cex = inputList$labelSize,padj=0)
mtext(side=2,outer=TRUE,line=-0.35,text = "Density",cex = inputList$labelSize,padj=1)

if(FALSE){
  xLimLower <- 10^floor(log10(min(c(
    exp(qlogis(.05,LOO.results.logis[[1]][[1]][5],LOO.results.logis[[1]][[1]][6]))/5,
    sort(unlist(lapply(LOO.results.logis,FUN=function(resList){
      linesMat <- resList[[2]]
      #print(head(linesMat[100:200,]))
      linesMat[round(linesMat[,1],3)==0.05,2]
    })))))))
  plot(x=xLimLower,y=yLimUpper,
       log='x',
       xlim=c(xLimLower,10^(max(ceiling(log10(testData$responses))))),
       ylim=c(0,yLimUpper),axes=FALSE,bty="n",
       xlab=concentrationLabel,ylab="Density",type="n")
  majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
  axis(side=1,at=majorTicks,label=majorTicks)
  axis(side=1,at=10^as.vector(sapply(ceiling(par("usr")[1]):(ceiling(par("usr")[2])-1),FUN=function(x)x+log10(1:9))),
       tck=-.01,labels=F)
  axis(side=2,las=1)
}
#box(lwd=2)
qVals <- qlogis(seq(.001,.999,length=200),LOO.results.logis[[1]][[1]][5],LOO.results.logis[[1]][[1]][6])
lines(y=dlogis(qVals,LOO.results.logis[[1]][[1]][5],LOO.results.logis[[1]][[1]][6]),
      x=exp(qVals),col="black",lwd=3)

invisible(lapply(LOO.results.logis[-1],FUN=function(resList){
  modelPars <- resList[[1]][5:6]
  #print(modelPars)
  qVals <- qlogis(seq(.001,.999,length=200),modelPars[1],modelPars[2])
  #print(qVals)
  lines(y=dlogis(qVals,modelPars[1],modelPars[2]),
        x=exp(qVals),col=do.call(rgb,c(as.list(col2rgb("cyan")[,1]),maxColorValue=255,alpha=.5*255)))
  invisible()
}))
rug(side=1,x=testData$responses,lwd=1,col=rgb(0,0,0,.25))
mtext(side=1,outer=TRUE,text="Distribution: logis",line=-1,adj=0)
#hc5 <- exp(qlogis(.05,LOO.results.logis[[1]][[1]][5],LOO.results.logis[[1]][[1]][6]))
#mtext(side=3,at=hc5,text=expression(HC[5]),adj=0,las=2)
#mtext(side=3,at=hc5/3,text=expression(HC[5]/3),adj=0,las=2)
#mtext(side=3,at=hc5/5,text=expression(HC[5]/5),adj=0,las=2)
#abline(v=hc5/c(1,2,3,5),col="gray")
hcX <- exp(qlogis(HC.primary,LOO.results.logis[[1]][[1]][5],LOO.results.logis[[1]][[1]][6]))
mtext(side=3,at=hcX,text=bquote(HC[.(round(100*HC.primary))]),adj=1.1,las=2)
mtext(side=3,at=hcX/3,text=bquote(HC[.(round(100*HC.primary))]/3),adj=1.1,las=2)
mtext(side=3,at=hcX/5,text=bquote(HC[.(round(100*HC.primary))]/5),adj=1.1,las=2)
abline(v=hcX/c(1,2,3,5),col="gray")


pageBreakPDF("B\nNormal\nLeave-One-Out")
xLimLower <- 10^floor(log10(min(c(
  exp(qnorm(.05,LOO.results.norm[[1]][[1]][5],LOO.results.norm[[1]][[1]][6]))/5,
  sort(unlist(lapply(LOO.results.norm,FUN=function(resList){
    linesMat <- resList[[2]]
    #print(head(linesMat[100:200,]))
    linesMat[round(linesMat[,1],3)==0.05,2]
  })))))))
plot(x=testData$responses,y=(1:nrow(testData))/(nrow(testData)+1),
     log='x',
     xlim=c(xLimLower,10^(max(ceiling(log10(testData$responses))))),
     ylim=c(0,1),axes=FALSE,bty="n",
     xlab=concentrationLabel,ylab="Probability")
majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
axis(side=1,at=majorTicks,label=majorTicks)
axis(side=1,at=10^as.vector(sapply(ceiling(par("usr")[1]):(ceiling(par("usr")[2])-1),FUN=function(x)x+log10(1:9))),
     tck=-.01,labels=F)
axis(side=2,las=1)
#box(lwd=2)
lines(x=exp(qnorm(seq(.001,.999,length=200),LOO.results.norm[[1]][[1]][5],LOO.results.norm[[1]][[1]][6])),
      y=seq(.001,.999,length=200),col="black",lwd=3)
lines(y=LOO.results.norm[[1]][[2]][,1],x=LOO.results.norm[[1]][[2]][,2],col="blue",lwd=3)
lines(y=LOO.results.norm[[1]][[2]][,1],x=LOO.results.norm[[1]][[2]][,3],col="blue",lwd=3)

invisible(lapply(LOO.results.norm[-1],FUN=function(resList){
  lines(x=exp(qnorm(seq(.001,.999,length=200),resList[[1]][5],resList[[1]][6])),
        y=seq(.001,.999,length=200),col=do.call(rgb,c(as.list(col2rgb("gray")[,1]),maxColorValue=255,alpha=.5*255)))
  lines(y=resList[[2]][,1],x=resList[[2]][,2],col=do.call(rgb,c(as.list(col2rgb("cyan")[,1]),maxColorValue=255,alpha=.5*255)))
  lines(y=resList[[2]][,1],x=resList[[2]][,3],col=do.call(rgb,c(as.list(col2rgb("cyan")[,1]),maxColorValue=255,alpha=.5*255)))
  invisible()
}))
abline(h=HC.primary,lty=3)
mtext(side=1,outer=TRUE,text="Distribution: norm",line=-1,adj=0)
#hc5 <- exp(qnorm(.05,LOO.results.norm[[1]][[1]][5],LOO.results.norm[[1]][[1]][6]))
#mtext(side=3,at=hc5,text=expression(HC[5]),adj=0,las=2)
#mtext(side=3,at=hc5/3,text=expression(HC[5]/3),adj=0,las=2)
#mtext(side=3,at=hc5/5,text=expression(HC[5]/5),adj=0,las=2)
#abline(v=hc5/c(1,2,3,5),col="gray")
hcX <- exp(qnorm(HC.primary,LOO.results.norm[[1]][[1]][5],LOO.results.norm[[1]][[1]][6]))
mtext(side=3,at=hcX,text=bquote(HC[.(round(100*HC.primary))]),adj=1.1,las=2)
mtext(side=3,at=hcX/3,text=bquote(HC[.(round(100*HC.primary))]/3),adj=1.1,las=2)
mtext(side=3,at=hcX/5,text=bquote(HC[.(round(100*HC.primary))]/5),adj=1.1,las=2)
abline(v=hcX/c(1,2,3,5),col="gray")


xLimLower <- 10^floor(log10(min(c(
  exp(qnorm(.05,LOO.results.norm[[1]][[1]][5],LOO.results.norm[[1]][[1]][6]))/5,
  sort(unlist(lapply(LOO.results.norm,FUN=function(resList){
    linesMat <- resList[[2]]
    #print(head(linesMat[100:200,]))
    linesMat[round(linesMat[,1],3)==0.05,2]
  })))))))
yLimUpper <- max(unlist(lapply(LOO.results.norm,FUN=function(resList){
  distPars <- resList[[1]][5:6]
  dnorm(distPars[1],distPars[1],distPars[2])
})))
plot(x=xLimLower,y=yLimUpper,
     log='x',
     xlim=c(xLimLower,10^(max(ceiling(log10(testData$responses))))),
     ylim=c(0,yLimUpper),axes=FALSE,bty="n",
     xlab=concentrationLabel,ylab="Density",type="n")
majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
axis(side=1,at=majorTicks,label=majorTicks)
axis(side=1,at=10^as.vector(sapply(ceiling(par("usr")[1]):(ceiling(par("usr")[2])-1),FUN=function(x)x+log10(1:9))),
     tck=-.01,labels=F)
axis(side=2,las=1)
#box(lwd=2)
qVals <- qnorm(seq(.001,.999,length=200),LOO.results.norm[[1]][[1]][5],LOO.results.norm[[1]][[1]][6])
lines(y=dnorm(qVals,LOO.results.norm[[1]][[1]][5],LOO.results.norm[[1]][[1]][6]),
      x=exp(qVals),col="black",lwd=3)

invisible(lapply(LOO.results.norm[-1],FUN=function(resList){
  modelPars <- resList[[1]][5:6]
  #print(modelPars)
  qVals <- qnorm(seq(.001,.999,length=200),modelPars[1],modelPars[2])
  #print(qVals)
  lines(y=dnorm(qVals,modelPars[1],modelPars[2]),
        x=exp(qVals),col=do.call(rgb,c(as.list(col2rgb("cyan")[,1]),maxColorValue=255,alpha=.5*255)))
  invisible()
}))
rug(side=1,x=testData$responses,lwd=1,col=rgb(0,0,0,.25))
mtext(side=1,outer=TRUE,text="Distribution: norm",line=-1,adj=0)
#hc5 <- exp(qnorm(.05,LOO.results.norm[[1]][[1]][5],LOO.results.norm[[1]][[1]][6]))
#mtext(side=3,at=hc5,text=expression(HC[5]),adj=0,las=2)
#mtext(side=3,at=hc5/3,text=expression(HC[5]/3),adj=0,las=2)
#mtext(side=3,at=hc5/5,text=expression(HC[5]/5),adj=0,las=2)
#abline(v=hc5/c(1,2,3,5),col="gray")
hcX <- exp(qnorm(HC.primary,LOO.results.norm[[1]][[1]][5],LOO.results.norm[[1]][[1]][6]))
mtext(side=3,at=hcX,text=bquote(HC[.(round(100*HC.primary))]),adj=1.1,las=2)
mtext(side=3,at=hcX/3,text=bquote(HC[.(round(100*HC.primary))]/3),adj=1.1,las=2)
mtext(side=3,at=hcX/5,text=bquote(HC[.(round(100*HC.primary))]/5),adj=1.1,las=2)
abline(v=hcX/c(1,2,3,5),col="gray")

