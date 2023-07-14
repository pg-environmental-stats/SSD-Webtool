SSDfitFUN <- function(
  pllData,
  speciesLabels,
  distName,
  parsONLY=FALSE,
  xlabString="",
  effectLevel=NULL,
  gridSize=50,
  titleString=NULL,
  italicFont=3,
  xlimVals=NULL,
  printSpeciesLabels=TRUE,
  showUpper=FALSE,
  roundTo=NULL,
  doPlots=TRUE,
  par1.LB=-Inf,
  par2.LB=-Inf,
  confLevel=0.95,
  logTransform=TRUE,
  startVals=NULL,
  quietTF=TRUE,
  inputList=NULL){

  #par(cex.lab=input$labelSize, cex.axis=input$axisSize, cex.main=1.2, cex.sub=1.2)

  if(is.element(distName,c("norm","logis","gumbel.evd","gompertz.fs")))par2.LB <- 1e-12
  if(is.element(distName,c("gamma","weibull")))par1.LB <- par2.LB <- 1e-12
  
  if(logTransform)pllData <- log10(pllData)
  #reorder
  newOrder <- order(pllData)
  pllData <- pllData[newOrder]
  speciesLabels <- speciesLabels[newOrder]

  dFUN <- get(x=paste("d",distName,sep=""),pos=1)
  qFUN <- get(x=paste("q",distName,sep=""),pos=1)
  pFUN <- get(x=paste("p",distName,sep=""),pos=1)

  #here, instead of survreg, etc, simply perform my own maximum likelihood, borrowing the rriskFitdist
  #function from package rriskDistributions (normal, logistic, weibull, gamma, etc are available)
  #the ONE restriction at the moment is that it must be a two-parameter distribution
  generalLL <- function(pars,x){
    sum(dFUN(x,pars[1],pars[2],log=TRUE))
  }
  print("Inside SSDfitFUN()")
  print(pllData)
  if( is.null(startVals))MLE.fit <- rriskFitdist.GJC(data=pllData,distr=distName)
  if(!is.null(startVals))MLE.fit <- rriskFitdist.GJC(data=pllData,distr=distName,start=startVals)
  print(MLE.fit)
  MLE.pars <- MLE.fit$estimate
  if(!quietTF){
    print(MLE.fit)
    print("MLE.pars")
    print(MLE.pars)
  }
  if(parsONLY)return(MLE.pars)
  if(!is.null(startVals))names(MLE.pars)<-names(formals(dFUN))[2:3]
  MLE.pars.sd <- MLE.fit$sd
  MLE.LL <- MLE.fit$loglik
  #CI.critval <- qchisq(confLevel,1)
  CI.critval <- qf(confLevel,1,MLE.fit$n)

  #this is going to be the biggest issue by this approach.  Should probably walk across the parameter space by
  #sd units one at a time (separately for each direction), until the convex hull of the contour is totally enclosed
  #ie, all edges of the parameter space rectangle are OUTSIDE the confidence region

  #the figure is also informative; this region is independent of ECx level chosen
  enclosedTF <- FALSE
  par1.down <- par1.up <- par2.down <- par2.up <- 2
  regionSize <- gridSize
  par1Small <- par2Small <- par1Large <- par2Large <- FALSE
  loopCt <- 0
  minPar1 <- max(par1.LB,MLE.pars[1]-par1.down*MLE.pars.sd[1])
  maxPar1 <- MLE.pars[1]+par1.up*MLE.pars.sd[1]
  minPar2 <- max(par2.LB,MLE.pars[2]-par2.down*MLE.pars.sd[2])
  maxPar2 <- MLE.pars[2]+par2.up*MLE.pars.sd[2]
  if(minPar1 == par1.LB)minPar1
  #if(strictPOS | FALSE){
  #  if(minPar1<0)minPar1 <- .1
  #  if(minPar2<0)minPar2 <- .1
  #}
  
  while(!enclosedTF){
    loopCt <- loopCt + 1
    if(minPar1==par1.LB)par1Vals<-seq(minPar1,maxPar1,length=regionSize+1)[-1]
    if(minPar1 >par1.LB)par1Vals<-seq(minPar1,maxPar1,length=regionSize)
    if(minPar2==par2.LB)par2Vals<-seq(minPar2,maxPar2,length=regionSize+1)[-1]
    if(minPar2 >par2.LB)par2Vals<-seq(minPar2,maxPar2,length=regionSize)
    # stay off of the bound values - code below will move it closer if necessary
    minPar1 <- par1Vals[1]
    minPar2 <- par2Vals[1]
    if(!quietTF){
      print(par1Vals)
      print(par2Vals)
    }
    #if(strictPOS | FALSE){
    #    par1Vals <- par1Vals[par1Vals>0]
    #    par2Vals <- par2Vals[par2Vals>0]
    #    #par2Vals <- c(par2Vals[1]/c(1000,300,100,30,10,3),par2Vals)
    #  }
     #if(par1Large)par1Vals <- c(par1Vals,max(par1Vals)*c(3,10,30,100,300,1000))
    #if(par2Large)par2Vals <- c(par2Vals,max(par2Vals)*c(3,10,30,100,300,1000))
    par1Size <- length(par1Vals)
    par2Size <- length(par2Vals)
    testGrid <- expand.grid(par1Vals,par2Vals)
    testGrid.LL <- matrix(2*(apply(testGrid,1,FUN=function(pars){generalLL(pars,pllData)})-MLE.LL),ncol=par1Size)
    testGrid.HC05 <- matrix(apply(testGrid,1,FUN=function(pars){qFUN(0.05,pars[1],pars[2])}),ncol=par1Size)
    gridDIM <- dim(testGrid.LL)
    #testGrid.LL <<- testGrid.LL
    if(!quietTF){
      print(c(par1Size=par1Size,par2Size=par2Size))
      print("gridDIM")
      print(gridDIM)
      print(par1Vals)
      print(par2Vals)
      print(apply(matrix(as.numeric(abs(testGrid.LL)<CI.critval),ncol=par1Size),
                  1,paste,collapse=" "))
    }
    edges <- c(par2low<-testGrid.LL[,1],par2high<-testGrid.LL[,par1Size],par1low<-testGrid.LL[1,],par1high<-testGrid.LL[par2Size,])
    if(!any(abs(edges)<CI.critval))enclosedTF <- TRUE
    if(any(abs(edges)<CI.critval)){
      if(any(abs(par2low )<CI.critval)){
        par2.down <- par2.down+0.5
        par2Small <- TRUE
        testPar2 <- MLE.pars[2]-par2.down*MLE.pars.sd[2]
        if(testPar2<=par2.LB)minPar2 <- (minPar2-par2.LB)/2
        if(testPar2 >par2.LB)minPar2 <- testPar2
      }
      if(any(abs(par2high)<CI.critval)){
        par2.up   <- par2.up  +0.5
        par2Large <- TRUE
        maxPar2 <- MLE.pars[2]+par2.up*MLE.pars.sd[2]
      }
      if(any(abs(par1low )<CI.critval)){
        par1.down <- par1.down+0.5
        par1Small <- TRUE
        testPar1 <- MLE.pars[1]-par1.down*MLE.pars.sd[1]
        if(testPar1<=par1.LB)minPar1 <- (minPar1-par2.LB)/2
        if(testPar1 >par1.LB)minPar1 <- testPar1
      }
      if(any(abs(par1high)<CI.critval)){
        par1.up   <- par1.up  +0.5
        par1Large <- TRUE
        maxPar1 <- MLE.pars[1]+par1.up*MLE.pars.sd[1]
      }
    }
    if(!quietTF){
      print(c(minPar1=minPar1,maxPar1=maxPar1,minPar2=minPar2,maxPar2=maxPar2))
      plot(x=testGrid[,1],y=testGrid[,2],xlab="",ylab="",
           bty="n",type="n",las=1)
      points(x=testGrid[,1],
             y=testGrid[,2])
      points(x=testGrid[abs(as.vector(testGrid.LL))<CI.critval,1],
             y=testGrid[abs(as.vector(testGrid.LL))<CI.critval,2],
             lwd=1,col="black",pch=16)
      mtext(side=1,outer=TRUE,line=-1.25,text = names(MLE.pars)[1],cex = 1.2,padj=0)
      mtext(side=2,outer=TRUE,line=-0.35,text = names(MLE.pars)[2],cex = 1.2,padj=1)
      mtext(side=1,outer=TRUE,text=paste("Distribution:",distName),line=-1,adj=0)
    }
    
    if(loopCt>10)break
  }
  testGrid.LL.vec <- as.vector(testGrid.LL)
  #cLine is the bounding line (in x,y pairs) for the confidence region on the distribution
  #these points can then be querried to ask "what is the smallest ECx in this confidence space,
  #and what is the largest, to translate the region to limits on the desired ECx.
  cLine <- contourLines(par1Vals,par2Vals,abs(testGrid.LL),levels=CI.critval)[[1]]

  #columns are levels of par2, rows are levels of par1
  if(FALSE)matrix(testGrid.LL,nrow=length(par1Vals),ncol=length(par1Vals))

  if(doPlots){
    #####################################################################################
    #####################################################################################
    #####################################################################################
    cexAXIS <- 1
    cexLAB <- 1
    cexLWD <- 2
    cexPCH <- ifelse(cexLWD<1,1,cexLWD*.75)
    print(c(cexAXIS=cexAXIS,cexLAB=cexLAB,cexLWD=cexLWD))
    plot(x=testGrid[,1],y=testGrid[,2],xlab="",ylab="",
         bty="n",type="n",las=1,axes=FALSE,
         cex.lab=cexLAB,cex.axis=cexAXIS,lwd=cexLWD)
    axis(side=1,las=1,
         cex.axis=cexAXIS,
         cex.lab=cexLAB,
         lwd=cexLWD)
    axis(side=2,las=1,
         cex.axis=cexAXIS,
         cex.lab=cexLAB,
         lwd=cexLWD)

    points(x=testGrid[,1],
           y=testGrid[,2])
    points(x=testGrid[abs(testGrid.LL.vec)<CI.critval,1],
           y=testGrid[abs(testGrid.LL.vec)<CI.critval,2],
           lwd=1,col="black",pch=16)
    lines(x=cLine$x,y=cLine$y,col="cyan",lwd=1.5*cexLWD)
    mtext(side=1,outer=TRUE,line=-1.25,text = names(MLE.pars)[1],cex = cexLAB,padj=0)
    mtext(side=2,outer=TRUE,line=-0.35,text = names(MLE.pars)[2],cex = cexLAB,padj=1)
    mtext(side=1,outer=TRUE,text=paste("Distribution:",distName),line=-1,adj=0)
    #####################################################################################
    #####################################################################################
    #####################################################################################

    #library(rgl)
    #open3d()
    #baseRadius <- max(diff(range(testGrid[,1])),diff(range(testGrid[,2])),diff(range(as.vector(testGrid.HC05))))/200
    #maxPoint <- which.max(as.vector(testGrid.HC05)[abs(testGrid.LL.vec)<CI.critval])
    #minPoint <- which.min(as.vector(testGrid.HC05)[abs(testGrid.LL.vec)<CI.critval])
    #spheres3d(x=testGrid[abs(testGrid.LL.vec)<CI.critval,1],y=testGrid[abs(testGrid.LL.vec)<CI.critval,2],z=as.vector(testGrid.HC05)[abs(testGrid.LL.vec)<CI.critval],col="blue",radius=baseRadius)
    #spheres3d(x=testGrid[abs(testGrid.LL.vec)>CI.critval,1],y=testGrid[abs(testGrid.LL.vec)>CI.critval,2],z=as.vector(testGrid.HC05)[abs(testGrid.LL.vec)>CI.critval],alpha=0.2,radius=baseRadius)
    #spheres3d(x=testGrid[abs(testGrid.LL.vec)<CI.critval,1][maxPoint],y=testGrid[abs(testGrid.LL.vec)<CI.critval,2][maxPoint],z=as.vector(testGrid.HC05)[abs(testGrid.LL.vec)<CI.critval][maxPoint],col="red",radius=baseRadius*1.5)
    #spheres3d(x=testGrid[abs(testGrid.LL.vec)<CI.critval,1][minPoint],y=testGrid[abs(testGrid.LL.vec)<CI.critval,2][minPoint],z=as.vector(testGrid.HC05)[abs(testGrid.LL.vec)<CI.critval][minPoint],col="red",radius=baseRadius*1.5)

    #axes3d(labels=TRUE)
    #aspect3d(1,1,1)

    #open3d()
    #surface3d(par1Vals,par2Vals,testGrid.LL,color="red",alpha=0.3)
    #baseRadius <- max(diff(range(testGrid[,1])),diff(range(testGrid[,2])),diff(range(testGrid.LL.vec)))/200
    #spheres3d(x=testGrid[abs(testGrid.LL.vec)<CI.critval,1],y=testGrid[abs(testGrid.LL.vec)<CI.critval,2],z=testGrid.LL.vec[abs(testGrid.LL.vec)<CI.critval],col="blue",radius=2*baseRadius)
    #spheres3d(x=testGrid[abs(testGrid.LL.vec)>CI.critval,1],y=testGrid[abs(testGrid.LL.vec)>CI.critval,2],z=testGrid.LL.vec[abs(testGrid.LL.vec)>CI.critval],radius=baseRadius)
    #spheres3d(x=MLE.pars[1],y=MLE.pars[2],z=0,col="red",radius=baseRadius*3)
    #lines3d(cLine$x,cLine$y,rep(-CI.critval,length(cLine$x)))
    #axes3d(labels=TRUE)
    #aspect3d(1,1,1)
  }


  #the interval, for a range of quantiles so confidence bands can be plotted
  #10^do.call(qFUN,as.list(c(p=.05,MLE.pars)))
  #10^qFUN(.05,MLE.pars[1],MLE.pars[2])
  pVals <- seq(.001,.999,by=.001)
  if( logTransform)ciVals <- t(sapply(pVals,FUN=function(ppp)10^range(apply(cbind(cLine$x,cLine$y),1,FUN=function(pars)do.call(qFUN,as.list(c(p=ppp,pars)))))))
  if(!logTransform)ciVals <- t(sapply(pVals,FUN=function(ppp)   range(apply(cbind(cLine$x,cLine$y),1,FUN=function(pars)do.call(qFUN,as.list(c(p=ppp,pars)))))))
  #ciVals[pVals==0.05,]
  if(!quietTF){
    print("head(ciVals)")
    print(head(ciVals))
  }

  #if(doPlots){
  #require(ADGofTest)
  #print(ad.test(pllData,distr=pFUN,MLE.pars[1],MLE.pars[2]))
  #}
  AD.pvalue <- ADGofTest::ad.test(pllData,distr=pFUN,MLE.pars[1],MLE.pars[2])$p.value
  if(!quietTF)print(MLE.pars)
  if( logTransform)modelVals <- 10^do.call(qFUN,c(list(p=pVals),as.list(MLE.pars)))
  if(!logTransform)modelVals <-    do.call(qFUN,c(list(p=pVals),as.list(MLE.pars)))
  if(is.null(xlimVals)){
    if( logTransform)xlims <- range(log10(modelVals))
    if(!logTransform)xlims <- range(     (modelVals))
    xlims[1] <- floor(na.omit(c(min(xlims[1],pllData))))
    xlims[2] <-  ceiling(na.omit(c(max(xlims[2],pllData))))
    if( logTransform){
      xlims[1]<-floor(xlims[1])
      xlims[2]<-ceiling(xlims[2])
      xlims <- 10^xlims
    }
  }
  if(!is.null(xlimVals))xlims <- xlimVals
  if(!quietTF){
    print("xlim Vals")
    print(rbind(
    c(xlims=xlims),
    c(range=10^range(pllData))
  ))
  }
  if(!quietTF){
    print("HC05 input parms")
    c(list(p=effectLevel),as.list(MLE.pars))
  }
  if( logTransform)HC05All <- 10^do.call(qFUN,c(list(p=effectLevel),as.list(MLE.pars)))
  if(!logTransform)HC05All <-    do.call(qFUN,c(list(p=effectLevel),as.list(MLE.pars)))
  if(!quietTF){
    print("HC05All")
    print(HC05All)
  }
  LCL.HC05All <- ciVals[pVals==effectLevel,1]
  UCL.HC05All <- ciVals[pVals==effectLevel,2]
  if(!quietTF){
    print("LCL.HC05All")
    print(LCL.HC05All)
  }
  print(LCL.HC05All)
  #find params that go with ci endpoints
  ciParms05 <- cbind(cLine$x,cLine$y,apply(cbind(cLine$x,cLine$y),1,FUN=function(pars)do.call(qFUN,as.list(c(p=effectLevel,pars)))))
  ciParms05.LCL <- ciParms05[which.min(ciParms05[,3]),1:2]
  ciParms05.UCL <- ciParms05[which.max(ciParms05[,3]),1:2]
  if(!quietTF){
    print("rbind(ciParms05.LCL,ciParms05.UCL)")
    print(rbind(ciParms05.LCL,ciParms05.UCL))
  }



  if(doPlots){

    ### why is this here?  It's before next plot is started
    if(logTransform & FALSE){
      majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
      axis(side=1,at=majorTicks,label=majorTicks)
      axis(side=1,at=10^as.vector(sapply(ceiling(par("usr")[1]):(ceiling(par("usr")[2])-1),FUN=function(x)x+log10(1:9))),
           tck=-.01,labels=F)
    }


    #points(x=(pllData),y=1 - 1/(1 + predictedValues),col=8,pch=16)
    if(!is.null(roundTo))CItexts <- format(round(c(LCL.HC05All,HC05All,UCL.HC05All),roundTo))
    if(is.null(roundTo))CItexts <- sapply(signif(c(LCL.HC05All,HC05All,UCL.HC05All),3),format,digits=3)
    xVals <- seq(par("usr")[1],par("usr")[2],length=1000)
    ylimMax <- max(c(
      dFUN(xVals,ciParms05.LCL[1],ciParms05.LCL[2]),
      dFUN(xVals,ciParms05.UCL[1],ciParms05.UCL[2]),
      dFUN(xVals,MLE.pars[1],MLE.pars[2])))

    #####################################################################################
    #####################################################################################
    #####################################################################################
    par(
      mai=c(
        8*.15,
        8*.15,
        0,
        8*0.2)+0.1,
      omi=rep(0,4))
    if( logTransform)plot(x=modelVals,y=pVals,ylim=c(0,1),xlim=xlims,
                          log='x',
                          type='n',
                          xlab="",
                          ylab="",
                          axes=FALSE,main=titleString)
    if(!logTransform)plot(x=modelVals,y=pVals,ylim=c(0,1),xlim=xlims,
                          type='n',bty="n",
                          xlab="",
                          ylab="",
                          axes=FALSE,main=titleString)
    xlabBuild <- "Effect Values (units)"
    mtext(side=1,outer=TRUE,line=-1.25,text = xlabBuild,cex = cexLAB,padj=0)
    mtext(side=2,outer=TRUE,line=-0.35,text = "Probability",cex = cexLAB,padj=1)
    mtext(side=1,outer=TRUE,text=paste("Distribution:",distName),line=-1,adj=0)
    axis(side=2,las=1,
         cex.axis=cexAXIS,
         cex.lab=cexLAB,
         lwd=cexLWD)
    if(logTransform){
      majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
      axis(side=1,at=majorTicks,label=majorTicks,
           cex.axis=cexAXIS,
           cex.lab=cexLAB,
           lwd=cexLWD)
      axis(side=1,at=10^as.vector(sapply(ceiling(par("usr")[1]):(ceiling(par("usr")[2])-1),FUN=function(x)x+log10(1:9))),
           tck=-.01,labels=FALSE,
           cex.axis=cexAXIS,
           cex.lab=cexLAB,
           lwd=cexLWD/2)
    }
    if(!logTransform)axis(side=1,
                          cex.axis=cexAXIS,
                          cex.lab=cexLAB,
                          lwd=cexLWD)
    #      mtext(side=1,expression(paste("Concentration (",mu,"g/L)")),line=3)
    if( logTransform)points(x=10^pllData,y=order(pllData)/(max(order(pllData))+1),col='gray',pch=16,cex=cexPCH)
    if(!logTransform)points(x=   pllData,y=order(pllData)/(max(order(pllData))+1),col='gray',pch=16,cex=cexPCH)

    lines(x=ciVals[,1],y=pVals,col="cyan",lwd=cexLWD)
    lines(x=ciVals[,2],y=pVals,col="cyan",lwd=cexLWD)
    lines(x=modelVals,y=pVals,col="blue",lwd=cexLWD*1.5)
    if(!quietTF & logTransform){
      lines(x=10^qFUN(pVals,ciParms05.LCL[1],ciParms05.LCL[2]),y=pVals,col="cyan")
      lines(x=10^qFUN(pVals,ciParms05.UCL[1],ciParms05.UCL[2]),y=pVals,col="cyan")
    }

    #predictedValues <- exp((log(pllData) - interceptParam)/scaleParam)
    if(printSpeciesLabels){
      mtext(side=4,
            at=order(pllData)/(max(order(pllData))+1),
            text=speciesLabels,
            las=2,
            family="serif",font = 3,cex=0.5)
    }
    #points(x=(pllData),y=1 - 1/(1 + predictedValues),col=8,pch=16)
    if(!showUpper)abline(h=effectLevel,v=c(LCL.HC05All,HC05All),col='gray')
    if(showUpper)abline(h=effectLevel,v=c(LCL.HC05All,HC05All,UCL.HC05All),col='gray')
    if(!is.null(roundTo))CItexts <- format(round(c(LCL.HC05All,HC05All,UCL.HC05All),roundTo))
    if(is.null(roundTo))CItexts <- sapply(signif(c(LCL.HC05All,HC05All,UCL.HC05All),3),format,digits=3)
    #print(CItexts)
    mtext(side=3,at=HC05All*.85,text=bquote(HC[.(round(100*effectLevel))]==.(CItexts[2])),adj=1,las=2,line=-.1)
    mtext(side=3,at=LCL.HC05All*.85,text=bquote(LCL==.(CItexts[1])),adj=1,las=2,line=-.1)
    if(showUpper)mtext(side=3,at=UCL.HC05All*.85,text=bquote(UCL==.(CItexts[3])),adj=1,las=2,line=-.1)
    #####################################################################################
    #####################################################################################
    #####################################################################################
    if(!logTransform & is.element(distName,c("weibull","gamma","gompertz.fs"))){
      par(
        mai=c(
          8*.15,
          8*.15,
          0,
          8*0.2)+0.1,
        omi=rep(0,4))
      xlims <- range(c(LCL.HC05All,pllData))
      xlims[1] <- 10^floor(log10(xlims[1]))
      xlims[2] <- 10^ceiling(log10(xlims[2]))
      plot(x=xlims,y=0*xlims,ylim=c(0,1),xlim=xlims,
                            log='x',
                            type='n',
                            xlab="",
                            ylab="",
                            axes=FALSE,main=titleString)
      xlabBuild <- "Effect Values (units)"
      mtext(side=1,outer=TRUE,line=-1.25,text = xlabBuild,cex = cexLAB,padj=0)
      mtext(side=2,outer=TRUE,line=-0.35,text = "Probability",cex = cexLAB,padj=1)
      mtext(side=1,outer=TRUE,text=paste("Distribution:",distName),line=-1,adj=0)
      axis(side=2,las=1,
           cex.axis=cexAXIS,
           cex.lab=cexLAB,
           lwd=cexLWD)
      if(logTransform){
        majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
        axis(side=1,at=majorTicks,label=majorTicks,
             cex.axis=cexAXIS,
             cex.lab=cexLAB,
             lwd=cexLWD)
        axis(side=1,at=10^as.vector(sapply(ceiling(par("usr")[1]):(ceiling(par("usr")[2])-1),FUN=function(x)x+log10(1:9))),
             tck=-.01,labels=FALSE,
             cex.axis=cexAXIS,
             cex.lab=cexLAB,
             lwd=cexLWD/2)
      }
      if(!logTransform)axis(side=1,
                            cex.axis=cexAXIS,
                            cex.lab=cexLAB,
                            lwd=cexLWD)
      #      mtext(side=1,expression(paste("Concentration (",mu,"g/L)")),line=3)
      if( logTransform)points(x=10^pllData,y=order(pllData)/(max(order(pllData))+1),col='gray',pch=16,cex=cexPCH)
      if(!logTransform)points(x=   pllData,y=order(pllData)/(max(order(pllData))+1),col='gray',pch=16,cex=cexPCH)
      
      lines(x=ciVals[,1],y=pVals,col="cyan",lwd=cexLWD)
      lines(x=ciVals[,2],y=pVals,col="cyan",lwd=cexLWD)
      lines(x=modelVals,y=pVals,col="blue",lwd=cexLWD*1.5)
      if(!quietTF & logTransform){
        lines(x=10^qFUN(pVals,ciParms05.LCL[1],ciParms05.LCL[2]),y=pVals,col="cyan")
        lines(x=10^qFUN(pVals,ciParms05.UCL[1],ciParms05.UCL[2]),y=pVals,col="cyan")
      }
      
      #predictedValues <- exp((log(pllData) - interceptParam)/scaleParam)
      if(printSpeciesLabels){
        mtext(side=4,
              at=order(pllData)/(max(order(pllData))+1),
              text=speciesLabels,
              las=2,
              family="serif",font = 3,cex=0.5)
      }
      #points(x=(pllData),y=1 - 1/(1 + predictedValues),col=8,pch=16)
      if(!showUpper)abline(h=effectLevel,v=c(LCL.HC05All,HC05All),col='gray')
      if(showUpper)abline(h=effectLevel,v=c(LCL.HC05All,HC05All,UCL.HC05All),col='gray')
      if(!is.null(roundTo))CItexts <- format(round(c(LCL.HC05All,HC05All,UCL.HC05All),roundTo))
      if(is.null(roundTo))CItexts <- sapply(signif(c(LCL.HC05All,HC05All,UCL.HC05All),3),format,digits=3)
      #print(CItexts)
      mtext(side=3,at=HC05All*.85,text=bquote(HC[.(round(100*effectLevel))]==.(CItexts[2])),adj=1,las=2,line=-.1)
      mtext(side=3,at=LCL.HC05All*.85,text=bquote(LCL==.(CItexts[1])),adj=1,las=2,line=-.1)
      if(showUpper)mtext(side=3,at=UCL.HC05All*.85,text=bquote(UCL==.(CItexts[3])),adj=1,las=2,line=-.1)
    }
    


  }
  result <- c(X=effectLevel,HCX=HC05All,LowerCL=LCL.HC05All,UpperCL=UCL.HC05All,MLE.pars,AD.pvalue)
  names(result)[2]<-paste("HC",100*effectLevel,sep="")
  result


  result <- list(fit=result,
       fitLines=cbind(pVals,ciVals,modelVals),
       distPars=data.frame(
         set=c("Fit","LCL","UCL"),
         location=c(MLE.pars[1],ciParms05.LCL[1],ciParms05.UCL[1]),
         scale=c(MLE.pars[2],ciParms05.LCL[2],ciParms05.UCL[2]),
         target=c(HC05All,LCL.HC05All,UCL.HC05All),
         density=c(
           dFUN(log(HC05All),MLE.pars[1],MLE.pars[2]),
           dFUN(log(LCL.HC05All),ciParms05.LCL[1],ciParms05.LCL[2]),
           dFUN(log(UCL.HC05All),ciParms05.UCL[1],ciParms05.UCL[2])
         )
       ),
       distName=distName,
       LL=MLE.LL)
  result
}









genericPLL.AddOne <- function(
  pllData,
  speciesLabels,
  distName,
  effectLevel=0.05,
  gridSize=50,
  titleString=NULL,
  italicFont=2,
  xlimVals=NULL,
  xlabString="",
  showUpper=FALSE,
  roundTo=0,
  doPlots=FALSE,
  par1.LB=-Inf,
  par2.LB=-Inf,
  confLevel=0.95,
  logTransform=TRUE,
  startVals=NULL,
  quietTF=TRUE){

  if(logTransform)pllData <- log(pllData)
  #reorder
  newOrder <- order(pllData)
  pllData <- pllData[newOrder]
  speciesLabels <- speciesLabels[newOrder]

  dFUN <- get(x=paste("d",distName,sep=""),pos=1)
  qFUN <- get(x=paste("q",distName,sep=""),pos=1)
  pFUN <- get(x=paste("p",distName,sep=""),pos=1)

  #here, instead of survreg, etc, simply perform my own maximum likelihood, borrowing the rriskFitdist
  #function from package rriskDistributions (normal, logistic, weibull, gamma, etc are available)
  #the ONE restriction at the moment is that it must be a two-parameter distribution
  generalLL <- function(pars,x){
    sum(dFUN(x,pars[1],pars[2],log=TRUE))
  }
  if( is.null(startVals))MLE.fit <- rriskFitdist.GJC(data=pllData,distr=distName)
  if(!is.null(startVals))MLE.fit <- rriskFitdist.GJC(data=pllData,distr=distName,start=startVals)
  MLE.pars <- MLE.fit$estimate
  if(!is.null(startVals))names(MLE.pars)<-names(formals(dFUN))[2:3]
  MLE.pars.sd <- MLE.fit$sd
  MLE.LL <- MLE.fit$loglik
  #CI.critval <- qchisq(confLevel,1)
  CI.critval <- qf(confLevel,1,MLE.fit$n)

  #this is going to be the biggest issue by this approach.  Should probably walk across the parameter space by
  #sd units one at a time (separately for each direction), until the convex hull of the contour is totally enclosed
  #ie, all edges of the parameter space rectangle are OUTSIDE the confidence region

  #the figure is also informative; this region is independent of ECx level chosen
  enclosedTF <- FALSE
  par1.down <- par1.up <- par2.down <- par2.up <- 1
  regionSize <- gridSize
  while(!enclosedTF){
    testGrid <- expand.grid(
      par1Vals<-seq(max(par1.LB,MLE.pars[1]-par1.down*MLE.pars.sd[1]),MLE.pars[1]+par1.up*MLE.pars.sd[1],length=regionSize),
      par2Vals<-seq(max(par2.LB,MLE.pars[2]-par2.down*MLE.pars.sd[2]),MLE.pars[2]+par2.up*MLE.pars.sd[2],length=regionSize))
    testGrid.LL <- matrix(2*(apply(testGrid,1,FUN=function(pars){generalLL(pars,pllData)})-MLE.LL),ncol=regionSize)
    testGrid.HC05 <- matrix(apply(testGrid,1,FUN=function(pars){qFUN(0.05,pars[1],pars[2])}),ncol=regionSize)
    edges <- c(par2low<-testGrid.LL[,1],par2high<-testGrid.LL[,regionSize],par1low<-testGrid.LL[1,],par1high<-testGrid.LL[regionSize,])
    if(!any(abs(edges)<CI.critval))enclosedTF <- TRUE
    if(any(abs(edges)<CI.critval)){
      if(any(abs(par2low )<CI.critval))par2.down <- par2.down+0.5
      if(any(abs(par2high)<CI.critval))par2.up   <- par2.up  +0.5
      if(any(abs(par1low )<CI.critval))par1.down <- par1.down+0.5
      if(any(abs(par1high)<CI.critval))par1.up   <- par1.up  +0.5
    }
    if(!quietTF)print(c(par1.down=par1.down,par1.up=par1.up,par2.down=par2.down,par2.up=par2.up))
  }
  testGrid.LL.vec <- as.vector(testGrid.LL)
  #cLine is the bounding line (in x,y pairs) for the confidence region on the distribution
  #these points can then be querried to ask "what is the smallest ECx in this confidence space,
  #and what is the largest, to translate the region to limits on the desired ECx.
  cLine <- contourLines(par1Vals,par2Vals,abs(testGrid.LL),levels=CI.critval)[[1]]

  #columns are levels of par2, rows are levels of par1
  if(FALSE)matrix(testGrid.LL,nrow=length(par1Vals),ncol=length(par1Vals))

  if(doPlots | FALSE){
    par(omi=c(0,0,0,0))

    plot(x=testGrid[,1],y=testGrid[,2],xlab=names(MLE.pars)[1],ylab=names(MLE.pars)[2])
    points(x=testGrid[abs(testGrid.LL.vec)<CI.critval,1],y=testGrid[abs(testGrid.LL.vec)<CI.critval,2],col="blue",pch=16)
    lines(x=cLine$x,y=cLine$y,col="cyan",lwd=3)
    mtext(side=1,outer=TRUE,text=paste("Distribution:",distName),line=-1,adj=0)

    #library(rgl)
    open3d()
    baseRadius <- max(diff(range(testGrid[,1])),diff(range(testGrid[,2])),diff(range(as.vector(testGrid.HC05))))/200
    maxPoint <- which.max(as.vector(testGrid.HC05)[abs(testGrid.LL.vec)<CI.critval])
    minPoint <- which.min(as.vector(testGrid.HC05)[abs(testGrid.LL.vec)<CI.critval])
    spheres3d(x=testGrid[abs(testGrid.LL.vec)<CI.critval,1],y=testGrid[abs(testGrid.LL.vec)<CI.critval,2],z=as.vector(testGrid.HC05)[abs(testGrid.LL.vec)<CI.critval],col="blue",radius=baseRadius)
    spheres3d(x=testGrid[abs(testGrid.LL.vec)>CI.critval,1],y=testGrid[abs(testGrid.LL.vec)>CI.critval,2],z=as.vector(testGrid.HC05)[abs(testGrid.LL.vec)>CI.critval],alpha=0.2,radius=baseRadius)
    spheres3d(x=testGrid[abs(testGrid.LL.vec)<CI.critval,1][maxPoint],y=testGrid[abs(testGrid.LL.vec)<CI.critval,2][maxPoint],z=as.vector(testGrid.HC05)[abs(testGrid.LL.vec)<CI.critval][maxPoint],col="red",radius=baseRadius*1.5)
    spheres3d(x=testGrid[abs(testGrid.LL.vec)<CI.critval,1][minPoint],y=testGrid[abs(testGrid.LL.vec)<CI.critval,2][minPoint],z=as.vector(testGrid.HC05)[abs(testGrid.LL.vec)<CI.critval][minPoint],col="red",radius=baseRadius*1.5)

    axes3d(labels=TRUE)
    aspect3d(1,1,1)

    open3d()
    surface3d(par1Vals,par2Vals,testGrid.LL,color="red",alpha=0.3)
    baseRadius <- max(diff(range(testGrid[,1])),diff(range(testGrid[,2])),diff(range(testGrid.LL.vec)))/200
    spheres3d(x=testGrid[abs(testGrid.LL.vec)<CI.critval,1],y=testGrid[abs(testGrid.LL.vec)<CI.critval,2],z=testGrid.LL.vec[abs(testGrid.LL.vec)<CI.critval],col="blue",radius=2*baseRadius)
    spheres3d(x=testGrid[abs(testGrid.LL.vec)>CI.critval,1],y=testGrid[abs(testGrid.LL.vec)>CI.critval,2],z=testGrid.LL.vec[abs(testGrid.LL.vec)>CI.critval],radius=baseRadius)
    spheres3d(x=MLE.pars[1],y=MLE.pars[2],z=0,col="red",radius=baseRadius*3)
    lines3d(cLine$x,cLine$y,rep(-CI.critval,length(cLine$x)))
    axes3d(labels=TRUE)
    aspect3d(1,1,1)
  }


  #the interval, for a range of quantiles so confidence bands can be plotted
  #10^do.call(qFUN,as.list(c(p=.05,MLE.pars)))
  #10^qFUN(.05,MLE.pars[1],MLE.pars[2])
  pVals <- seq(.001,.999,by=.001)
  #pVals <- seq(.01,.99,by=.01)
  if( logTransform)ciVals <- t(sapply(pVals,FUN=function(ppp)exp(range(apply(cbind(cLine$x,cLine$y),1,FUN=function(pars)do.call(qFUN,as.list(c(p=ppp,pars))))))))
  if(!logTransform)ciVals <- t(sapply(pVals,FUN=function(ppp)   range(apply(cbind(cLine$x,cLine$y),1,FUN=function(pars)do.call(qFUN,as.list(c(p=ppp,pars)))))))
  #ciVals[pVals==0.05,]
  if(!quietTF)print(head(ciVals))

  if(doPlots){
    require(ADGofTest)
    print(ad.test(pllData,distr=pFUN,MLE.pars[1],MLE.pars[2]))
  }
  if(!quietTF)print(MLE.pars)
  if( logTransform)modelVals <- exp(do.call(qFUN,c(list(p=pVals),as.list(MLE.pars))))
  if(!logTransform)modelVals <-    do.call(qFUN,c(list(p=pVals),as.list(MLE.pars)))
  if(is.null(xlimVals)){
    if( logTransform)xlims <- range(log(modelVals))
    if(!logTransform)xlims <- range(   (modelVals))
    xlims[1] <- floor(na.omit(c(min(xlims[1],pllData))))
    xlims[2] <-  ceiling(na.omit(c(max(xlims[2],pllData))))
    if( logTransform)xlims <- exp(xlims)
  }
  if(!is.null(xlimVals))xlims <- xlimVals

  if( logTransform)HC05All <- exp(do.call(qFUN,c(list(p=effectLevel),as.list(MLE.pars))))
  if(!logTransform)HC05All <-    do.call(qFUN,c(list(p=effectLevel),as.list(MLE.pars)))
  LCL.HC05All <- ciVals[pVals==effectLevel,1]
  UCL.HC05All <- ciVals[pVals==effectLevel,2]
  #find params that go with ci endpoints
  ciParms05 <- cbind(cLine$x,cLine$y,apply(cbind(cLine$x,cLine$y),1,FUN=function(pars)do.call(qFUN,as.list(c(p=effectLevel,pars)))))
  ciParms05.LCL <- ciParms05[which.min(ciParms05[,3]),1:2]
  ciParms05.UCL <- ciParms05[which.max(ciParms05[,3]),1:2]
  if(!quietTF)print(rbind(ciParms05.LCL,ciParms05.UCL))



  if(doPlots){
    par(omi=c(0,0,0.5,2.5))

    if( logTransform)plot(x=modelVals,y=pVals,ylim=c(0,1),log='x',
                          type='n',xlab=xlabString,ylab="Probability",axes=F,main=titleString,xlim=xlims)
    if(!logTransform)plot(x=modelVals,y=pVals,ylim=c(0,1),
                          type='n',xlab=xlabString,ylab="Probability",axes=F,main=titleString,xlim=xlims)
    mtext(side=1,outer=TRUE,text=paste("Distribution:",distName),line=-1,adj=0)
    axis(side=2)
    if(logTransform){
      majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
      axis(side=1,at=majorTicks,label=majorTicks)
      axis(side=1,at=10^as.vector(sapply(ceiling(par("usr")[1]):(ceiling(par("usr")[2])-1),FUN=function(x)x+log10(1:9))),
           tck=-.01,labels=F)
    }
    if(!logTransform)axis(side=1)
    box()
    #      mtext(side=1,expression(paste("Concentration (",mu,"g/L)")),line=3)
    if( logTransform)points(x=exp(pllData),y=order(pllData)/(max(order(pllData))+1),col='gray',pch=16)
    if(!logTransform)points(x=   pllData,y=order(pllData)/(max(order(pllData))+1),col='gray',pch=16)

    lines(x=ciVals[,1],y=pVals,col="blue",lwd=3)
    lines(x=ciVals[,2],y=pVals,col="blue",lwd=3)
    lines(x=modelVals,y=pVals,lwd=3)

    if(!quietTF & logTransform){
      lines(x=exp(qFUN(pVals,ciParms05.LCL[1],ciParms05.LCL[2])),y=pVals,col="cyan")
      lines(x=exp(qFUN(pVals,ciParms05.UCL[1],ciParms05.UCL[2])),y=pVals,col="cyan")
    }

    #predictedValues <- exp((log(pllData) - interceptParam)/scaleParam)
    mtext(side=4,at=order(pllData)/(max(order(pllData))+1),text=speciesLabels,las=2,font=italicFont,cex=.7,line=1)
    #points(x=(pllData),y=1 - 1/(1 + predictedValues),col=8,pch=16)
    if(!showUpper)abline(h=effectLevel,v=c(LCL.HC05All,HC05All),col='gray')
    if(showUpper)abline(h=effectLevel,v=c(LCL.HC05All,HC05All,UCL.HC05All),col='gray')
    CItexts <- format(sapply(c(LCL.HC05All,HC05All,UCL.HC05All),signif,roundTo))
    mtext(side=3,at=HC05All*.85,text=bquote(HC[5]==.(CItexts[2])),adj=1,las=2,line=-.1)
    mtext(side=3,at=LCL.HC05All*.85,text=bquote(LCL==.(CItexts[1])),adj=1,las=2,line=-.1)
    if(showUpper)mtext(side=3,at=UCL.HC05All*.85,text=bquote(UCL==.(CItexts[3])),adj=1,las=2,line=-.1)
    par(omi=rep(0,4))

    #xVals comes from the plot range, which would always be log10
    xVals <- seq(par("usr")[1]-1,par("usr")[2],length=1000)
    ylimMax <- max(c(
      dFUN(log(10^xVals),ciParms05.LCL[1],ciParms05.LCL[2]),
      dFUN(log(10^xVals),ciParms05.UCL[1],ciParms05.UCL[2]),
      dFUN(log(10^xVals),MLE.pars[1],MLE.pars[2])))

    if( logTransform){
      plot(x=modelVals,y=rep(0,length(modelVals)),ylim=c(0,ylimMax),log='x',
           type='n',xlab=xlabString,ylab="Distribution Density",axes=F,main=titleString,xlim=xlims)
      majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
      axis(side=1,at=majorTicks,label=majorTicks)
      axis(side=1,at=10^as.vector(sapply(ceiling(par("usr")[1]):(ceiling(par("usr")[2])-1),FUN=function(x)x+log10(1:9))),
           tck=-.01,labels=F)
      axis(side=2)
      lines(y=dFUN(log(10^xVals),MLE.pars[1],MLE.pars[2]),x=10^(xVals),lwd=2)
      lines(y=dFUN(log(10^xVals),ciParms05.LCL[1],ciParms05.LCL[2]),x=10^(xVals),col="cyan",lwd=2)
      lines(y=dFUN(log(10^xVals),ciParms05.UCL[1],ciParms05.UCL[2]),x=10^(xVals),col="cyan",lwd=2,lty=2)
      rug(side=1,x=exp(pllData),lwd=2,col=rgb(0,0,0,.2))
      abline(v=HC05All,col='gray')
      abline(v=LCL.HC05All,col='cyan')
      if(showUpper)abline(v=UCL.HC05All,col='cyan',lty=2)
      points(x=HC05All,y=dFUN(log(HC05All),MLE.pars[1],MLE.pars[2]))
      points(x=LCL.HC05All,y=dFUN(log(LCL.HC05All),ciParms05.LCL[1],ciParms05.LCL[2]),col="cyan")
      points(x=UCL.HC05All,y=dFUN(log(UCL.HC05All),ciParms05.UCL[1],ciParms05.UCL[2]),col="cyan")
      CItexts <- format(sapply(c(LCL.HC05All,HC05All,UCL.HC05All),signif,roundTo))
      mtext(side=3,at=HC05All*.85,text=bquote(HC[5]==.(CItexts[2])),adj=1,las=2,line=-.1)
      mtext(side=3,at=LCL.HC05All*.85,text=bquote(LCL==.(CItexts[1])),adj=1,las=2,line=-.1)
      if(showUpper)mtext(side=3,at=UCL.HC05All*.85,text=bquote(UCL==.(CItexts[3])),adj=1,las=2,line=-.1)
      box()
    }


  }
  list(fit=c(X=effectLevel,HCx=HC05All,LowerCL=LCL.HC05All,UpperCL=UCL.HC05All,MLE.pars),
       CI.line=cbind(pVals,ciVals))
}







addOnePLL <- function(
  effectValues,
  speciesLabels,
  distName,
  dataTag,
  xlabString="AS Concentration (mg/L)",
  effectLevelA1=0.05,
  plotTF=FALSE,
  debugTF=FALSE){
  #climEnd is for built-in distributions (typically either "norm" or "logis")
  #distName is for the survreg call argument dist (typically either "lognormal" or "loglogistic", paired with climEnd)
  standardPlot <- function(caseData,speciesLabels,distName,climEnd,titleString=NULL,italicFont=2,
                           xlimVals="NULL",showUpper=FALSE,roundTo=NULL,unitStr=xlabString){
    #print(caseData)
    distributionFitPLL <- genericPLL.AddOne(caseData,speciesLabels,distName,logTransform=TRUE,effectLevel=effectLevelA1)
    #require(survival)
    #distributionFitPLL<-survreg(formula = Surv(caseData) ~ 1, na.action = na.exclude, dist = distName)
    #if(debugTF)print(distributionFitPLL)
    interceptParamAll <- interceptParam <- distributionFitPLL[[1]][5]
    scaleParamAll <- scaleParam <- distributionFitPLL[[1]][6]
    par(omi=c(0,0,0.5,2.5))
    probs <- distributionFitPLL[[2]][,1]
    qFUN <- get(paste("q",distName,sep=""))
    pFUN <- get(paste("p",distName,sep=""))


    #if(require(ADGofTest))print(ad.test(log(caseData),distr=pFUN,interceptParamAll,scaleParamAll))

    quantiles <- exp(qFUN(probs,interceptParamAll,scaleParamAll))
    #climTableCase <- get(paste("myCLTable",climEnd,length(caseData),sep="."),pos=1)
    #this to show all confidence limits
    percentileStats.p <- distributionFitPLL[[2]][,1]
    percentileStatsLCLall <- distributionFitPLL[[2]][,2]
    percentileStatsUCLall <- distributionFitPLL[[2]][,3]
    #this for specific HC05
    HCxAll<-distributionFitPLL[[1]][2]
    LCL.HCxAll <- distributionFitPLL[[1]][3]
    UCL.HCxAll <- distributionFitPLL[[1]][4]
    #if(debugTF)print(c(HCxAll,LCL.HCxAll,UCL.HCxAll))
    #print(range(quantiles))
    if(is.character(xlimVals)){
      xlims <- range(quantiles[probs>=.01 & probs<=0.99])
      xlims[1] <- floor(min(na.omit(log10(c(LCL.HCxAll,xlims[1],caseData)))))
      xlims[2] <-  ceiling(max(na.omit(log10(c(xlims[2],caseData)))))
      xlims <- 10^xlims
    }else
      xlims <- xlimVals
    #no matter what, the lower xlim must encompass the LCL:
    if(LCL.HCxAll<xlims[1])xlims[1]<-10^floor(log10(LCL.HCxAll))

    plot(x=quantiles,y=probs,ylim=c(0,1),log='x',
         type='n',xlab=unitStr,ylab="Probability",axes=F,main=titleString,xlim=xlims)
    axis(side=2)
    majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
    axis(side=1,at=majorTicks,label=majorTicks)
    box()
    #      mtext(side=1,expression(paste("Concentration (",mu,"g/L)")),line=3)
    axis(side=1,at=10^as.vector(sapply(ceiling(par("usr")[1]):(ceiling(par("usr")[2])-1),FUN=function(x)x+log10(1:9))),
         tck=-.01,labels=F)
    points(x=(caseData),y=order(caseData)/(max(order(caseData))+1),col='gray',pch=16)

    mySpline<-spline(x=log(percentileStatsLCLall),y=percentileStats.p)
    mySpline$x<-exp(mySpline$x)
    lines(mySpline,col="blue",lwd=3)
    mySpline<-spline(x=log(percentileStatsUCLall),y=percentileStats.p)
    mySpline$x<-exp(mySpline$x)
    lines(mySpline,col="blue",lwd=3)
    lines(x=(quantiles),y=probs,lwd=3)

    if(is.null(roundTo)){
      roundTo <- -(floor(log10(LCL.HCxAll))-2)
    }
    #predictedValues <- exp((log(caseData) - interceptParam)/scaleParam)
    mtext(side=4,at=order(caseData)/(max(order(caseData))+1),text=speciesLabels,las=2,font=italicFont,cex=.7,line=1)
    #points(x=(caseData),y=1 - 1/(1 + predictedValues),col=8,pch=16)
    if(!showUpper)abline(h=effectLevelA1,v=c(LCL.HCxAll,HCxAll),col='gray')
    if(showUpper)abline(h=effectLevelA1,v=c(LCL.HCxAll,HCxAll,UCL.HCxAll),col='gray')
    CItexts <- format(sapply(c(LCL.HCxAll,HCxAll,UCL.HCxAll),signif,roundTo))
    mtext(side=3,at=HCxAll*.85,text=bquote(HC[.(round(100*effectLevelA1))]==.(CItexts[2])),adj=1,las=2,line=-.1)
    mtext(side=3,at=LCL.HCxAll*.85,text=bquote(LCL==.(CItexts[1])),adj=1,las=2,line=-.1)
    if(showUpper)mtext(side=3,at=UCL.HCxAll*.85,text=bquote(UCL==.(CItexts[3])),adj=1,las=2,line=-.1)

    c(HCx=HCxAll,LowerCL=LCL.HCxAll,UpperCL=UCL.HCxAll)
  }


  #this program will assume that tox values are given in dose units (NOT log transformed), and that they
  #will be transformed by ln(), or log()

  #require(survival)
  caseData <- effectValues
  caseTaxa <- speciesLabels
  newOrder <- order(caseData)
  caseData <- caseData[newOrder]
  caseTaxa <- caseTaxa[newOrder]

  dataTable <- data.frame(caseData,caseTaxa)
  row.names(dataTable) <- 1:nrow(dataTable)

  qFUN <- get(paste("q",distName,sep=""))
  pFUN <- get(paste("p",distName,sep=""))


  distributionFit <- rriskFitdist.GJC(data=log(caseData),distr=distName)
  #distributionFit<-survreg(formula = Surv(caseData) ~ 1, na.action= na.exclude, dist = distName)
  interceptParamAll <- interceptParam <- distributionFit$estimate[1]
  scaleParamAll <- scaleParam <- distributionFit$estimate[2]
  HCxAll<-exp(qFUN(effectLevelA1,interceptParamAll,scaleParamAll))
  if(debugTF)print(distributionFit$estimate)
  if(debugTF)print(c(HCxAll=HCxAll))

  #solve for the value of a new observation that will shift the HC05 to a new target value
  #input candidate value is on log-scale.  When the point is added, we are looking for the HCx
  #value to shift lower.
  #These were tested because I had a bug in the code, where the second argument to qFUN in each
  #of these referred to distributionFitA1$estimate[2] rather than distributionFit$estimate[2],
  #where distributionFit is for the ORIGINAL data, whereas it is supposed to be changing in here
  funOpt.old<-function(addedPoint,CaseData,HCxTargetValue){
    #print(c(HCxgiven=HCxgiven))
    distributionFitA1<-rriskFitdist.GJC(log(c(exp(addedPoint),CaseData)), dist = distName)
    #log(predict(distributionFit,type="quantile",p=c(0.05),newdata=data.frame(1)))-HCxgiven
    #The HCx percentile, when adding the data value, should equal the target, eq HCx/2
    HCx.new <- qFUN(effectLevelA1,distributionFitA1$estimate[1],distributionFitA1$estimate[2])
    #print(c(HCx.new=HCx.new))
    HCx.new-HCxTargetValue
  }

  #different approach.  When the new point is added, it should result in a fit where the target HC level
  #is returned when we put in the target HCx value, eg, HCx/2
  funOpt<-function(addedPoint,CaseData,HCxTargetValue){
    #print(c(HCxgiven=HCxgiven))
    distributionFitA1<-rriskFitdist.GJC(log(c(exp(addedPoint),CaseData)), dist = distName)
    #log(predict(distributionFit,type="quantile",p=c(0.05),newdata=data.frame(1)))-HCxgiven
    #The HCx percentile, when adding the data value, should equal the target, eq HCx/2
    HCx.new <- pFUN(HCxTargetValue,distributionFitA1$estimate[1],distributionFitA1$estimate[2])
    #print(c(HCx.new=HCx.new))
    effectLevelA1-HCx.new
  }

  #this assumes that the added point will be less than the HC5 (upper=), but that is not always the case. An alternative is to take the
  #GREATER of the HC5, and the smallest data value.
  #upperValue <- log(max(HCxAll,min(caseData)))
  print(caseData)
  upperValue <- log(max(HCxAll,max(caseData)))
  #checks on interval -- should be opposite signs
  print(c(effectLevelA1=effectLevelA1,HCxAll=HCxAll))
  for(logVal2 in seq(5,200,by=5)){
    if(funOpt(log(HCxAll)-logVal2,caseData,log(HCxAll/2))<0)break
  }
  for(logVal3 in seq(5,200,by=5)){
    if(funOpt(log(HCxAll)-logVal3,caseData,log(HCxAll/3))<0)break
  }
  for(logVal5 in seq(5,200,by=5)){
    if(funOpt(log(HCxAll)-logVal5,caseData,log(HCxAll/5))<0)break
  }
  for(logVal10 in seq(5,200,by=5)){
    if(funOpt(log(HCxAll)-logVal10,caseData,log(HCxAll/10))<0)break
  }
  print(c(logVal2=logVal2,logVal3=logVal3,logVal5=logVal5,logVal10=logVal10))
  print(
    intervalChecks <- rbind(
      c(lower=try(funOpt(log(HCxAll)-logVal2,caseData,log(HCxAll/2))),upper=try(funOpt(upperValue,caseData,log(HCxAll/2)))),
      c(lower=try(funOpt(log(HCxAll)-logVal3,caseData,log(HCxAll/3))),upper=try(funOpt(upperValue,caseData,log(HCxAll/3)))),
      c(lower=try(funOpt(log(HCxAll)-logVal5,caseData,log(HCxAll/5))),upper=try(funOpt(upperValue,caseData,log(HCxAll/5)))),
      c(lower=try(funOpt(log(HCxAll)-logVal10,caseData,log(HCxAll/10))),upper=try(funOpt(upperValue,caseData,log(HCxAll/10))))
    )
  )
  if(!all(rowSums(sign(intervalChecks))==0))stop("Dataset may require add-one-in values too far from observed data.")
  addPoints <- c(
    HC5.2=try(uniroot(funOpt,lower=log(HCxAll)-logVal2,upper=upperValue,CaseData=caseData,HCxTargetValue=log(HCxAll/2),extendInt="no")$root),
    HC5.3=try(uniroot(funOpt,lower=log(HCxAll)-logVal3,upper=upperValue,CaseData=caseData,HCxTargetValue=log(HCxAll/3),extendInt="no")$root),
    HC5.5=try(uniroot(funOpt,lower=log(HCxAll)-logVal5,upper=upperValue,CaseData=caseData,HCxTargetValue=log(HCxAll/5),extendInt="no")$root),
    HC5.10=try(uniroot(funOpt,lower=log(HCxAll)-logVal10,upper=upperValue,CaseData=caseData,HCxTargetValue=log(HCxAll/10),extendInt="no")$root))

  print(addPoints)
  print(origPars <- rriskFitdist.GJC(log(c(caseData)), dist = distName)$estimate)
  print(pFUN(log(HCxAll),origPars[1],origPars[2]))
  print(newPars <- rriskFitdist.GJC(log(c(exp(addPoints[1]),caseData)), dist = distName)$estimate)
  print(pFUN(log(HCxAll/2),newPars[1],newPars[2]))
  percentageSTR <- round(100*effectLevelA1)
  percentageSTR <- formatC(percentageSTR,width=2,format="d",flag="0")
  addProbs <- pFUN(addPoints,interceptParamAll,scaleParamAll)
  names(addProbs) <- paste(paste("HC",percentageSTR,sep=""),c(2,3,5,10),sep=".")
  oneIn <- 1/addProbs
  #print(addPoints)
  #print(exp(addPoints))

  ### calculate a goodness of fit -- not used
  #ks.p <- stats::ks.test(x = (log(caseData)+seq(-1,1,along=caseData)/10000), y = "plogis", location = interceptParam, scale = scaleParam)$p.value
  ###define SSD curve
  probs <- c(1e-10,seq(.0001,.9999,by=.0001))
  #quantiles <- (predict(distributionFit,type="quantile",p=probs,newdata=data.frame(1)))
  quantiles <- exp(qFUN(probs,interceptParamAll,scaleParamAll))
  ###PLOT
  xlims <- range(log10(quantiles))
  xlims[1] <- floor(xlims[1])
  xlims[2] <-  ceiling(xlims[2])
  xlims <- 10^xlims
  if(plotTF){
    plot(x=quantiles,y=probs,ylim=c(0,1),log='x',type='n',
         xlab=xlabString,ylab="Probability",axes=F,xlim=xlims)
    #print(par("usr"))
    axis(side=2)
    majorTicks <- 10^seq(ceiling(par("usr")[1]),floor(par("usr")[2]))
    axis(side=1,at=majorTicks,label=majorTicks)
    box()
    #     mtext(side=1,expression(paste("Concentration (",mu,"g/L)")),line=3)
    axis(side=1,
         at=10^as.vector(sapply(ceiling(par("usr")[1]):(floor(par("usr")[2])-1),
                                FUN=function(x)x+log10(1:9))),
         tck=-.01,labels=F)

    points(x=(caseData),y=order(caseData)/(max(order(caseData))+1),col='red',pch=16)

    #climTableCase <- get(paste("myCLTable",climEnd,length(caseData),sep="."),pos=1)
    #percentileStatsLCLall <- exp(interceptParam - climTableCase[["lower_c"]] * scaleParam)
    #percentileStatsUCLall <- exp(interceptParam - climTableCase[["upper_c"]] * scaleParam)
    #if(climEnd=="logis")HC05<-qlogis(0.05,interceptParam,scaleParam)#exp(scaleParam*log(0.05/(1-0.05)) + interceptParam)
    #if(climEnd=="norm")HC05<-qnorm(0.05,interceptParam,scaleParam)
    #mySpline<-spline(x=log(percentileStatsLCLall),y=unique(climTable$p))
    #mySpline$x<-exp(mySpline$x)
    #lines(mySpline,col=2,lwd=3)
    #mySpline<-spline(x=log(percentileStatsUCLall),y=unique(climTable$p))
    #mySpline$x<-exp(mySpline$x)
    #lines(mySpline,col=2,lwd=3)
    lines(x=quantiles,y=probs,lwd=3)
    abline(h=effectLevelA1,v=c(HCxAll,HCxAll/2,HCxAll/3,HCxAll/5,HCxAll/10),col='gray')
    #HC05All<-HC05

    #for (newCase in exp(seq(log(.001),log(HC05All),length=10))){
    for (newCase in exp(addPoints)){
      newDistributionFit<-rriskFitdist.GJC(log(newCaseData<-c(newCase,caseData)), dist = distName)
      #distributionFit<-survreg(formula = Surv(newCaseData<-c(newCase,caseData)) ~ 1, na.action = na.exclude, dist = distName)
      newQuantiles <- exp(qFUN(probs,newDistributionFit$estimate[1],newDistributionFit$estimate[2]))
      #newQuantiles <- (predict(distributionFit,type="quantile",p=probs,newdata=data.frame(1)))
      interceptParam <- newDistributionFit$estimate[1]
      scaleParam <- newDistributionFit$estimate[2]
      ### calculate a goodness of fit -- not used
      #ks.p <- stats::ks.test(x = log(newCaseData)+seq(-1,1,along=newCaseData)/1000000, y = "plogis", location = interceptParam, scale = scaleParam)$p.value
      #print(c(ks.p=ks.p))
      ###define SSD curve
      #	allEC20 <- exp(seq(from=log(.01),to=log(100),length=100))
      #	logEC20All <- log(allEC20)
      #	expTerm <- exp((logEC20All - interceptParam)/scaleParam)
      #	estimatedCdfAll <- 1 - 1/(1 + expTerm)
      #	tableCDF <- scaleParam*log(unique(climTable$p)/(1-unique(climTable$p))) + interceptParam
      #	1 - 1/(1 + exp((tableCDF - interceptParam)/scaleParam))
      #	unique(climTable$p)
      #	ord<-order(newCaseData)
      #	phat<-ord/(max(ord)+1)


      #climTableCase <- get(paste("myCLTable",climEnd,length(newCaseData),sep="."),pos=1)
      #percentileStatsLCLall <- exp(interceptParam - climTableCase[["lower_c"]] * scaleParam)
      #percentileStatsUCLall <- exp(interceptParam - climTableCase[["upper_c"]] * scaleParam)
      #if(climEnd=="logis")HC05<-exp(scaleParam*log(0.05/(1-0.05)) + interceptParam)
      #if(climEnd=="norm")HC05<-qnorm(0.05,interceptParam,scaleParam)
      #mySpline<-spline(x=log(percentileStatsLCLall),y=unique(climTable$p))
      #mySpline$x<-exp(mySpline$x)
      #	lines(mySpline,col=2,lwd=.5)
      #mySpline<-spline(x=log(percentileStatsUCLall),y=unique(climTable$p))
      #mySpline$x<-exp(mySpline$x)
      #	lines(mySpline,col=2,lwd=.5)
      lines(x=newQuantiles,y=probs,lwd=.5)
      points(x=(newCaseData)[1],y=(order(newCaseData)/(max(order(newCaseData))+1))[1],col=8,pch=1)
      probNewCase <- pFUN(log(newCase),interceptParamAll,scaleParamAll)
      #print(format(1/probNewCase,digits=2,scientific=F,big.mark=","))

      oneInVal <- 1/probNewCase
      oneInVal <- signif(oneInVal,3)
      oneInVal <- sapply(oneInVal,FUN=function(x){
        ifelse(log10(x)>10,yes = format(x,scientific = TRUE),no = format(x,scientific = FALSE, big.mark=","))
      })

      text(
        x=newCaseData[1],
        y=.1,
        label=paste("1 / ",oneInVal,sep=""),
        adj=0,srt=90
      )
    }
    mtext(side=3,at=HCxAll,text=bquote(HC[.(round(100*effectLevelA1))]),adj=0,las=2)
    mtext(side=3,at=HCxAll/2,text=bquote(HC[.(round(100*effectLevelA1))]/2),adj=0,las=2)
    mtext(side=3,at=HCxAll/3,text=bquote(HC[.(round(100*effectLevelA1))]/3),adj=0,las=2)
    mtext(side=3,at=HCxAll/5,text=bquote(HC[.(round(100*effectLevelA1))]/5),adj=0,las=2)
    mtext(side=3,at=HCxAll/10,text=bquote(HC[.(round(100*effectLevelA1))]/10),adj=0,las=2)
  }
  baseSTR <- paste("HC",round(100*effectLevelA1),sep="")

  oneInVal <- sapply(oneIn,FUN=function(x){
    ifelse(log10(x)>10,yes = format(signif(x,3),scientific = TRUE),no = format(signif(x,3),scientific = FALSE, big.mark=","))
  })
  results <- data.frame(target=paste(baseSTR,c(2,3,5,10),sep="/"),targetValue=HCxAll/c(2,3,5,10),addOneValue=signif(exp(addPoints),4),addOneProb=signif(addProbs,4),oneIn=oneInVal,
                        oneInFormated=paste(
                          "1 / ",
                          oneInVal,
                          sep=""))

  if(plotTF){
    title(paste("Log",switch(distName,logis="Logistic",norm="Normal"),sep="-"),adj=0)
    mtext(side=1,outer=TRUE,line=-1,text=dataTag,adj=.05)

    #verification plots
    #print(caseData)
    standardPlot(caseData,as.character(caseTaxa),distName,xlimVals=" ")
    switch(distName,logis=title("Logistic (Original Data)",adj=0),norm=title("Normal (Original Data)",adj=0))
    sapply(1:nrow(results),FUN=function(i){
      x<-results[i,"addOneValue"]
      #print(results)
      #print(x)
      standardPlot(
        c(x,caseData),
        c(paste("Added value (",format(c(x,caseData),scientific=FALSE,digits=5)[1],")",sep=""),as.character(caseTaxa)),
        distName,showUpper=TRUE)
      points(x=x,y=1/(length(caseData)+2))

      #rug(side=1,x=x,col="red")
      switch(distName,
             logis=title(paste("Logistic (",as.character(results[i,"target"]),")",sep=""),adj=0),
             norm=title(paste("Normal (",as.character(results[i,"target"]),")",sep=""),adj=0))
    })
  }
  data.frame(responseVar=rep(dataTag,4),distribution=rep(distName,4),results)
}

if(FALSE){
  addOnePLL(effectValues=foo[,2],speciesLabels=foo$Species,distName="logis",dataTag="C12-13 All Species",effectLevelA1=0.05)
  addOnePLL(effectValues=foo[,2],speciesLabels=foo[,1],distName="logis",dataTag="C12-13 All Species",effectLevelA1=0.5,debugTF=TRUE)
  addOnePLL(effectValues=foo$Norm.C12.C13,speciesLabels=foo$Species,distName="norm",dataTag="C12-13 All Species")
}

if(FALSE){
  #a simple test
  genericPLL(10^rnorm(100),distName="norm",speciesLabels=paste("S",1:100),showUpper=TRUE,quiet=FALSE,par2.LB=1e-20,xlabString=expression(Concentration~(mu*g/L)))
  genericPLL((10^rnorm(100))/1000,distName="norm",speciesLabels=paste("S",1:100),showUpper=TRUE,quiet=FALSE,par2.LB=1e-20,xlabString=expression(Concentration~(mu*g/L)))
  foo <- read.delim("AllSpeciesC12-C13.txt")
  genericPLL(foo$Norm.C12.C13,speciesLabels=foo$Species,distName="logis")


  rbind(
    logistic=genericPLL(foo$Norm.C12.C13,speciesLabels=foo$Species,distName="logis",doPlots=FALSE),
    normal=genericPLL(foo$Norm.C12.C13,speciesLabels=foo$Species,distName="norm",doPlots=FALSE)
  )


  t(sapply(1:nrow(foo),FUN=function(i)genericPLL(foo$Norm.C12.C13[-i],speciesLabels=foo$Species,distName="logis",doPlots=FALSE)))

  library(parallel)
  snowCluster <- makePSOCKcluster(8)
  clusterExport(snowCluster,varlist=c("genericPLL","foo", "rriskFitdist.GJC"))
  do.call(rbind,clusterApplyLB(snowCluster,x=0:nrow(foo),fun=function(i){
    if(i==0)result <- genericPLL(foo$Norm.C12.C13,speciesLabels=foo$Species,distName="logis",doPlots=FALSE)
    if(i>=1)result <- genericPLL(foo$Norm.C12.C13[-i],speciesLabels=foo$Species[-i],distName="logis",doPlots=FALSE)
    result
  }))
}
