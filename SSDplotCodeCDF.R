### speciesTF = FALSE is used to override listing species on the right margin.
### this is useful for using this same code to set up other plots

print("Running CDF plot code")
cexLAB <- input$labelSize # size of xlab and ylab
cexAXIS <- input$axisSize  # tick annotations size on axis
cexLWD <- input$lineSize  # line width
cexPCH <- ifelse(cexLWD<1,1,cexLWD*.75)  # plot symbol size
if(useFIT){
  print(fitOBJ$distName)
  dFUN <- get(paste0("d",fitOBJ$distName),envir = .GlobalEnv)
  qFUN <- get(paste0("q",fitOBJ$distName),envir = .GlobalEnv)
  pFUN <- get(paste0("p",fitOBJ$distName),envir = .GlobalEnv)
  FL2 <- fitOBJ$fitLines[,2]
  FL2 <- FL2[is.finite(FL2)]
  FL4 <- fitOBJ$fitLines[,4]
  FL4 <- FL4[is.finite(FL4)]
  xlims <- range(c(log10(c(min(FL2),max(FL4))),log10(input2plot$responses)))
  xlims[1] <- ifelse((xlims[1]-floor(xlims[1]))<0.5,floor(xlims[1]),floor(xlims[1])+1)
  xlims[2] <- ifelse((ceiling(xlims[2])-xlims[2])<0.5,ceiling(xlims[2]),ceiling(xlims[2])-1)
}
if(!useFIT)xlims <- range(log10(input2plot$responses))
### just in case, must cover range...
xlims <- range(c(xlims,log10(input2plot$responses)))
xlims <- 10^xlims
if(!is.null(xlimsFORCE)) xlims <- xlimsFORCE
### the par() setup allows for species names
par(mai=c(input$figH*.15, input$figW*.15, 0, input$figW*input$speciesMargin)+0.1,omi=rep(0,4))
if(!speciesTF)par(mai=c(input$figH*.15, input$figW*.15, 0, input$figW*0)+0.1,
                  omi=rep(0,4))
plotSetupGeneric(inputDF=input2plot,
                 yRange=c(0,1),
                 xRange=xlims,
                 cexLAB=cexLAB,cexAXIS=cexAXIS,cexLWD=cexLWD,
                 plotType=c("CDF","PDF")[1],
                 logscaleTF=TRUE,
                 ptColor="darkgray",
                 xlabSTR=paste0(input$xLab," (",input$units,")"),
                 ylabSTR="Probability")
if(useFIT){
  lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,4],col=lineColors[1],lwd=cexLWD*1.7)
  lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,2],col=lineColors[2],lwd=cexLWD)
  lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,3],col=lineColors[2],lwd=cexLWD)
  if(input$doGrays){
    lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,2],col=lineColors[3],lwd=cexLWD,lty=2)
    lines(y=fitOBJ$fitLines[,1],x=fitOBJ$fitLines[,3],col=lineColors[3],lwd=cexLWD,lty=2)
  }
}
if(!useFIT){
  #use the nonparametric version
  xVals <- quantile(input2plot$responses,probs = seq(0.0001,0.9999,length=10000),type = 8)
  yVals <- seq(0.0001,0.9999,length=10000)
  lines(y=yVals,x=xVals,col=lineColors[1],lwd=cexLWD*1.7)
}
#if(any(input2plot$species=="ADD ONE")){
#  with(subset(input2plot,species=="ADD ONE"),{
#    points(x=responses,y=yVals,cex=cexPCH*sqrt(2),pch=18)
#  })
#}

if(!inputList$doGrps){
  with(subset(input2plot,species!="ADD ONE"),{
    points(x=responses,y=yVals,cex=cexPCH,col="black",pch=pchOpens[1])
    points(x=responses,y=yVals,cex=cexPCH,col=colorList[1],pch=pchSolids[1])
  })
  if(any(input2plot$species=="ADD ONE")){
    with(subset(input2plot,species=="ADD ONE"),{
      points(x=responses,y=yVals,cex=cexPCH,col="black",pch=pchOpens[2])
      points(x=responses,y=yVals,cex=cexPCH,col="black",pch=pchSolids[2])
    })
  }
}

if(inputList$doGrps){
  groupList <- unique(subset(input2plot,species!="ADD ONE")$groups)
  if(any(input2plot$species=="ADD ONE")){
    groupStr <- unique(subset(input2plot,species=="ADD ONE")$groups)
    groupList <- c(groupList,groupStr)
  }
  with(subset(input2plot,species!="ADD ONE"),{
    points(x=responses,y=yVals,cex=cexPCH,col="black",pch=pchOpens[match(groups,groupList)])
    points(x=responses,y=yVals,cex=cexPCH,col=colorList[match(groups,groupList)],pch=pchSolids[match(groups,groupList)])
  })
  if(any(input2plot$species=="ADD ONE")){
    with(subset(input2plot,species=="ADD ONE"),{
      points(x=responses,y=yVals,cex=cexPCH,col="black",pch=pchOpens[match(groups,groupList)])
      points(x=responses,y=yVals,cex=cexPCH,col=colorList[match(groups,groupList)],pch=pchSolids[match(groups,groupList)])
      #points(x=responses,y=yVals,cex=cexPCH*sqrt(2),col=colorList[match(groups,groupList)],pch=18)
    })
  }
}

if(speciesTF){
  if(inputList$speciesMargin>0 & inputList$speciesSize>0 & !inputList$doGrps){
    mtext(side=4,at=input2plot$yVals,text = input2plot$species,
          las=1,family="serif",font = 3,cex=inputList$speciesSize)
  }
  #if(inputList$speciesMargin>0 & inputList$speciesSize>0 & inputList$doGrps & inputList$doGrays){
  #  mtext(side=4,at=input2plot$yVals,text = input2plot$species,
  #        las=1,family="serif",font = 3,cex=inputList$speciesSize,
  #        col="black",adj=-0.002)
  #}
  if(inputList$speciesMargin>0 & inputList$speciesSize>0 & inputList$doGrps){
    mtext(side=4,at=input2plot$yVals,text = input2plot$species,
          las=1,family="serif",font = 3,cex=inputList$speciesSize,
          col="black")
          #col=colorList[match(input2plot$groups,groupList)])
  }
}
#hcX <- 10^(qlogis(HC.primary,LOO.results.logis[[1]][[1]][5],LOO.results.logis[[1]][[1]][6]))
abline(h=inputList$ECXvalue,lty=3)
if(useFIT)HCX.value <- fitOBJ$fit[2]
if(!useFIT)HCX.value <- 10^quantile(log10(input2plot$responses),probs = inputList$ECXvalue,type = 8)
HCX.str <- format(signif(HCX.value,3),scientific = FALSE)
PCT.shift <- (2*nchar(HCX.str)+6)/100
HCX.lab <- paste0("HC",round(100*inputList$ECXvalue))
if(PCT.shift<=0.4){
  abline(v=HCX.value)
  text(x=HCX.value,y=par("usr")[4],
       labels = bquote(.(HCX.lab)==.(HCX.str)),
       cex = input$hcxSize,
       adj=c(1,-0.2),srt=90)
  if(useFIT){
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
}
if(PCT.shift>0.4){
  abline(v=HCX.value)
  text(x=HCX.value,y=par("usr")[4],
       labels = bquote(.(HCX.lab)),
       cex = input$hcxSize,
       adj=c(1,-0.2),srt=90)
  if(useFIT){
    abline(v=fitOBJ$fit["LowerCL"],col="gray")
    text(x=fitOBJ$fit["LowerCL"],y=par("usr")[4],labels = "LCL",adj=c(1,-0.2),srt=90)
  }
}

#print(fitOBJ$fit)
print(c(HCX.str=HCX.str))

