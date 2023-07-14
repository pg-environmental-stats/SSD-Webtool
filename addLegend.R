if(input$doGrps){
  groupList <- unique(subset(data4legend,species!="ADD ONE")$groups)
  if(any(data4legend$species=="ADD ONE")){
    groupStr <- unique(subset(data4legend,species=="ADD ONE")$groups)
    groupList <- c(groupList,groupStr)
  }

  ### if doGroups but not doLegend, start a new graph for just the legend
  ### and set correct x-value to locate
  if(!input$doLegend){
    plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
    xLegend <- par("usr")[1]
  }
  #if(input$doLegend){
  #  xLegend <- fitOBJ$fit[2]
  #}
  #set up legend and plot it
  if( any(data4legend$species=="ADD ONE"))pchVec <- c(pchSolids,18)
  if(!any(data4legend$species=="ADD ONE"))pchVec <- pchSolids
  print(groupList)
  print(pchVec)
  legend(x=xLegend,y=par("usr")[4],
         legend=groupList,col=colorList[match(groupList,groupList)],
         pch=pchVec,
         cex = input$hcxSize)
}
