# add a page to section out pdf doc
pageBreakPDF <- function(messageString){
  plot(x=0,y=0,type="n",axes=FALSE,xlab="",ylab="")
  text(x=0,y=0,labels=messageString,adj=0.5,cex=2)
}

# for variable matching when default names not used
tags_from_names <- function(dfnames){
  lapply(
    dfnames,
    function(co) {
      tag(
        "p",
        list(
          tags$span(class = "glyphicon glyphicon-move"),
          tags$strong(co)
        )
      )
    }
  )
}

