shinyServer(function(input, output, session){
        
  ### not clear where libraries should be initialized...Currently in multiple places
  ### one on github, the "run" file will check for those needed, install, and load
  #source("libsAndFiles.R",local=TRUE)
  ### just a couple of functions used in program.  Others could be added to it.
  source("helperFUNs.R",local=TRUE)
  ### fitting function, and two AOI functions
  source("SSDfitFUN.R",local = TRUE)
  ### a function that I modified from the rriskDistributions package
  source("rriskFitdist.GJC.R",local = TRUE)
  ### plotting functions.
  source("plotFUNs.R",local = TRUE)

  source("LCxFUNS.R",local = TRUE)
  
  source("BVFunction2-0.R", local = TRUE)
  #the parallel processing of leave one out needs to know the environment
  #to export functions to the cluster.
  serverEnv <- environment()

  # if reset button is pressed
  observeEvent(input$reset_button, {
    js$reset()
    updateTabsetPanel(session, "outputTabActive",selected = "Help")
  })

  # Begin with focus on help
  updateTabsetPanel(session, "outputTabActive",selected = "Help")
  #when rvs$setupComplete is 0, it means that the preview button
  #must be clicked to commit selections and view data plot
  #prior to analysis being run.  This will result in rvs$setupComplete
  #being set to 1.

  # Reactive vars:
  #   1.  setupComplete
  #       0 means not ready, as in, preview data has not been completed
  #       1 means the the preview button has been clicked, so data should be ready for analysis
  #   2.  AnalysisComplete
  #       0 after setupComplete = 1 (ready for analysis but not complete)
  #       1 when end of SSD code is reached.
  #   3.  units
  #       Simply holds the units input string
  #   4.  indata
  #       The input data, after processing (ie, data input to the analysis code)

  # Initialize reactives
  rvs <- reactiveValues()
  rvs$setupComplete <- 0
  rvs$AnalysisComplete <- 0
  rvs$dataImported <- 0
  rvs$dataChecked <- 0
  rvs$varsChecked <- 0
  output$dataChecked <- reactive({rvs$dataChecked})
  outputOptions(output,"dataChecked",suspendWhenHidden = FALSE)
  output$setupComplete <- reactive({rvs$setupComplete})
  outputOptions(output,"setupComplete",suspendWhenHidden = FALSE)

  ### effect levels are at different default levels by convention, so
  ### this just sets a different default value for each analysis type.

  # set analysis options for each (SSD only for now)
  # this establishes input variables that will be used for next steps below
  observe({
    req(input$analysisType)
    if(FALSE){### better is just define slider in each if() block, as below
      output$effectSelects <- renderUI({
        tagList(
          conditionalPanel(
            condition = "input.analysisType=='Count'",
            sliderInput("ECXvalue", "Effect Level", 0.05, .95, 0.50, step = 0.05)
          ),
          conditionalPanel(
            condition = "input.analysisType=='BMD'",
            sliderInput("ECXvalue", "Effect Level", 0.05, .95, 0.10, step = 0.05)
          ),
          conditionalPanel(
            condition = "input.analysisType=='Continuous'",
            sliderInput("ECXvalue", "Effect Level", 0.05, .95, 0.10, step = 0.05)
          ),
          conditionalPanel(
            condition = "input.analysisType=='SSD'",
            sliderInput("ECXvalue", "Effect Level", 0.05, .50, 0.05, step = 0.05)
          )
        )
      })
    }
    ### At this point, don't allow anything but SSD selection

    output$optshead <- renderUI({
      p("Analysis options:",style = "font-size: 24px; padding: 0px 0px; margin:0%",.noWS = c("before","after","outside"))
    })
    output$conflevelSelects <- renderUI({
      sliderInput("ConfLevel", "Confidence Level", 0.80, .99, 0.95, step = 0.05)
    })
    
    ### show the default variable names (the code is more general, such as ignores case...)
    if(input$analysisType=="SSD"){
      output$effectSelects <- renderUI({
        sliderInput("ECXvalue", "Effect Level", 0.05, .50, 0.05, step = 0.05)
      })
      #this can be mimiced for other analysis examples
      output$downloadExcelExample <- downloadHandler(
        filename = "SSD Example.xlsx",
        content = function(file){
          file.copy("SSDexample.xlsx", file)
        }
      )
      output$ExampleDownload <- renderUI({
        req(input$analysisType)
        #if (input$AnalysisComplete==0)return(actionButton("dummyButton","DUMMY"))
        return(downloadBttn('downloadExcelExample', 'SSD Example',"fill"))
      })
      
      output$helpPanel <- renderUI({
        includeHTML(path = "SSD Analysis Tool-New.html")
      })
      
      output$guidePanel <- renderUI({
        includeHTML(path = "SSD Analysis Tool User Guide-2.html")
      })
      
      #The SSD analysis is now generalized to the point that default var names are unnecessary
      output$defaultVars <- NULL
      if(FALSE)output$defaultVars <- renderUI(HTML(
        paste(
          h5(HTML("Required (default)</br>columns:"),.noWS = c("after","before","outside")),
          p(HTML("<ul><li>species</li><li>responses</li><li>(groups)</li></ul>"),.noWS = c("after","before","outside"))
        )
      )
      )
      output$analysisOpts <- renderUI({
        tagList(
          splitLayout(checkboxInput(inputId = "doLOO",label = "Leave 1 Out",value = FALSE),
                      checkboxInput(inputId = "doAOI",label = "Add 1 In",value = FALSE)),
          splitLayout(checkboxInput(inputId = "doGrps",label = "Grouping",value = FALSE))
                      #checkboxInput(inputId = "doAvg",label = "Model Averaging",value = FALSE))
        )
      })
      output$plotOpts <- renderUI({
        splitLayout(checkboxInput(inputId = "doGrays",label = "Grayscale",value = TRUE),
                    checkboxInput(inputId = "doLegend",label = "Legend on plots",value = FALSE))
      })
    }
    if(input$analysisType=="Count"){
      #this can be mimiced for other analysis examples
      output$effectSelects <- renderUI({
        sliderInput("ECXvalue", "Effect Level", 0.05, .95, 0.50, step = 0.05)
      })
      output$downloadExcelExample <- downloadHandler(
        filename = "LCx Example.xlsx",
        content = function(file){
          file.copy("LCxexample.xlsx", file)
        }
      )
      output$ExampleDownload <- renderUI({
        req(input$analysisType)
        #if (input$AnalysisComplete==0)return(actionButton("dummyButton","DUMMY"))
        return(downloadBttn('downloadExcelExample', 'LCx Example',"fill"))
      })
      
      output$helpPanel <- renderUI({
        includeHTML(path = "LCx Analysis Tool.html")
      })
      output$plotOpts <- renderUI({
        checkboxInput(inputId = "doGrays",label = "Grayscale",value = TRUE)
      })
      output$analysisOpts <- renderUI({
        radioButtons(inputId = "modelType",label = "Model assumption on background",
                     choiceNames = c("Assume Zero (STD)", "Estimate it (Abbott)"),
                     choiceValues = c("lcx", "abbott"),
                     inline = TRUE,
                     width = "100%",
                     selected = "lcx"
        )
      })
      
      #The SSD analysis is now generalized to the point that default var names are unnecessary
      output$defaultVars <- NULL
      if(FALSE){output$defaultVars <- renderUI(HTML(
        paste(
          h5(HTML("Required (default)</br>columns:"),.noWS = c("after","before","outside")),
          p(HTML("<ul><li>doses</li><li>responses</li><li>sizes</li></ul>"),.noWS = c("after","before","outside"))
        )
      )
      )
    }
    }
    
    if(input$analysisType=="Continuous"){
      #this can be mimiced for other analysis examples
      output$effectSelects <- renderUI({
        sliderInput("ECXvalue", "Effect Level", 0.05, .95, 0.10, step = 0.05)
      })
      output$downloadExcelExample <- downloadHandler(
        filename = "BV Example.xlsx",
        content = function(file){
          file.copy("BVexample.xlsx", file)
        }
      )
      output$ExampleDownload <- renderUI({
        req(input$analysisType)
        #if (input$AnalysisComplete==0)return(actionButton("dummyButton","DUMMY"))
        return(downloadBttn('downloadExcelExample', 'BV Example',"fill"))
      })
      
      output$helpPanel <- renderUI({
        includeHTML(path = "BV Analysis Tool.html")
      })
      output$plotOpts <- renderUI({
        checkboxInput(inputId = "doGrays",label = "Grayscale",value = TRUE)
      })
      output$analysisOpts <- renderUI({
        radioButtons(
          "varFixed",
          "Variance assumption",
          choiceNames = c("Proportional", "Constant"),
          choiceValues = c(FALSE, TRUE),
          inline = TRUE,
          width = "100%",
          selected = FALSE
        )
      })
      
      #The SSD analysis is now generalized to the point that default var names are unnecessary
      output$defaultVars <- NULL
      if(FALSE){output$defaultVars <- renderUI(HTML(
        paste(
          h5(HTML("Required (default)</br>columns:"),.noWS = c("after","before","outside")),
          p(HTML("<ul><li>doses</li><li>responses</li><li>sizes</li></ul>"),.noWS = c("after","before","outside"))
        )
      )
      )
      }
    }
    
    if(input$analysisType!="SSD"){
      output$speciesOpts <- NULL
    }
    if(input$analysisType!="Count"){
      #output$LCXmodelType <- NULL
    }
    if(input$analysisType!="Continuous"){
      output$respLabels <- NULL
    }
  })
  


  #create a reactive expression that will be check for going back to
  #"beginning", without a complete reset
  goBack <- reactive({
    list(
      input$analysisType,
      input$pasteData,
      input$doGrps
      #input$doGrays
      #input$doLegend
    )
  })
  #
  newVars <- reactive({
    list(
      input$sort_x,
      input$sort_y,
      input$sort_z
      #input$doGrays
      #input$doLegend
    )
  })
  
  #what to do here?  Reset things if something changes that would change numerical results?
  #or also any graphics.  Graphics changes can still be seen by clicking the check inputs
  observeEvent({list(
    input$pasteData,
    #input$figH,
    #input$figW,
    #input$axisSize,
    #input$labelSize,
    #input$lineSize,
    #input$speciesMargin,
    #input$speciesSize,
    #input$hcxSize,
    #input$doLOO,
    #input$doAOI,
    #input$xLab,
    #input$units,
    input$ECXvalue)},
    {
      req(rvs$dataChecked ==1)
      print("Input changes detected")
      print("Input changes detected")
      print("Input changes detected")
      print("Input changes detected")
      print(c(rvs.setupComplete.pre=rvs$setupComplete))
      rvs$setupComplete <- 0
      print(c(rvs.setupComplete.post=rvs$setupComplete))
    }
  )
  
  observeEvent(goBack(),{
     print("Inside observe event on goBack()")
     updateTabsetPanel(session, "outputTabActive",selected = "Help")
     rvs$setupComplete <- 0
     rvs$AnalysisComplete <- 0
     rvs$dataImported <- 0
     rvs$dataChecked <- 0
     rvs$varsChecked <- 0
     rvs$finalDF <- NULL
     rvs$inputDF <- NULL
     #output$dataChecked <- reactive({rvs$dataChecked})
     #Don't go further unless something is pasted into the data box
     req(input$pasteData)
     updateTabsetPanel(session, "outputTabActive",selected = "Output")
     if(is.null(input$pasteData))return(indata=NULL)
     print((input$pasteData))
     #put data into regular object and process it
     inputText <- (input$pasteData)
     print(inputText)
     inputLines <- strsplit(inputText,split = "\n")[[1]]
     print(inputLines)
     varNames <- scan(text=inputLines[[1]],sep="\t",what=character())
     print(varNames)
     dataBody <- t(sapply(inputLines[-1],FUN=function(x)scan(text=x,sep="\t",what=character())))
     #if only one column of numbers, the t() above should be undone
     if(nrow(dataBody)==1)dataBody <- t(dataBody)
     dimnames(dataBody) <- list(NULL,NULL)
     print(dataBody)
     inputDF <- structure(as.data.frame(dataBody,stringsAsFactors=FALSE),names=make.names(varNames))
     print(inputDF)
     #above results in character variables only.  Here, if most of the entries in a variable
     #can be converted to numeric, then convert it to numeric
     for(i in 1:ncol(inputDF)){
       if(sum(is.na(as.numeric(inputDF[,i])))<=nrow(inputDF)/2){
         inputDF[,i] <- as.numeric(inputDF[,i])
       }
     }
     print(inputDF)
     print(unlist(lapply(inputDF,is.numeric)))
     print(c("Data import complete"))
     rvs$dataImported <- 1
     rvs$inputDF <- inputDF
     # https://community.rstudio.com/t/extend-width-of-column-with-renderdatatable-in-shiny/50906
     outputDT.Raw <- as.datatable(formattable(inputDF),
                                  #class = 'row-border stripe hover compact nowrap',
                                  class = 'stripe compact',
                                  #escape = FALSE,
                                  options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                                 #autoWidth = TRUE,
                                                 pageLength = 10, info = FALSE,
                                                 lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                                 scrollX = TRUE, scrollY = FALSE,
                                                 paging = TRUE, ordering = FALSE,
                                                 searching = FALSE))
     #options=list(autoWidth = TRUE,scrollX = FALSE,scrollY = FALSE,searching = FALSE))
     output$DTtableRaw <- DT::renderDT(outputDT.Raw)
   },ignoreInit = TRUE)

  
  ### Any time the variable selections are changed, reprocess data setup
  observeEvent(newVars(),{
    req(input$analysisType=="SSD")
    rvs$dataChecked <- 0
    req(rvs$dataImported == 1 & rvs$dataChecked == 0 & rvs$varsChecked == 0)# & is.null(rvs$finalDF))
    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    #shiny::req(getData())
    testData <- rvs$inputDF
    #req({rvs$dataImported == 1})
    namesInFrame <- names(testData)

    if(!input$doGrps){
      print(c(species=input$sort_x,responses=input$sort_y))
      # this forces wait until both vars are selected
      print("SSD check 2")
      testData <- rvs$inputDF
      namesInFrame <- names(testData)
      if(!all(c("species","responses") %in% namesInFrame)){
        #Check that each selection is made
        print("Match check")
        req(input$sort_x)
        req(input$sort_y)
        #req({input$sort_x != input$sort_y})
        print(c(Spec.var=input$sort_x,NOEC.var=input$sort_y))
        print("Both matches made")
        oldNames <- c(input$sort_x,input$sort_y)
        print(oldNames)
        testData <- testData[,oldNames]
        names(testData) <- c("species","responses")
      }
      print(testData)

      print("ready to do formattable() on testData")
      print(head(testData))
      ### once we get here, we can assume that the data have been correctly identified
      outputDT.1 <- as.datatable(formattable(testData[,c("species","responses")],
                                             #align =c("r","l"),
                                             list(
                                               species = formatter("span", style = ~ style(color = "blue",font.style = "italic")),
                                               responses = formatter("span", style = ~ style(color="green", float="right")))),
                                 class = 'stripe compact',
                                 #escape = FALSE,
                                 options = list(#columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                   #autoWidth = TRUE,
                                   pageLength = 10, info = FALSE,
                                   lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                   scrollX = TRUE, scrollY = FALSE,
                                   paging = TRUE, ordering = FALSE,
                                   searching = FALSE))
      #%>%
      #  DT::formatRound(columns = 2,digits = roundTo)
      #if(FALSE)outputDT <- DT::datatable(outputDT,
      #                          class = 'row-border stripe hover compact nowrap',
      #                          rownames = FALSE,
      #                          autoHideNavigation = TRUE, escape =FALSE) %>%
      #formatStyle(columns = "Species",
      #            target="cell",
      #            fontWeight = styleEqual(1:nrow(outputData), rep("bold",nrow(outputData)))) %>%
      #DT::formatRound(columns = 2,digits = roundTo)
      #output$DTtableRaw <- DT::renderDT(rvs$inputDF)
      output$DTtable <- DT::renderDT(outputDT.1)
      
      #output$table <- renderTable(testData)
      
      # Then, do a couple of checks
      if(length(unique(testData$species))<nrow(testData))alerID <- shinyalert(
        title = "Warning",
        text = "Species labels not all unique.\nCheck if unexpected.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      if(!is.numeric(testData$responses))alerID <- shinyalert(
        title = "Warning",
        text = "Effects variable not numeric.\nCheck input!!",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      rvs$dataChecked <- 1
      #And at this point, blank out other sections of the inputs interface
      #these should only appear once data has been set up.
      output$scaleSelect <- NULL
      output$varLabels <- NULL
      output$downloadExcel <- NULL
      output$downloadPDF <- NULL
      output$Excelbutton <- NULL
      output$PDFbutton <- NULL
      output$setupButton <- NULL
      output$SSD.1.1 <- NULL
      output$SSD.1.2 <- NULL
      output$SSD.1.3 <- NULL
      #output$SSD.2.1 <- NULL
      #output$SSD.2.2 <- NULL
      #output$SSDoptshead <- NULL
      output$SSDconditional2 <- NULL
    }
    
    
    if(input$doGrps){
      print(c(species=input$sort_x,responses=input$sort_y,groups=input$sort_z))
      # this forces wait until both vars are selected
      print("SSD check 2")
      testData <- rvs$inputDF
      namesInFrame <- names(testData)
      if(!all(c("species","responses","groups") %in% namesInFrame)){
        #Check that each selection is made
        print("Match check")
        req(input$sort_x)
        req(input$sort_y)
        req(input$sort_z)
        print(c(Spec.var=input$sort_x,NOEC.var=input$sort_y,Group.var=input$sort_z))
        print("All 3 matches made")
        oldNames <- c(input$sort_x,input$sort_y,input$sort_z)
        print(oldNames)
        testData <- testData[,oldNames]
        names(testData) <- c("species","responses","groups")
      }
      print("ready to do formattable() on testData")
      print(head(testData))
      ### once we get here, we can assume that the data have been correctly identified
      outputDT.1 <- as.datatable(formattable(testData[,c("species","responses","groups")],
                                             #align =c("r","l"),
                                             list(
                                               species = formatter("span", style = ~ style(color = "blue",font.style = "italic")),
                                               responses = formatter("span", style = ~ style(color="green", float="right")),
                                               groups=formatter("span", style = ~ style(color = "blue")))),
                                 class = 'stripe compact',
                                 #escape = FALSE,
                                 options = list(#columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                   #autoWidth = TRUE,
                                   pageLength = 10, info = FALSE,
                                   lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                   scrollX = TRUE, scrollY = FALSE,
                                   paging = TRUE, ordering = FALSE,
                                   searching = FALSE))
      #%>%
      #  DT::formatRound(columns = 2,digits = roundTo)
      #if(FALSE)outputDT <- DT::datatable(outputDT,
      #                          class = 'row-border stripe hover compact nowrap',
      #                          rownames = FALSE,
      #                          autoHideNavigation = TRUE, escape =FALSE) %>%
      #formatStyle(columns = "Species",
      #            target="cell",
      #            fontWeight = styleEqual(1:nrow(outputData), rep("bold",nrow(outputData)))) %>%
      #DT::formatRound(columns = 2,digits = roundTo)
      #output$DTtableRaw <- DT::renderDT(rvs$inputDF)
      output$DTtable <- DT::renderDT(outputDT.1)
      
      #output$table <- renderTable(testData)
      
      # Then, do a couple of checks
      if(length(unique(testData$species))<nrow(testData))alerID <- shinyalert(
        title = "Warning",
        text = "Species labels not all unique.\nCheck if unexpected.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      if(!is.numeric(testData$responses))alerID <- shinyalert(
        title = "Warning",
        text = "Effects variable not numeric.\nCheck input!!",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      rvs$dataChecked <- 1
      #And at this point, blank out other sections of the inputs interface
      #these should only appear once data has been set up.
      output$scaleSelect <- NULL
      output$varLabels <- NULL
      output$downloadExcel <- NULL
      output$downloadPDF <- NULL
      output$Excelbutton <- NULL
      output$PDFbutton <- NULL
      output$setupButton <- NULL
      output$SSD.1.1 <- NULL
      output$SSD.1.2 <- NULL
      output$SSD.1.3 <- NULL
      #output$SSD.2.1 <- NULL
      #output$SSD.2.2 <- NULL
      #output$SSDoptshead <- NULL
      output$SSDconditional2 <- NULL
    }
    ### as others are added, these will be populated like SSD
    #at this point, remove data with missing values on response
    if(sum(is.na(testData$responses))>0){
      alerID <- shinyalert(
        title = "Warning",
        text = "Missing/non-numeric values in effects values will be removed on preview action.  Check if unexpected.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
    rvs$preSort <- testData
    testData <- testData[!is.na(testData$responses),]
    #calculate the nonparametric quantiles of the data for all plotting
    #order the response values before proceeding with analysis
    testData <- testData[order(testData$responses),]
    if(nrow(testData)<3)alerID <- shinyalert(
      title = "Error",
      text = "This analysis requires 3 or more effect values in the data.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){js$reset()}
    )
    if(sum(testData$responses<=0)>0)alerID <- shinyalert(
      title = "Error",
      text = "Effect values must all be greater than zero.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){js$reset()}
    )
    
    xVals <- quantile(log10(testData$responses),probs = seq(0.0001,0.9999,length=10000),type=8)
    #dummy values are used so that ties don't occur in the y-dimension
    #(tied data will sort in order they appear)
    xValsDummy <- quantile(order(testData$responses),probs = seq(0.0001,0.9999,length=10000),type=8)
    yVals <- seq(0.0001,0.9999,length=10000)
    pointIDs <- sapply(order(testData$responses),FUN = function(x)which.min(abs(x-xValsDummy)))
    print(c(pointIDs=pointIDs))
    pointIDs[1] <- max(which(xValsDummy==xValsDummy[1]))
    print(c(pointIDs=pointIDs))
    testData$yVals <- yVals[pointIDs]
    print(c(yVals=yVals[pointIDs]))
    rvs$finalDF <- testData
    
    req(!is.null(rvs$finalDF))
    print("FinalDF is found, set reactives for SSD")
    if(all(c("species","responses") %in% names(rvs$finalDF))){
      rvs$dataChecked <- 1
    }
  })

  observeEvent(newVars(),{
    req(input$analysisType=="Count")
    rvs$Countdata <- 0
    rvs$dataChecked <- 0
    req(rvs$dataImported == 1 & rvs$dataChecked == 0 & rvs$varsChecked == 0)# & is.null(rvs$finalDF))
    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    #shiny::req(getData())
    testData <- rvs$inputDF
    #req({rvs$dataImported == 1})
    namesInFrame <- names(testData)
    
    print(c(doses=input$sort_x,responses=input$sort_y,sizes=input$sort_z))
    # this forces wait until all vars are selected
    print("LCx check 2")
    testData <- rvs$inputDF
    namesInFrame <- names(testData)
    if(!all(c("doses","responses","sizes") %in% namesInFrame)){
      #Check that each selection is made
      print("Match check")
      req(input$sort_x)
      req(input$sort_y)
      req(input$sort_z)
      print(c(Unique.Var.Count = length(unique(c(input$sort_x,input$sort_y,input$sort_z)))))
      #req({input$sort_x != input$sort_y})
      print(c(Dose.var=input$sort_x,RespCount.var=input$sort_y,SizeCount.var=input$sort_z))
      oldNames <- c(input$sort_x,input$sort_y,input$sort_z)
      print(oldNames)
      testData <- testData[,oldNames]
      names(testData) <- c("doses","responses","sizes")
    }
    print(testData)
    
    print("ready to do formattable() on testData")
    print(head(testData))
    
    ### once we get here, we can assume that the data have been correctly identified
    outputData <- testData[,c("doses","responses","sizes")]
    names(outputData) <- c("Exposure Conc","Response Count","Group Size")
    print("USE DT TO FORMAT")
    #output$DTtable <- renderDT(outputData)
    #options below turn off the search field ,options = list(dom = 't'))
    #but, that also disables scroll so not using now.
    outputDT <- as.datatable(formattable(outputData,
                                         #align =c("r","l"),
                                         list(
                                           'Exposure Conc' = formatter("span", style = ~ style(float="right")),
                                           'Response Count' = formatter("span", style = ~ style(float="right")),
                                           'Group Size' = formatter("span", style = ~ style(float="right")))),
                             class = 'stripe compact',
                             #escape = FALSE,
                             options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                            #autoWidth = TRUE,
                                            pageLength = 10, info = FALSE,
                                            lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                            scrollX = TRUE, scrollY = FALSE,
                                            paging = TRUE, ordering = FALSE,
                                            searching = FALSE))
    #%>%
    #  DT::formatRound(columns = 2,digits = roundTo)
    #if(FALSE)outputDT <- DT::datatable(outputDT,
    #                          class = 'row-border stripe hover compact nowrap',
    #                          rownames = FALSE,
    #                          autoHideNavigation = TRUE, escape =FALSE) %>%
    #formatStyle(columns = "Species",
    #            target="cell",
    #            fontWeight = styleEqual(1:nrow(outputData), rep("bold",nrow(outputData)))) %>%
    #DT::formatRound(columns = 2,digits = roundTo)
    output$DTtable <- DT::renderDT(outputDT)
    
    #output$table <- renderTable(testData)
    
    # Then, do a couple of checks
    if(!is.numeric(testData$responses))alerID <- shinyalert(
      title = "Warning",
      text = "Response count variable not numeric.\nCheck input!!",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    if(!is.numeric(testData$sizes))alerID <- shinyalert(
      title = "Warning",
      text = "Group size count variable not numeric.\nCheck input!!",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    if(length(unique(c(input$sort_x,input$sort_y,input$sort_z)))==3)rvs$dataChecked <- 1
    #And at this point, blank out other sections of the inputs interface
    #these should only appear once data has been set up.
    output$scaleSelect <- NULL
    output$varLabels <- NULL
    output$downloadExcel <- NULL
    output$downloadPDF <- NULL
    output$Excelbutton <- NULL
    output$PDFbutton <- NULL
    output$setupButton <- NULL
    ###probably no harm in nulling these even though not SSD section
    output$SSD.1.1 <- NULL
    output$SSD.1.2 <- NULL
    output$SSD.1.3 <- NULL
    #output$SSD.2.1 <- NULL
    #output$SSD.2.2 <- NULL
    #output$SSDoptshead <- NULL
    output$SSDconditional2 <- NULL

    
    ### as others are added, these will be populated like SSD
    #at this point, remove data with missing values on response
    if(sum(is.na(testData$responses))>0){
      alerID <- shinyalert(
        title = "Warning",
        text = "Missing/non-numeric values in response count values will be removed on preview action.  Check if unexpected.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
    if(sum(is.na(testData$sizes))>0){
      alerID <- shinyalert(
        title = "Warning",
        text = "Missing/non-numeric values in group size count values will be removed on preview action.  Check if unexpected.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
    rvs$preSort <- testData
    ###addresses two issues here, missing values in any vars get omitted, and doses with multiple records get combined
    testData <- aggregate(cbind(responses,sizes)~doses,data=testData,FUN = sum)
    #calculate the nonparametric quantiles of the data for all plotting
    #order the response values before proceeding with analysis
    testData <- testData[order(testData$doses),]
    rvs$postSort <- testData
    if(nrow(testData)<3)alerID <- shinyalert(
      title = "Error",
      text = "This analysis requires 3 or more dose values in the data.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){js$reset()}
    )
    if(sum(testData$responses<0)>0)alerID <- shinyalert(
      title = "Error",
      text = "Response count values must all be zero or greater.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){js$reset()}
    )
    if(sum(testData$sizes<0)>0)alerID <- shinyalert(
      title = "Error",
      text = "Group size count values must all be zero or greater.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){js$reset()}
    )
    if(sum((testData$sizes-testData$responses)<0)>0)alerID <- shinyalert(
      title = "Error",
      text = "At least one response count is larger than the group size.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){js$reset()}
    )
    
    rvs$finalDF <- testData
    
    req(!is.null(rvs$finalDF))
    print("FinalDF is found, set reactives for LCx")
    if(all(c("doses","responses","sizes") %in% names(rvs$finalDF)) &
       length(unique(c(input$sort_x,input$sort_y,input$sort_z)))==3){
      #rvs$Countdata <- 1
      rvs$dataChecked <- 1
      ### do this check after data has the right variables
      if(nrow(rvs$postSort)<nrow(rvs$preSort)){
        alerID <- shinyalert(
          title = "Warning",
          text = "At least one dose value occurs more than once, and will be combined.  Check if unexpected.",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      }
    }
  })
  
  observeEvent(newVars(),{
    req(input$analysisType=="Continuous")
    #rvs$Countdata <- 0
    rvs$dataChecked <- 0
    req(rvs$dataImported == 1 & rvs$dataChecked == 0 & rvs$varsChecked == 0)# & is.null(rvs$finalDF))
    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    #shiny::req(getData())
    testData <- rvs$inputDF
    #req({rvs$dataImported == 1})
    namesInFrame <- names(testData)
    
    print(c(doses=input$sort_x,responses=input$sort_y))
    # this forces wait until all vars are selected
    print("BV check 2")
    testData <- rvs$inputDF
    namesInFrame <- names(testData)
    if(!all(c("doses","responses") %in% namesInFrame)){
      #Check that each selection is made
      print("Match check")
      req(input$sort_x)
      req(input$sort_y)
      
      print(c(Unique.Var.Count = length(unique(c(input$sort_x,input$sort_y)))))
      #req({input$sort_x != input$sort_y})
      print(c(Dose.var=input$sort_x,RespCount.var=input$sort_y))
      oldNames <- c(input$sort_x,input$sort_y)
      print(oldNames)
      testData <- testData[,oldNames]
      names(testData) <- c("doses","responses")
    }
    print(testData)
    
    print("ready to do formattable() on testData")
    print(head(testData))
    
    ### once we get here, we can assume that the data have been correctly identified
    outputData <- testData[,c("doses","responses")]
    names(outputData) <- c("Exposure Conc","Response")
    print("USE DT TO FORMAT")
    #output$DTtable <- renderDT(outputData)
    #options below turn off the search field ,options = list(dom = 't'))
    #but, that also disables scroll so not using now.
    outputDT <- as.datatable(formattable(outputData,
                                         #align =c("r","l"),
                                         list(
                                           'Exposure Conc' = formatter("span", style = ~ style(float="right")),
                                           'Response' = formatter("span", style = ~ style(float="right")))),
                             class = 'stripe compact',
                             #escape = FALSE,
                             options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                            #autoWidth = TRUE,
                                            pageLength = 10, info = FALSE,
                                            lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                            scrollX = TRUE, scrollY = FALSE,
                                            paging = TRUE, ordering = FALSE,
                                            searching = FALSE))
    #%>%
    #  DT::formatRound(columns = 2,digits = roundTo)
    #if(FALSE)outputDT <- DT::datatable(outputDT,
    #                          class = 'row-border stripe hover compact nowrap',
    #                          rownames = FALSE,
    #                          autoHideNavigation = TRUE, escape =FALSE) %>%
    #formatStyle(columns = "Species",
    #            target="cell",
    #            fontWeight = styleEqual(1:nrow(outputData), rep("bold",nrow(outputData)))) %>%
    #DT::formatRound(columns = 2,digits = roundTo)
    output$DTtable <- DT::renderDT(outputDT)
    
    #output$table <- renderTable(testData)
    
    # Then, do a couple of checks
    if(!is.numeric(testData$responses))alerID <- shinyalert(
      title = "Warning",
      text = "Response variable not numeric.\nCheck input!!",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    
    if(length(unique(c(input$sort_x,input$sort_y)))==2)rvs$dataChecked <- 1
    #And at this point, blank out other sections of the inputs interface
    #these should only appear once data has been set up.
    output$scaleSelect <- NULL
    output$varLabels <- NULL
    output$downloadExcel <- NULL
    output$downloadPDF <- NULL
    output$Excelbutton <- NULL
    output$PDFbutton <- NULL
    output$setupButton <- NULL
    ###probably no harm in nulling these even though not SSD section
    output$SSD.1.1 <- NULL
    output$SSD.1.2 <- NULL
    output$SSD.1.3 <- NULL
    #output$SSD.2.1 <- NULL
    #output$SSD.2.2 <- NULL
    #output$SSDoptshead <- NULL
    output$SSDconditional2 <- NULL
    
    
    ### as others are added, these will be populated like SSD
    #at this point, remove data with missing values on response
    if(sum(is.na(testData$responses))>0){
      alerID <- shinyalert(
        title = "Warning",
        text = "Missing/non-numeric values in response count values will be removed on preview action.  Check if unexpected.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }

    rvs$preSort <- testData
    ###addresses two issues here, missing values in any vars get omitted, and doses with multiple records get combined
    #testData <- aggregate(cbind(responses,sizes)~doses,data=testData,FUN = sum)
    #calculate the nonparametric quantiles of the data for all plotting
    #order the response values before proceeding with analysis
    testData <- testData[order(testData$doses),]
    rvs$postSort <- testData
    if(length(unique((testData$doses)))<3)alerID <- shinyalert(
      title = "Error",
      text = "This analysis requires 3 or more dose values in the data.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){js$reset()}
    )
    if(sum(testData$responses<0)>0)alerID <- shinyalert(
      title = "Error",
      text = "Response values must all be zero or greater.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){js$reset()}
    )

    
    rvs$finalDF <- testData
    
    req(!is.null(rvs$finalDF))
    print("FinalDF is found, set reactives for BV")
    if(all(c("doses","responses") %in% names(rvs$finalDF)) &
       length(unique(c(input$sort_x,input$sort_y)))==2){
      #rvs$Countdata <- 1
      rvs$dataChecked <- 1
      ### do this check after data has the right variables
      
    }
  })
  
  
  ### observeEvent(input$xLab,{
  ###  parenPos <- gregexpr(pattern="[()]",text = input$xLab)[[1]]
  ###  rvs$units <- substring(isolate(input$xLab),first=parenPos[1]+1,last=tail(parenPos,1)-1)
  ### })

  ### read the data that has been pasted in.  This is independent of the type of analysis

  # Effect level conventions change with the type of experiment/analysis, so they
  # are set independently here

  #GRAPH options section presented after data setup and checks
  observe({
    print("In graph options setups")
    output$graphOpts <- renderUI({
      tagList(
        h3("Finalize inputs above",style="color: red"))})
    print(c(dataImported = rvs$dataImported, dataChecked=rvs$dataChecked,nullFinal = as.numeric(!is.null(rvs$finalDF))))
    output$speciesOpts <- NULL
    req(rvs$dataImported == 1 & rvs$dataChecked == 1 & !is.null(rvs$finalDF))
    
    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    output$graphOpts <- renderUI({
      tagList(
        h3("Graphics options:"),
        splitLayout(
          sliderInput("figH", label = "Fig Ht (in)",  min = 3,max = 12,value = 8,step = 0.5),
          sliderInput("figW", label = "Fig Wd (in)",  min = 3,max = 12,value = 8,step = 0.5)
        ),
        splitLayout(
          sliderInput("axisSize", label = "Tick Labels",min = 0.1,max = 4,value = 1.5,step = 0.05),
          sliderInput("labelSize",label = "Axis Labels",  min = 0.1,max = 4,value = 1.5,step = 0.05),
          sliderInput("lineSize",label = "Line/Point Weight",  min = 0.1,max = 4,value = 2,step = 0.05)
        )
      )
    })
    if(input$analysisType=="SSD"){
      ###only offer these options if it is SSD analysis (starts as NULL above)
      output$speciesOpts <- renderUI({
        splitLayout(
          sliderInput("speciesMargin", "Sp Margin", min = 0, max = 0.5, value = 0.3,step = 0.05),
          sliderInput("speciesSize", "Sp Size", min = 0, max = 2, value = 1,step = 0.05),
          sliderInput("hcxSize", "HCp Size", min = 0.5, max = 2, value = 1,step = 0.05)
        )
      })
    }
  })

  #on new data import, or a change in analysis type, redo the variable matching
  #here, instead of everything in one block, do each analysis type separately
  observe({
    req(input$analysisType=="SSD")
    req(rvs$dataImported == 1 & rvs$dataChecked == 0 & rvs$varsChecked == 0)# & is.null(rvs$finalDF))
    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    #shiny::req(getData())
    testData <- rvs$inputDF
    #req({rvs$dataImported == 1})
    namesInFrame <- names(testData)
    #source("ComboDragDrops.R",local = TRUE)
    print("build variable selections interface")
    print(input$analysisType)
    print(testData)
    numericVars <- which(unlist(lapply(testData,is.numeric)))
    #drop variables with negative values because those cannot be valid doses
    #but, what about missing values here?
    numericVars <- numericVars[which(colSums(testData[,numericVars,drop=FALSE]<=0,na.rm = TRUE)==0)]
    uniqueCounts <- unlist(lapply(testData,FUN = function(x)length(unique(x))))
    #| max(uniqueCounts)<nrow(testData)
    if(length(numericVars)==0){
      alerID <- shinyalert(
        title = "Error",
        text = "No valid numeric data found (check for zero and negative values?).  Tool will Reset.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE,
        callbackR = function(x){js$reset()}
      )
      #stop("There is a problem with your data (no numeric or every var has duplicates)")
      }
    bestResp <- (names(testData)[numericVars])[which(uniqueCounts[numericVars]==max(uniqueCounts[numericVars]))]
    if(length(bestResp)>1){
      print("Break ties on response variable by GOF testing")
      print(bestResp)
      gofP <- sapply(bestResp,FUN = function(resVar){
        x <- na.omit(log10(testData[,resVar]))
        ADGofTest::ad.test(x,distr.fun = pnorm,mean=mean(x),sd=sd(x))$p.value
      })
      print(gofP)
      bestResp <- bestResp[which.max(gofP)]
    }
    bestSpecies <- (names(testData)[-numericVars])[which.max(uniqueCounts[-numericVars])]
    if(!input$doGrps){
      output$varSelects <- renderUI({
        tagList(
          selectInput(inputId="sort_y","Effect concentration:",choices = names(testData)[numericVars],selected = bestResp),
          selectInput(inputId="sort_x","Species:",choices = names(testData),selected = bestSpecies))
        })
      # once the varSelects is completed, finalDF should be created
    }
    
    if(input$doGrps){
      #if(ncol(testData)<3) stop("If you are grouping data, need 3 columns (response, species, group)")
      bestGroup <- names(testData)[which.min(uniqueCounts)]
      output$varSelects <- renderUI({
        tagList(
          selectInput(inputId="sort_y","Effect concentration:",choices = names(testData)[numericVars],selected = bestResp),
          selectInput(inputId="sort_x","Species:",choices = names(testData),selected = bestSpecies),
          selectInput(inputId="sort_z","Grouping:",choices = names(testData),selected = bestGroup))
        })
      print(c(sort_x=input$sort_x,sort_y=input$sort_y))
}

    req(!is.null(rvs$finalDF))
    print("FinalDF is found, set reactives for SSD")
    if(all(c("species","responses") %in% names(rvs$finalDF))){
      rvs$dataChecked <- 1
    }
  })
  
  observe({
    req(input$analysisType=="Count")
    req(rvs$dataImported == 1 & rvs$dataChecked == 0 & rvs$varsChecked == 0)# & is.null(rvs$finalDF))
    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    #shiny::req(getData())
    testData <- rvs$inputDF
    #req({rvs$dataImported == 1})
    namesInFrame <- names(testData)
    #source("ComboDragDrops.R",local = TRUE)
    print("build variable selections interface")
    print(input$analysisType)
    print(testData)
    ### for LCx/Count, all vars are numeric
    numericVars <- which(unlist(lapply(testData,is.numeric)))
    if(length(numericVars)<3)alerID <- shinyalert(
      title = "Error",
      text = "LCx analysis requires three numeric variables (dose, response count, and group size), but fewer are present.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){shinyjs::js$reset()}
    )
    
    #drop variables with negative values because those cannot be valid doses
    #but, what about missing values here?
    numericVars <- numericVars[which(colSums(testData[,numericVars,drop=FALSE]<0,na.rm = TRUE)==0)]
    #counts are integers
    integerVars <- numericVars[which(sapply(numericVars,FUN=function(i){
      all(round(testData[,i])==testData[,i])
    }))]
    uniqueCounts <- unlist(lapply(testData,FUN = function(x)length(unique(x))))
    #| max(uniqueCounts)<nrow(testData)
    if(length(numericVars)==0){
      alerID <- shinyalert(
        title = "Error",
        text = "No valid numeric data found (check for zero and negative values?).  Tool will Reset.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE,
        callbackR = function(x){js$reset()}
      )
      #stop("There is a problem with your data (no numeric or every var has duplicates)")
    }
    if(length(integerVars)<2){
      alerID <- shinyalert(
        title = "Error",
        text = paste(
          "Count data must contain two integer-value variables",
          "(Group size and response counts).  Tool will Reset."),
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE,
        callbackR = function(x){js$reset()}
      )
      #stop("There is a problem with your data (no numeric or every var has duplicates)")
    }
    # dose could be integer, but should be one of numeric vars with most unique values
    print(c(numericVars=length(numericVars),integerVars=length(integerVars)))
    if((length(numericVars)-length(integerVars))>0){
      ###if there are non-integer variables, select dose among them
      nonInteger <- numericVars[!is.element(numericVars,integerVars)]
      bestDose <- (names(testData)[nonInteger])[which(uniqueCounts[nonInteger]==max(uniqueCounts[nonInteger]))]
      bestDose <- bestDose[1]
    }else{###otherwise, numeric and integer are the same, so just choose the first one with most levels
      bestDose <- (names(testData)[numericVars])[which(uniqueCounts[numericVars]==max(uniqueCounts[numericVars]))]
      bestDose <- bestDose[1]
    }
    print(c(bestDose==bestDose))
    bestResp <- (names(testData)[integerVars])[which.max(uniqueCounts[integerVars])]
    bestSize <- (names(testData)[integerVars])[which.min(uniqueCounts[integerVars])]
    bestResp <- bestResp[1]
    bestSize <- bestSize[1]
    output$varSelects <- renderUI({
        tagList(
          selectInput(inputId="sort_x","Exposure concentration:",choices = names(testData)[numericVars],selected = bestDose),
          selectInput(inputId="sort_y","Response count:",choices = names(testData)[integerVars],selected = bestResp),
          selectInput(inputId="sort_z","Group size:",choices = names(testData)[integerVars],selected = bestSize))
      })
    

    req(!is.null(rvs$finalDF))
    print("FinalDF is found, set reactives for LCX")
    if(length(unique(c(input$sort_x,input$sort_y,input$sort_z)))<3)
      output$graphOpts <- renderUI({
        tagList(
          h3("Resolve duplicate variables above",style="color: red"))})
    
    if(all(c("doses","responses","sizes") %in% names(rvs$finalDF)) &
       length(unique(c(input$sort_x,input$sort_y,input$sort_z)))==3){
      #rvs$Countdata <- 1
      rvs$dataChecked <- 1
      
    }
  })
  
  observe({
    req(input$analysisType=="Continuous")
    req(rvs$dataImported == 1 & rvs$dataChecked == 0 & rvs$varsChecked == 0)# & is.null(rvs$finalDF))
    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    #shiny::req(getData())
    testData <- rvs$inputDF
    #req({rvs$dataImported == 1})
    namesInFrame <- names(testData)
    #source("ComboDragDrops.R",local = TRUE)
    print("build variable selections interface")
    print(input$analysisType)
    print(testData)
    ### for BV, all vars are numeric
    numericVars <- which(unlist(lapply(testData,is.numeric)))
    if(length(numericVars)<2)alerID <- shinyalert(
      title = "Error",
      text = "BV analysis requires two numeric variables (dose and response), but fewer are present.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){shinyjs::js$reset()}
    )
    
    #drop variables with negative values because those cannot be valid doses
    #but, what about missing values here?
    numericVars <- numericVars[which(colSums(testData[,numericVars,drop=FALSE]<0,na.rm = TRUE)==0)]
    #counts are integers
    uniqueCounts <- unlist(lapply(testData,FUN = function(x)length(unique(x))))
    #| max(uniqueCounts)<nrow(testData)
    if(length(numericVars)==0){
      alerID <- shinyalert(
        title = "Error",
        text = "No valid numeric data found (check for zero and negative values?).  Tool will Reset.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE,
        callbackR = function(x){js$reset()}
      )
      #stop("There is a problem with your data (no numeric or every var has duplicates)")
    }
    
      #stop("There is a problem with your data (no numeric or every var has duplicates)")

    # dose could be integer, but should be one of numeric vars with most unique values
    print(c(numericVars=length(numericVars)))
    bestDose <- (names(testData)[numericVars])[which(uniqueCounts[numericVars]==min(uniqueCounts[numericVars]))]
    bestDose <- bestDose[1]

    print(c(bestDose==bestDose))
    bestResp <- (names(testData)[numericVars])[which.max(uniqueCounts[numericVars])]
    
    bestResp <- bestResp[1]
    
    output$varSelects <- renderUI({
      tagList(
        selectInput(inputId="sort_x","Exposure concentration:",choices = names(testData)[numericVars],selected = bestDose),
        selectInput(inputId="sort_y","Response:",choices = names(testData)[numericVars],selected = bestResp),
        )
    })
    
    
    req(!is.null(rvs$finalDF))
    print("FinalDF is found, set reactives for BV")
    if(length(unique(c(input$sort_x,input$sort_y)))<2)
      output$graphOpts <- renderUI({
        tagList(
          h3("Resolve duplicate variables above",style="color: red"))})
    
    if(all(c("doses","responses") %in% names(rvs$finalDF)) &
       length(unique(c(input$sort_x,input$sort_y)))==2){
      #rvs$Countdata <- 1
      rvs$dataChecked <- 1
      
    }
  })
  
  observe({
    print("NULL out run button until ready")
    req(is.element(input$analysisType,c("SSD","Count", "Continuous")))
    req(rvs$dataImported == 1 & rvs$dataChecked == 0)

    ### Can't get here unless it is SSD because of the req() above,
    ### so I dont think this if() ever does anything?
    if(!is.element(input$analysisType,c("SSD","Count", "Continuous"))){
      output$scaleSelect <- NULL
      output$varLabels <- NULL
      output$downloadExcel <- NULL
      output$downloadPDF <- NULL
      output$Excelbutton <- NULL
      output$PDFbutton <- NULL
      output$setupButton <- NULL
      output$SSD.1.1 <- NULL
      output$SSD.1.2 <- NULL
      output$SSD.1.3 <- NULL
      #output$SSD.2.1 <- NULL
      #output$SSD.2.2 <- NULL
      #output$SSDoptshead <- NULL
      output$SSDconditional2 <- NULL
    }
    
    ### but it will null out the run until ready...
    output$runButton <- NULL

  })



  observe({
    req(input$analysisType=="SSD")

    req(rvs$dataImported == 1 & rvs$dataChecked == 1 & !is.null(rvs$finalDF))
    print("After SSD var selections made, adjust names/checks.")
    
    output$varLabels <- renderUI({
      tagList(
        textInput("xLab",label="Effect Measure (eg, 'NOEC value')",value="NOEC"),
        textInput("units",label="Units (eg, 'mg/L')",value="mg/L")
      )
    })
    output$downloadExcel <- downloadHandler(
      filename = "SSD Analysis.xlsx",
      content = function(file){
        file.copy("SSDoutput.xlsx", file)
      }
    )

    output$downloadPDF <- downloadHandler(
      filename = "SSD Analysis.pdf",
      content = function(file){
        file.copy("SSDplotOutput.pdf", file)
      }
    )

    output$Excelbutton <- renderUI({
      req(rvs$setupComplete == 1)
      req(rvs$AnalysisComplete == 1)
      #if (input$AnalysisComplete==0)return(actionButton("dummyButton","DUMMY"))
      if (rvs$AnalysisComplete==1)return(downloadBttn('downloadExcel', 'Excel Results',"fill"))
    })

    
    output$PDFbutton <- renderUI({
      req(rvs$setupComplete == 1)
      req(rvs$AnalysisComplete == 1)
      if (rvs$AnalysisComplete==1)return(downloadBttn('downloadPDF', 'PDF Results',"fill"))
    })

    output$setupButton <- renderUI({
      req(rvs$dataChecked==1)
      shinyWidgets::actionBttn(
        inputId="previewData",
        label = "Preview",
        icon = icon("check"),
        style = "pill",
        color = "danger",
        size = "md",
        block = FALSE,
        no_outline = TRUE
      )
    })





    output$scaleSelect <- renderUI({
            #radioButtons("doseScale",label = "Exposure Scale",
            #       choices = c("Measured","Log"),inline = TRUE,selected = "Log")
            radioButtons("doseScale",label = "Analysis Scale",
                         choices = c("Log10"),inline = TRUE,selected = "Log10")
    })

    output$runButton <- renderUI({
      req(rvs$setupComplete == 1)
      shinyWidgets::actionBttn(
        inputId="runAnalysis",
        label = "Run Analysis",
        icon = icon("rocket"),
        style = "pill",
        color = "success",
        size = "md",
        block = FALSE,
        no_outline = TRUE
      )
      #actionButton("runAnalysis", "Run Analysis",class = "btn-primary btn-lg",icon = icon("circle-empty-play",lib = "glyphicon"))
    })

  })


  observe({
    req(input$analysisType=="Count")
    
    req(rvs$dataImported == 1 & rvs$dataChecked == 1 & !is.null(rvs$finalDF))
    print("After LCx var selections made, adjust names/checks.")
    
    output$varLabels <- renderUI({
      tagList(
        textInput("xLab",label="Test Concentration (eg, 'Concentration')",value="Concentration"),
        textInput("units",label="Units (eg, 'mg/L')",value="mg/L")
      )
    })
    output$downloadExcel <- downloadHandler(
      filename = "LCx Analysis.xlsx",
      content = function(file){
        file.copy("LCxoutput.xlsx", file)
      }
    )
    
    output$downloadPDF <- downloadHandler(
      filename = "LCx Analysis.pdf",
      content = function(file){
        file.copy("LCxplotOutput.pdf", file)
      }
    )
    
    output$Excelbutton <- renderUI({
      req(rvs$setupComplete == 1)
      req(rvs$AnalysisComplete == 1)
      #if (input$AnalysisComplete==0)return(actionButton("dummyButton","DUMMY"))
      if (rvs$AnalysisComplete==1)return(downloadBttn('downloadExcel', 'Excel Results',"fill"))
    })
    
    
    output$PDFbutton <- renderUI({
      req(rvs$setupComplete == 1)
      req(rvs$AnalysisComplete == 1)
      if (rvs$AnalysisComplete==1)return(downloadBttn('downloadPDF', 'PDF Results',"fill"))
    })
    
    output$setupButton <- renderUI({
      req(rvs$dataChecked==1)
      shinyWidgets::actionBttn(
        inputId="previewData",
        label = "Preview",
        icon = icon("check"),
        style = "pill",
        color = "danger",
        size = "md",
        block = FALSE,
        no_outline = TRUE
      )
    })

    
    
    
    output$scaleSelect <- renderUI({
      #radioButtons("doseScale",label = "Exposure Scale",
      #       choices = c("Measured","Log"),inline = TRUE,selected = "Log")
      radioButtons("doseScale",label = "Analysis Scale",
                   choices = c("Log10"),inline = TRUE,selected = "Log10")
    })
    
    output$runButton <- renderUI({
      req(rvs$setupComplete == 1)
      shinyWidgets::actionBttn(
        inputId="runAnalysis",
        label = "Run Analysis",
        icon = icon("rocket"),
        style = "pill",
        color = "success",
        size = "md",
        block = FALSE,
        no_outline = TRUE
      )
      #actionButton("runAnalysis", "Run Analysis",class = "btn-primary btn-lg",icon = icon("circle-empty-play",lib = "glyphicon"))
    })
    
  })
  
  observe({
    req(input$analysisType=="Continuous")
    
    req(rvs$dataImported == 1 & rvs$dataChecked == 1 & !is.null(rvs$finalDF))
    print("After BV var selections made, adjust names/checks.")
    
    output$varLabels <- renderUI({
      tagList(
        textInput("xLab",label="Test Concentration (eg, 'Concentration')",value="Concentration"),
        textInput("units",label="Units (eg, 'mg/L')",value="mg/L")
      )
    })
    
    output$respLabels <- renderUI({
      tagList(
        textInput("yLab",label="Test Response (eg, 'Weight')",value="Response"),
        textInput("y.units",label="Units (eg, 'mg')",value="mg")
      )
    })
    
    output$downloadExcel <- downloadHandler(
      filename = "BV Analysis.xlsx",
      content = function(file){
        file.copy("BVoutput.xlsx", file)
      }
    )
    
    output$downloadPDF <- downloadHandler(
      filename = "BV Analysis.pdf",
      content = function(file){
        file.copy("BVplotOutput.pdf", file)
      }
    )
    
    output$Excelbutton <- renderUI({
      req(rvs$setupComplete == 1)
      req(rvs$AnalysisComplete == 1)
      #if (input$AnalysisComplete==0)return(actionButton("dummyButton","DUMMY"))
      if (rvs$AnalysisComplete==1)return(downloadBttn('downloadExcel', 'Excel Results',"fill"))
    })
    
    
    output$PDFbutton <- renderUI({
      req(rvs$setupComplete == 1)
      req(rvs$AnalysisComplete == 1)
      if (rvs$AnalysisComplete==1)return(downloadBttn('downloadPDF', 'PDF Results',"fill"))
    })
    
    output$setupButton <- renderUI({
      req(rvs$dataChecked==1)
      shinyWidgets::actionBttn(
        inputId="previewData",
        label = "Preview",
        icon = icon("check"),
        style = "pill",
        color = "danger",
        size = "md",
        block = FALSE,
        no_outline = TRUE
      )
    })
    
    
    
    
    output$scaleSelect <- renderUI({
      #radioButtons("doseScale",label = "Exposure Scale",
      #       choices = c("Measured","Log"),inline = TRUE,selected = "Log")
      radioButtons("doseScale",label = "Analysis Scale",
                   choices = c("Log10"),inline = TRUE,selected = "Log10")
    })
    
    output$runButton <- renderUI({
      req(rvs$setupComplete == 1)
      shinyWidgets::actionBttn(
        inputId="runAnalysis",
        label = "Run Analysis",
        icon = icon("rocket"),
        style = "pill",
        color = "success",
        size = "md",
        block = FALSE,
        no_outline = TRUE
      )
      #actionButton("runAnalysis", "Run Analysis",class = "btn-primary btn-lg",icon = icon("circle-empty-play",lib = "glyphicon"))
    })
    
  })
  
    
  # This section responds to the selection of input analysisType.
  # It puts the proper template into a new slot of input, called "Selected"
  # It also puts header strings into DF that are used in the datatable view

  observeEvent(input$previewData,{
    print("Preview Data has been clicked")
    req(rvs$finalDF)

    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    testData <- rvs$finalDF
    print("Data at point of clicking preview")
    print(testData)
    previewFile <- tempfile(fileext = ".png",tmpdir = "www")
    namesInFrame <- names(testData)

    # Do additional processing required for plotting data
    if(input$analysisType=="Count" |
       input$analysisType=="SK" |
       input$analysisType=="BMD"){
      #print("check1")
      #print(testData)
      print(namesInFrame)
      print(names(testData))
      if(!all(namesInFrame %in% c("doses","responses","sizes"))){
        print("assign std names")
        req(input$sort_x,input$sort_y,input$sort_z)
        oldNames <- c(input$sort_x,input$sort_y,input$sort_z)
        print(oldNames)
        testData <- testData[,oldNames]
        names(testData) <- c("doses","responses","sizes")
      }
      #print(str(testData))
      testData$logDose <- log10(testData$doses)
      doseSpacing <- median(diff(testData$logDose[testData$doses>0]),na.rm = TRUE)
      testData$logDose[testData$doses==0] <- min(testData$logDose[testData$doses>0],na.rm = TRUE)-1.5*doseSpacing
      testData$yVals <- with(testData,responses/sizes)
      print("Data after adding helper vars")
      print(testData)
    }

    if(input$analysisType=="Continuous"){
      #print("check1")
      #print(testData)
      print(namesInFrame)
      print(names(testData))
      if(!all(namesInFrame %in% c("doses","responses"))){
        print("assign std names")
        req(input$sort_x,input$sort_y)
        oldNames <- c(input$sort_x,input$sort_y)
        print(oldNames)
        testData <- testData[,oldNames]
        names(testData) <- c("doses","responses")
      }
      #print(str(testData))
      testData$logDose <- log10(testData$doses)
      doseSpacing <- median(diff(unique(testData$logDose[testData$doses>0])),na.rm = TRUE)
      testData$logDose[testData$doses==0] <- min(testData$logDose[testData$doses>0],na.rm = TRUE)-2*doseSpacing
      testData$yVals <- with(testData,responses)
    }


    ### Now, do the plots
    # Here, for those with (x,y)-type variable analyses (other than SSD)
    if(input$analysisType=="Count" |
       input$analysisType=="Continuous" |
       input$analysisType=="SK" |
       input$analysisType=="BMD"){
      #remove yVals and logDose columns
      outputTable <- testData[,-(c(-1,0)+ncol(testData))]
      if(!(isolate(input$analysisType)=="Continuous"))
        names(outputTable) <- c("Exposure Concentration","Response Count","Group Size")
      if( (isolate(input$analysisType)=="Continuous"))
        names(outputTable) <- c("Exposure Concentration","Response")
      output$table <- renderTable(outputTable)
      print(str(testData))
      nrow1 <- nrow(testData)
      testData <- na.omit(testData)
      nrow2 <- nrow(testData)
      print(str(testData))
      if(nrow1!=nrow2){
        alerID <- shinyalert(
          title = "Warning",
          text = "One or more rows of data were eliminated for missing values.\nCheck if unexpected.",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      }
      if(FALSE){
        output$basePlot <- renderPlot({
        par(mar=c(5, 7, 0, 0.5) + 0.1)
        if(input$analysisType=="Continuous")yRangeVals <- range(testData$yVals,na.rm = TRUE)
        if(input$analysisType!="Continuous")yRangeVals <- c(0,1)
        png(filename = previewFile,width = input$figW,height = input$figH,units = "in",res = 100)
        if(input$doseScale=="Measured"){
          print("Measured scale, plotSetupGeneric")
          print(testData)
          print(yRangeVals)
          plotSetupGeneric(inputDF=testData,
                           yRange=yRangeVals,
                           cexLAB=input$labelSize,
                           cexAXIS=input$axisSize,
                           cexLWD=input$lineSize,
                           logscaleTF=FALSE,
                           xlabSTR=input$xLab,ylabSTR=input$yLab)
          if(input$analysisType=="Count" |
             input$analysisType=="SK" |
             input$analysisType=="BMD"){
            with(testData,{
              monoY <- isotone::gpava(z=doses,y=yVals,weights=sizes)$x
              points(x=doses,y=monoY,pch=16,col="red")
              monoSpline <- spline(x=doses,y=monoY,method = "hyman",xout = seq(min(doses),max(doses),length=1000))
              lines(monoSpline,col="red")
              abline(h=min(monoSpline$y)+input$ECXvalue*(1-min(monoSpline$y)),lty=3)
            })
          }
        }
        if(input$doseScale=="Log"){
          plotSetupGeneric(inputDF=testData,
                           yRange=yRangeVals,
                           cexLAB=input$labelSize,
                           cexAXIS=input$axisSize,
                           cexLWD=input$lineSize,
                           logscaleTF=TRUE,
                           xlabSTR=input$xLab,ylabSTR=input$yLab)
          if(input$analysisType=="Count" |
             input$analysisType=="SK" |
             input$analysisType=="BMD"){
            with(testData,{
              monoY <- isotone::gpava(z=logDose,y=yVals,weights=sizes)$x
              points(x=10^logDose,y=monoY,pch=16,col="red")
              monoSpline <- spline(x=logDose,y=monoY,method = "hyman",xout = seq(min(logDose),max(logDose),length=1000))
              lines(x=10^(monoSpline$x),y=monoSpline$y,col="red")
              abline(h=min(monoSpline$y)+input$ECXvalue*(1-min(monoSpline$y)),lty=3)
            })
          }
        }
      })
      }
    }

    # And separately for SSD
    if(input$analysisType=="SSD"){
      #when preview is clicked, reset compete flag
      #helps with multiple runs?
      rvs$AnalysisComplete <- 0
      print("SSD plotting")
      #print(testData)
      if(!all(namesInFrame %in% c("species","responses")) & FALSE){
        print("assign std names")
        req(input$sort_x,input$sort_y)
        oldNames <- c(input$sort_x,input$sort_y)
        print(oldNames)
        testData <- testData[,oldNames]
        names(testData) <- c("species","responses")
      }
      #req(input$doGrays)
      if(input$doGrays){
        lineColors <<- c(rgb(0,0,0),rgb(2/3,2/3,2/3),rgb(1/3,1/3,1/3))
        if(input$doGrps){
          seqVals <- seq(.1,.75,length=length(unique(testData$groups)))
          colorList <<- rgb(seqVals,seqVals,seqVals)
          pchSolids <<- c(rep(15:17,length=length(unique(testData$groups))),18)
          pchOpens <<- c(rep(0:2,length=length(unique(testData$groups))),5)
          #add black on the end
          colorList <<- c(colorList,rgb(0,0,0))
        }
        if(!input$doGrps){
          colorList <<- c(rgb(0.5,0.5,0.5),rgb(0,0,0))
          pchSolids <<- c(16,18)
          pchOpens <<- c(1,5)
        }
      }
      if(!input$doGrays){
        lineColors <<- c("blue","cyan")
        if(input$doGrps){
          colorList <<- c(RColorBrewer::brewer.pal(name = "Set1",n=length(unique(testData$groups))),rgb(0,0,0))
          pchSolids <<- c(rep(16,length(unique(testData$groups))),18)
          pchOpens <<- c(rep(1,length(unique(testData$groups))),5)
        }
        if(!input$doGrps){
          colorList <<- c("gray","black")
          pchSolids <<- c(16,18)
          pchOpens <<- c(1,5)
        }
      }
      if(!input$doGrps){
        outputData <- testData[,c("species","responses")]
        names(outputData) <- c("Species Label","Effect Concentration")
        print("USE DT TO FORMAT")
        #output$DTtable <- renderDT(outputData)
        roundTo <- floor(log10(min(outputData$Response)))-1
        roundTo <- ifelse(roundTo < 0,abs(roundTo),roundTo)
        outputData$'Effect Concentration' <- round(outputData$'Effect Concentration',digits = roundTo)
        #options below turn off the search field ,options = list(dom = 't'))
        #but, that also disables scroll so not using now.
        outputDT <- as.datatable(formattable(outputData,
                                             #align =c("r","l"),
                                             list(
                                               'Species Label' = formatter("span", style = ~ style(color = "blue",font.style = "italic")),
                                               'Effect Concentration' = formatter("span", style = ~ style(color="green", float="right")))),
                                 class = 'stripe compact',
                                 #escape = FALSE,
                                 options = list(columnDefs = list(list(className = 'dt-right', targets = 2)),
                                   #autoWidth = TRUE,
                                   pageLength = 10, info = FALSE,
                                   lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                   scrollX = TRUE, scrollY = FALSE,
                                   paging = TRUE, ordering = FALSE,
                                   searching = FALSE))
      }
      if( input$doGrps){
        outputData <- testData[,c("species","responses","groups")]
        names(outputData) <- c("Species Label","Effect Concentration","Grouping")
        print("USE DT TO FORMAT")
        #output$DTtable <- renderDT(outputData)
        roundTo <- floor(log10(min(outputData$Response)))-1
        roundTo <- ifelse(roundTo < 0,abs(roundTo),roundTo)
        outputData$'Effect Concentration' <- round(outputData$'Effect Concentration',digits = roundTo)
        #options below turn off the search field ,options = list(dom = 't'))
        #but, that also disables scroll so not using now.
        outputDT <- as.datatable(formattable(outputData,
                                             #align =c("r","l"),
                                             list(
                                               'Species Label' = formatter("span", style = ~ style(color = "blue",font.style = "italic")),
                                               'Effect Concentration' = formatter("span", style = ~ style(color="green", float="right")),
                                               Grouping=formatter("span", style = ~ style(color = colorList[match(outputData$Group,unique(outputData$Group))])))),
                                 class = 'stripe compact',
                                 #escape = FALSE,
                                 options = list(columnDefs = list(list(className = 'dt-right', targets = 2)),
                                   #autoWidth = TRUE,
                                   pageLength = 10, info = FALSE,
                                   lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                   scrollX = TRUE, scrollY = FALSE,
                                   paging = TRUE, ordering = FALSE,
                                   searching = FALSE))
      }
      #%>%
      #  DT::formatRound(columns = 2,digits = roundTo)
      #if(FALSE)outputDT <- DT::datatable(outputDT,
      #                          class = 'row-border stripe hover compact nowrap',
      #                          rownames = FALSE,
      #                          autoHideNavigation = TRUE, escape =FALSE) %>%
      #formatStyle(columns = "Species",
      #            target="cell",
      #            fontWeight = styleEqual(1:nrow(outputData), rep("bold",nrow(outputData)))) %>%
      #DT::formatRound(columns = 2,digits = roundTo)
      output$DTtable <- DT::renderDT(outputDT)

      #this is same as earlier, but change rows to 5 instead of 10 after preview is clicked
      outputDT.Raw <- as.datatable(formattable(rvs$inputDF),
                                   #class = 'row-border stripe hover compact nowrap',
                                   class = 'stripe compact',
                                   #escape = FALSE,
                                   options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                                  #autoWidth = TRUE,
                                                  pageLength = 5, info = FALSE,
                                                  lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                                  scrollX = TRUE, scrollY = FALSE,
                                                  paging = TRUE, ordering = FALSE,
                                                  searching = FALSE))
      #options=list(autoWidth = TRUE,scrollX = FALSE,scrollY = FALSE,searching = FALSE))
      output$DTtableRaw <- DT::renderDT(outputDT.Raw)

      #output$DTtableRaw <- DT::renderDT(rvs$inputDF)
      #datatable(outputData) %>% formatStyle("Species","font-style: italic")
      nrow1 <- nrow(testData)
      testData <- na.omit(testData)
      nrow2 <- nrow(testData)
      if(nrow1!=nrow2){
        alerID <- shinyalert(
          title = "Warning",
          text = "One or more rows of data were eliminated for missing values.\nCheck if unexpected.",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      }
      png(filename = previewFile,width = input$figW,height = input$figH,units = "in",res = 100)
      print("Margins")
      print(c(input$figH*.15, input$figW*.15, 0, input$figW*input$speciesMargin)+0.1)
      par(mai=c(input$figH*.15, input$figW*.15, 0, input$figW*input$speciesMargin)+0.1,
          omi=rep(0,4))

      inputList <- input
      input2plot <- testData
      input2plot$doses <- testData$responses
      input2plot$logDose <- log10(input2plot$doses)
      newLims <- 10^(log10(range(input2plot$responses,na.rm = TRUE))-c(0.1,0)*diff(log10(range(input2plot$responses,na.rm = TRUE))))
      useFIT <- FALSE
      xlimsFORCE <- newLims
      yMaxFORCE <- NULL
      speciesTF <- TRUE # setting this to TRUE effectively is to allow default plotting setup.
      # when FALSE species will NOT be plotted not matter other settings.
      source("SSDplotCodeCDF.R",local = TRUE)
      data4legend <- testData
      xLegend <- 10^quantile(log10(data4legend$responses),prob=input$ECXvalue,type=8)
      if(input$doLegend & input$doGrps)source("addLegend.R",local = TRUE)
      box(which = "figure")
      dev.off()
      output$basePlot <- renderImage({list(src=previewFile,alt="Preview Figure")},deleteFile = TRUE)
    }

    if(input$analysisType=="Count"){
      #when preview is clicked, reset compete flag
      #helps with multiple runs?
      print("LCx preview plotting")
      rvs$AnalysisComplete <- 0  #############
      print("LCx preview plotting")
      #print(testData)
      if(!all(namesInFrame %in% c("doses","responses","sizes")) & FALSE){
        print("assign std names")
        req(input$sort_x,input$sort_y)
        oldNames <- c(input$sort_x,input$sort_y)
        print(oldNames)
        testData <- testData[,oldNames]
        names(testData) <- c("species","responses")
      }
      #req(input$doGrays)
      if(input$doGrays){
        lineColors <<- c(rgb(0,0,0),rgb(2/3,2/3,2/3),rgb(1/3,1/3,1/3))
        colorList <<- c(rgb(0.5,0.5,0.5),rgb(0,0,0))
        pchSolids <<- c(16,18)
        pchOpens <<- c(1,5)
      }
      if(!input$doGrays){
        lineColors <<- c("blue","cyan")
        colorList <<- c("gray","black")
        pchSolids <<- c(16,18)
        pchOpens <<- c(1,5)
      }
      outputData <- testData[,c("doses","responses","sizes")]
      names(outputData) <- c("Exposure Conc","Response Count","Group Size")
      print("USE DT TO FORMAT")
      #output$DTtable <- renderDT(outputData)
      #options below turn off the search field ,options = list(dom = 't'))
      #but, that also disables scroll so not using now.
      outputDT <- as.datatable(formattable(outputData,
                                           #align =c("r","l"),
                                           list(
                                             'Exposure Conc' = formatter("span", style = ~ style(float="right")),
                                             'Response Count' = formatter("span", style = ~ style(float="right")),
                                             'Group Size' = formatter("span", style = ~ style(float="right")))),
                               class = 'stripe compact',
                               #escape = FALSE,
                               options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                 #autoWidth = TRUE,
                                 pageLength = 10, info = FALSE,
                                 lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                 scrollX = TRUE, scrollY = FALSE,
                                 paging = TRUE, ordering = FALSE,
                                 searching = FALSE))
      #%>%
      #  DT::formatRound(columns = 2,digits = roundTo)
      #if(FALSE)outputDT <- DT::datatable(outputDT,
      #                          class = 'row-border stripe hover compact nowrap',
      #                          rownames = FALSE,
      #                          autoHideNavigation = TRUE, escape =FALSE) %>%
      #formatStyle(columns = "Species",
      #            target="cell",
      #            fontWeight = styleEqual(1:nrow(outputData), rep("bold",nrow(outputData)))) %>%
      #DT::formatRound(columns = 2,digits = roundTo)
      output$DTtable <- DT::renderDT(outputDT)
      
      #this is same as earlier, but change rows to 5 instead of 10 after preview is clicked
      outputDT.Raw <- as.datatable(formattable(rvs$inputDF),
                                   #class = 'row-border stripe hover compact nowrap',
                                   class = 'stripe compact',
                                   #escape = FALSE,
                                   options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                                  #autoWidth = TRUE,
                                                  pageLength = 5, info = FALSE,
                                                  lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                                  scrollX = TRUE, scrollY = FALSE,
                                                  paging = TRUE, ordering = FALSE,
                                                  searching = FALSE))
      #options=list(autoWidth = TRUE,scrollX = FALSE,scrollY = FALSE,searching = FALSE))
      output$DTtableRaw <- DT::renderDT(outputDT.Raw)
      
      #output$DTtableRaw <- DT::renderDT(rvs$inputDF)
      #datatable(outputData) %>% formatStyle("Species","font-style: italic")
      nrow1 <- nrow(testData)
      testData <- na.omit(testData)
      nrow2 <- nrow(testData)
      if(nrow1!=nrow2){
        alerID <- shinyalert(
          title = "Warning",
          text = "One or more rows of data were eliminated for missing values.\nCheck if unexpected.",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      }
      #swap these png() if helps with testing.  The temp version is not put into working directory
      #png(filename = "test.png",width = input$figW,height = input$figH,units = "in",res = 100)
      png(filename = previewFile,width = input$figW,height = input$figH,units = "in",res = 100)
      inputList <- input
      input2plot <- testData
      #ONLY SSD for this
      #input2plot$doses <- testData$responses
      #input2plot$logDose <- log10(input2plot$doses)
      newLims <- 10^((range(input2plot$logDose,na.rm = TRUE))-c(0.1,0)*diff((range(input2plot$logDose,na.rm = TRUE))))
      useFIT <- FALSE
      xlimsFORCE <- newLims
      yMaxFORCE <- NULL
      speciesTF <- TRUE # setting this to TRUE effectively is to allow default plotting setup.
      # when FALSE species will NOT be plotted not matter other settings.
      source("LCxplotCode.R",local = TRUE)
      data4legend <- testData
      box(which = "figure")
      with(testData,{
        monoY <- isotone::gpava(z=logDose,y=yVals,weights=sizes)$x
        points(x=10^logDose,y=monoY,pch=16,col="red")
        monoSpline <- spline(x=logDose,y=monoY,method = "hyman",xout = seq(min(logDose),max(logDose),length=1000))
        lines(x=10^monoSpline$x,y=monoSpline$y,col="red")
        print(c(ECXvalue=input$ECXvalue))
        abline(h=input$ECXvalue,lty=3)
      })
      
      dev.off()
      output$basePlot <- renderImage({list(src=previewFile,alt="Preview Figure")},deleteFile = TRUE)
    }
    
    if(input$analysisType=="Continuous"){
      #when preview is clicked, reset compete flag
      #helps with multiple runs?
      print("BV preview plotting")
      rvs$AnalysisComplete <- 0  #############
      print("BV preview plotting")
      #print(testData)
      if(!all(namesInFrame %in% c("doses","responses")) & FALSE){
        print("assign std names")
        req(input$sort_x,input$sort_y)
        oldNames <- c(input$sort_x,input$sort_y)
        print(oldNames)
        testData <- testData[,oldNames]
        names(testData) <- c("species","responses")
      }
      #req(input$doGrays)
      if(input$doGrays){
        lineColors <<- c(rgb(0,0,0),rgb(2/3,2/3,2/3),rgb(1/3,1/3,1/3))
        colorList <<- c(rgb(0.5,0.5,0.5),rgb(0,0,0))
        pchSolids <<- c(16,18)
        pchOpens <<- c(1,5)
      }
      if(!input$doGrays){
        lineColors <<- c("blue","cyan")
        colorList <<- c("gray","black")
        pchSolids <<- c(16,18)
        pchOpens <<- c(1,5)
      }
      outputData <- testData[,c("doses","responses")]
      names(outputData) <- c("Exposure Conc","Response")
      print("USE DT TO FORMAT")
      #output$DTtable <- renderDT(outputData)
      #options below turn off the search field ,options = list(dom = 't'))
      #but, that also disables scroll so not using now.
      outputDT <- as.datatable(formattable(outputData,
                                           #align =c("r","l"),
                                           list(
                                             'Exposure Conc' = formatter("span", style = ~ style(float="right")),
                                             'Response' = formatter("span", style = ~ style(float="right")))),
                               class = 'stripe compact',
                               #escape = FALSE,
                               options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                              #autoWidth = TRUE,
                                              pageLength = 10, info = FALSE,
                                              lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                              scrollX = TRUE, scrollY = FALSE,
                                              paging = TRUE, ordering = FALSE,
                                              searching = FALSE))
      #%>%
      #  DT::formatRound(columns = 2,digits = roundTo)
      #if(FALSE)outputDT <- DT::datatable(outputDT,
      #                          class = 'row-border stripe hover compact nowrap',
      #                          rownames = FALSE,
      #                          autoHideNavigation = TRUE, escape =FALSE) %>%
      #formatStyle(columns = "Species",
      #            target="cell",
      #            fontWeight = styleEqual(1:nrow(outputData), rep("bold",nrow(outputData)))) %>%
      #DT::formatRound(columns = 2,digits = roundTo)
      output$DTtable <- DT::renderDT(outputDT)
      
      #this is same as earlier, but change rows to 5 instead of 10 after preview is clicked
      outputDT.Raw <- as.datatable(formattable(rvs$inputDF),
                                   #class = 'row-border stripe hover compact nowrap',
                                   class = 'stripe compact',
                                   #escape = FALSE,
                                   options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                                  #autoWidth = TRUE,
                                                  pageLength = 5, info = FALSE,
                                                  lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                                  scrollX = TRUE, scrollY = FALSE,
                                                  paging = TRUE, ordering = FALSE,
                                                  searching = FALSE))
      #options=list(autoWidth = TRUE,scrollX = FALSE,scrollY = FALSE,searching = FALSE))
      output$DTtableRaw <- DT::renderDT(outputDT.Raw)
      
      #output$DTtableRaw <- DT::renderDT(rvs$inputDF)
      #datatable(outputData) %>% formatStyle("Species","font-style: italic")
      nrow1 <- nrow(testData)
      testData <- na.omit(testData)
      nrow2 <- nrow(testData)
      if(nrow1!=nrow2){
        alerID <- shinyalert(
          title = "Warning",
          text = "One or more rows of data were eliminated for missing values.\nCheck if unexpected.",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      }
      #swap these png() if helps with testing.  The temp version is not put into working directory
      #png(filename = "test.png",width = input$figW,height = input$figH,units = "in",res = 100)
      png(filename = previewFile,width = input$figW,height = input$figH,units = "in",res = 100)
      inputList <- input
      input2plot <- testData
      #ONLY SSD for this
      #input2plot$doses <- testData$responses
      #input2plot$logDose <- log10(input2plot$doses)
      newLims <- 10^((range(input2plot$logDose,na.rm = TRUE))-c(0.1,0)*diff((range(input2plot$logDose,na.rm = TRUE))))
      useFIT <- FALSE
      xlimsFORCE <- newLims
      yMaxFORCE <- NULL
      speciesTF <- TRUE # setting this to TRUE effectively is to allow default plotting setup.
      # when FALSE species will NOT be plotted not matter other settings.
      source("BVplotCode.R",local = TRUE)
      data4legend <- testData
      box(which = "figure")
      with(testData,{
        monoY <- -(isotone::gpava(z=logDose,y=-yVals)$x)
        #points(x=10^logDose,y=monoY,pch=16,col="red")
        monoSpline <- spline(x=logDose,y=monoY,method = "hyman",xout = seq(min(logDose),max(logDose),length=1000))
        lines(x=10^monoSpline$x,y=monoSpline$y,col="red")
        print(c(ECXvalue=input$ECXvalue))
        abline(h=(1-(input$ECXvalue))*(max(monoSpline$y)),lty=3)
      })
      
      dev.off()
      output$basePlot <- renderImage({list(src=previewFile,alt="Preview Figure")},deleteFile = TRUE)
    }
    
    ### Now, data is ready to go, so set reactive variables
    rvs$setupComplete <- 1
    rvs$indata <- testData
  })

  ### https://community.rstudio.com/t/updating-input-variable-not-triggering-observeevent-if-new-value-is-the-same/57120
  ### https://stackoverflow.com/questions/46732849/shiny-detect-a-change-in-input-with-a-warning-in-ui-r
  ### Not understanding why if input selections are changed, this does not reset setupComplete so the preview needs to be done
  ### again.


  # Reset the "readiness" for analysis when inputs are changed
  observeEvent(input$runAnalysis, {
    #if (!is.null(rvs())) write.csv(rvs(), input$select2, row.names = FALSE)
    req({rvs$setupComplete == 1})
    print("Run button clicked")
    print(c(rvs.setupComplete=rvs$setupComplete))
    if(rvs$setupComplete == 0){
      print("You must preview your data first")
      alerID <- shinyalert(
        title = "Error",
        text = "You must preview your data first to confirm it is ready for analysis.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
    req(rvs$setupComplete == 1)
    #clean up old results files if they exist

    testData <- na.omit(rvs$indata)
    BMRinput <- input$ECXvalue
    gZeroInput <- TRUE
    #save(list=c("testData","BMRinput","gZeroInput"),file = "testData.RData")
    if(isolate(input$analysisType)=="BMD"){
      output$markdown <- renderUI({
        HTML(markdown::markdownToHTML(knit('BMD-shiny.Rmd', quiet = FALSE)))
      })
    }
    if(isolate(input$analysisType)=="SSD"){
      #set a flag that will switch to 1 when
      #SSD analysis is complete
      Nsteps <- 2
      stepProgress <- 0
      if(input$doGrps) Nsteps <- Nsteps + 1
      if(input$doLOO) Nsteps <- Nsteps + 4
      if(input$doAOI) Nsteps <- Nsteps + 3
      withProgress(message = 'SSD Calculations:',
                   detail = 'This may take a while...', value = 0,max=1,
                   expr = {source("SSD.run.code.R",local = TRUE)})

    }
    if(isolate(input$analysisType)=="Count"){
      #set a flag that will switch to 1 when
      #SSD analysis is complete
      Nsteps <- 2
      stepProgress <- 0
      withProgress(message = 'LCx Calculations:',
                   detail = 'This may take a while...', value = 0,max=1,
                   expr = {source("LCx.run.code.R",local = TRUE)})
      
    }
    
    if(isolate(input$analysisType)=="Continuous"){
      #set a flag that will switch to 1 when
      #SSD analysis is complete
      Nsteps <- 2
      stepProgress <- 0
      withProgress(message = 'BV Calculations:',
                   detail = 'This may take a while...', value = 0,max=1,
                   expr = {source("BV.run.code.R",local = TRUE)})
      
    }
  })
})
