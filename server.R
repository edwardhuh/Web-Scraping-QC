function(input, output, session) {
  
  # create the datatable using functions defined in global.R
  getData <- reactive({
    # require there to be at least a text input to run
    req(input$text)
    inText <- input$text
    
    # count of the number of files
    numtext  <- nrow(inText)
    sandwich <- c()
    # should read in each text input for long sandwich (raw scrape)
    
    for (i in 1:numtext) {
      piece <- unlist(
        read.delim(
          file               = input$text[[i, 'datapath']]
          , header           = FALSE
          , sep              = "="
          , stringsAsFactors = FALSE
        )
        , recursive = TRUE
        , use.name  = FALSE
      )
      sandwich <- c(sandwich, piece)
    }
    
    # ------------------------------------------------------------------------
    # Ingredients : sandwich (rawscrape), inputformat, spliter, formatter
    # ------------------------------------------------------------------------
    result      <- spliter(inputformat(sandwich))
    show        <- formatter(result)
    titles      <- names(show)
    names(show) <- trimws(titles)
    
    # ------------------------------------------------------------------------
    # Enforce class for each column
    # ------------------------------------------------------------------------
    number = ncol(show)
    
    # Enforce class for each column
    show[[1]] <- lubridate::mdy_hm(show[[1]])
    show[[2]] <- as.numeric(show[[2]] )
    show[[3]] <- lubridate::ymd(show[[3]])
    k <- 4
    while (k <= number) {
      show[[k]] <- as.numeric(show[[k]])
      k         <- k + 1
    }
    # ------------------------------------------------------------------------
    
    return(dupcheck2(dupcheck(show)))
  })
  
  combineData <- reactive({
    req(input$table)
    show    <- getData()
    inTable <- input$table
    
    # condition 3: when there is also a table in the input file
    # read the file
    numTable <- nrow(inTable)
    for (i in 1:numTable) {
      df <- readxl::read_excel(input$table[[i, 'datapath']], col_names = TRUE)  
    }
    df[[2]] <- lubridate::ymd(df[[2]])
    df      <- dplyr::bind_rows(df, show)
    
    while (sum(duplicated(df$`File Date`) != 0)) {
      index <- anyDuplicated(df$`File Date`)
      
      temp <- df[index,]$`File Date`
      indexes <- grep(temp,df$`File Date`)
      df <- df[-indexes[1],]
    }
    
    # attach the text file to the bottom of the new excel file
    return(df[order(df$`File Date`),])      
  })
  
  # Need this because the output changes depending on input
  getOutput <- reactive({
    if (is.null(input$table)) {
      result = getData()  
    } else if(!is.null(input$table)) {
      result = combineData()
    }
    return(as.data.frame(result))
  })
  
  output$poop <- DT::renderDataTable({
    getOutput()
  })
  
  getSummary <- reactive({
    inTable <- getOutput()
    
    ind     <- sapply(inTable, is.numeric)
    ind     <- as.logical(as.numeric(ind))
    summary <- sapply(subset(inTable, select = ind), my.summary)
    return(summary)
  })
  
  output$summary <- renderDataTable(
    getSummary()
  )
  
  getCausal <- reactive({
    req(input$text)
    inText <- input$text
    
    # count of the number of files
    numtext  <- nrow(inText)
    sandwich <- c()
    # should read in each text input for long sandwich (raw scrape)
    
    for (i in 1:numtext) {
      piece <- unlist(
        read.delim(
          file               = input$text[[i, 'datapath']]
          , header           = FALSE
          , sep              = "="
          , stringsAsFactors = FALSE
        )
        , recursive = TRUE
        , use.name  = FALSE
      )
      sandwich <- c(sandwich, piece)
    }
    source      <- inputformat(sandwich)
    colname <- c("File Name", "Time Period", "QC", "QC Value", "Threshold")
    index <- grep("[[:blank:]]CAUSAL[[:blank:]]", source)
    last <- length(source)
    result <- na.omit(source[index + 1 : last])
    for(i in result){
      blankSpaces <- unlist(strsplit(i, " "))
      
      name <- basename(blankSpaces[1])
      num <- str_locate(name, '\\d{8}')
      time <- substr(name, num[1], num[2])
      
      if (is.na(as.numeric(blankSpaces[5]))) {
        if (blankSpaces[5] == "percentage"){
          qc <- glue("{blankSpaces[3]} {blankSpaces[4]}")
          qcVal <- glue("{blankSpaces[5]} {blankSpaces[6]}")
          thresh <- glue("{blankSpaces[7]} {blankSpaces[8]}")
          return <- c(name, time, qc, qcVal, thresh)
          colname <- cbind(colname, return)
          next
        }
        
        qc <- glue("{blankSpaces[3]} {blankSpaces[4]} {blankSpaces[5]}")
        if(is.na(as.numeric(blankSpaces[7]))) {
          qcVal <- blankSpaces[6] 
          thresh <- glue("{blankSpaces[7]} {blankSpaces[8]} {blankSpaces[9]}")
        } else {
              qcVal <- glue("{blankSpaces[6]} {blankSpaces[7]}")
              thresh <- glue("{blankSpaces[8]} {blankSpaces[9]}")
            }
      } else if (!is.na(as.numeric(blankSpaces[5]))) {
        qc <- glue("{blankSpaces[3]} {blankSpaces[4]}")
        if (is.na(as.numeric(blankSpaces[6]))){
          qcVal <- blankSpaces[5]
          thresh <- glue("{blankSpaces[6]} {blankSpaces[7]} {blankSpaces[8]}")
        } else {
          qcVal <- glue("{blankSpaces[5]} {blankSpaces[6]}")
          thresh <- glue("{blankSpaces[7]} {blankSpaces[8]}")
        }
      }
      return <- c(name, time, qc, qcVal, thresh)
      colname <- cbind(colname, return)
      } #/ end of for loop 

      colnames(colname) <- NULL
      trans <- t(colname)
      cnames <- trans[1, ]
      trans <- trans[-1, ]
      
      return <- as_tibble(trans)
      colnames(return) <- cnames
      
      return(return)
    })
  
  
  
  
  
  output$causalOutput <- renderDataTable({
    req(input$text)
    req(input$showcausal)
    data <- getCausal()
    return(data)
  })
  
  output$avgtripdy <- renderDygraph({
    df     <- getOutput()
    req(nrow(df) > 3)
    
    logicV <- grepl("Average", names(df), ignore.case = TRUE)
    avgdf  <- df[logicV]
    select <- avgdf[,-1:-3]
    data   <- xts(x = select, order.by = df$`File Date`)
    s <- input$poop_rows_selected
    
    # build the graph
    dygraph(data, main = "Averages per Trip", group = "sync") %>%
      dySeries("Average units per trip", label = "Average Units", drawPoints = TRUE, pointSize = 4,  strokeWidth = 3, color = "blue") %>%
      dySeries("Average dollars per trip",label = "Average Dollars", drawPoints = TRUE, pointSize = 4, strokeWidth = 3, color = "red") %>%
      dyLimit(as.numeric(min(select[2])), color = "red") %>%
      dyLimit(as.numeric(max(select[2])), color = "red") %>%
      dyLimit(as.numeric(min(select[1])), color = "blue") %>%
      dyLimit(as.numeric(max(select[1])), color = "blue") %>%
      dyOptions(drawGrid = input$showgrid) 
    # if(!!s){
    #   dygraph(data[s])%>%
    #     dySeries(drawPoints = TRUE, pointsize = 4, color = "black")  
    # }
  }) #/output$avgtripdy
  
  output$avgpaneldy <- renderDygraph({
    df     <- getOutput()
    req(nrow(df) > 3)
    logicV <- grepl("Average", names(df), ignore.case = TRUE)
    avgdf  <- df[logicV]
    select <- avgdf[,-1]
    data   <- xts(x = select[, -3:-4], order.by = df$`File Date`)
    s <- input$poop_rows_selected
    
    dygraph(data, main = "Averages per Panel", group = "sync") %>%
      dySeries("Average units per panelist", label = "Average Units", drawPoints = TRUE, pointSize = 4,  strokeWidth = 3, color = "green") %>%
      dySeries("Average dollars per panelist",label = "Average Dollars", drawPoints = TRUE, pointSize = 4, strokeWidth = 3, color = "purple") %>%
      dyLimit(as.numeric(min(select[2])), color = "purple") %>%
      dyLimit(as.numeric(max(select[2])), color = "purple") %>%
      dyLimit(as.numeric(min(select[1])), color = "green") %>%
      dyLimit(as.numeric(max(select[1])), color = "green") %>%
      dyOptions(drawGrid = input$showgrid)
  }) #/output$avgpaneldy
  
  output$downloadData <- downloadHandler(
    filename    = function() {
      glue("SKB-QC-{Sys.Date()}-{Sys.time()}.xlsx")
    }
    , content     = function(file) {
      write.xlsx(getOutput(), sheetName = "Processed Data", file = file, col.names = TRUE, showNA = TRUE, row.names = FALSE)
      if(input$showcausal){
        write.xlsx(getCausal(), sheetName = "Causal Data", file = file, col.names = TRUE, showNA = TRUE, append = TRUE, row.names = FALSE)
      }
      # comment the line above, and uncomment the line below to download formatted excel.
      # composeFormattedXLSX(file = file, data = getOutput())
    }
    , contentType = "application/xlsx"
  ) #/ downloadHandler
  
}

