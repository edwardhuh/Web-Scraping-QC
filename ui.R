fluidPage(
  # App Title
  titlePanel("Webscraping QC Document BuildR")
  
  #Sidebar layout with input and output definitions
  , sidebarLayout(
    sidebarPanel(
      # Input 1: The text files
      fileInput(
        inputId     = "text"
        , label       = "Upload all the text(.txt) files you want to crunch!"
        , multiple    = TRUE
        , accept      = "text/plain"
        , buttonLabel = "Upload"
      )
      
      # Input 2: The data table to modify
      , fileInput(
        inputId     = "table"
        , label       = "Upload the excel file you have been working on!"
        , multiple    = TRUE
        , accept      = c(".xlsx", ".xls")
        , buttonLabel = "Upload"
      )
      
      # Input 3: check for grid
      , checkboxInput("showcausal", label = "Show Causal", value = FALSE)
      
      # Input 4: download button
      , downloadButton(outputId = 'downloadData', label = 'Download')
      
      # The manual
      , p(strong("What am I supposed to do?")
          , br()
          , strong("(BUILD new report from scratch)")
          , br()
          , strong("I just have one / a couple of reports in textfile that need to be put into excel.")
          , br()
          , "Just attach all of them with the first tab."
          , em("You can select multiple files by pressing 'ctrl' as you click on 1+ files, or just drag!")
          , br()
          , strong("(UPDATE existing report)")
          , br()
          , br()
          , strong("I already have an excel file built using Build.R.")
          , br()
          , strong("I just have a couple reports that I want to include for a new, updated report.")
          , br()
          , "Attach the new text files that you want to include in the first attachment."
          , br()
          , "Then, attach the excel file with the second attachment button.")
    )
    
    # Main panel for displaying outputs ---
    , mainPanel(
      # Output 1: summary table
      dataTableOutput(outputId = "summary")
      
      # Output 2: raw table
      , DT::dataTableOutput(outputId = "poop")
      , DT::dataTableOutput(outputId = "causalOutput")
      
      # Output 3: avg Dygraph 1
      , dygraphOutput(outputId = "avgpaneldy")
      
      # Output 4: avg Dygraph 2
      , dygraphOutput(outputId = "avgtripdy")
    )
  )
)
