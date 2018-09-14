# --------------------------------------------------------------------------
# Shiny App development for SKB QC 
# Function : Global.R
# By: Edward Huh
# UPDATE: 9/10/2018 - taking care of causal. streamline code 
# --------------------------------------------------------------------------

library("shiny")
library("data.table")
library("xlsx")
library("glue")
library("purrr")
library("dygraphs")
library("xts")
library("stringr")
library("dplyr")
library("tidyverse")
library("readr")
library("qpcR")
library("DT")

long.line = "----------------------------------------"

## Function 1: inputformat - the scraped content has commas(",") in numbers (ex: 2,300).
##             need to take out this comma for type conversion.
inputformat <- function(input.vector) {
  input.vector <- gsub(pattern = ",", "", input.vector)
}

## Function 2: spliter - sorts the long scraped text into organized table
spliter <- function(input.vector) {
  i              <- 1
  count          <- sum((input.vector == long.line), na.rm = TRUE)
  dateIndex      <- grep("Sent:+", input.vector, ignore.case = TRUE)
  fromCount      <- length(dateIndex)
  dateInfo       <- unlist(strsplit(unlist(input.vector[dateIndex[1]]), "\t"))
  dateInfo       <- dateInfo[2]
  dateInfo_delay <-  NA
  
  while (count > 0) {
    index          <- match(long.line, input.vector)
    important.part <- tail(input.vector, (-1 * index))
    count          <- count - 1
    # if there is only input files in the table
    if (count == 0) {
      useful.part <- important.part
    } else {
      nextLine    <- match(long.line, important.part)
      useful.part <- head(important.part, (-1 * length(important.part)) + (nextLine - 1))
    }
    
    # format the data prior to framing
    if (sum(grepl("From:+", useful.part)) != 0) {
      # need to have a delayed dateInfo
      dateIndex      <- grep("Sent+", useful.part)
      dateInfo_delay <- unlist(strsplit(useful.part[dateIndex], "\t"))
      dateInfo_delay <- dateInfo_delay[2]
      
      ending         <- nextLine - 1
      remove         <- important.part[ grep("From:+", important.part):ending] 
      important.part <- important.part[!important.part %in% remove] #remove the part that is causing concern
      #reassing useful.part
      nextLine    <- match(long.line, important.part)
      useful.part <- head(important.part, (-1 * length(important.part)) + (nextLine - 1))
    }
    if (grepl("CAUSAL", useful.part[1])) {
      next
    }
    
    useful.part <- useful.part[!(useful.part == " ")] # eliminate the empty line
    useful.part <- useful.part[!(grepl("skb+", useful.part))]
    useful.part <- useful.part[!(grepl("File Name+", useful.part))]
    
    #useful.part[4] <- as.Date(useful.part[4], format = "%Y %m %d")  should figure out how to store as date.
    num   <- length(useful.part)
    evens <- num / 2
    
    # format the data to frames
    if (i == 1) {
      # get the names
      odd_indexes <- seq(1, num, 2)
      names       <- useful.part[odd_indexes]
      names       <- append(names, "TIME", after = 0)
      names[1]    <- "TIME"
      
      # get the numbers
      title <- match("Delivery Week ", useful.part) + 1
      if (evens == as.integer(evens)) {
        source        <- useful.part[1:evens * 2]
        source        <- append(source, dateInfo, after = 0)
        result        <- cbind.data.frame(names, source)
        storeTitle    <- c("Variable Names", paste0("Week", useful.part[title], ""))
        names(result) <- storeTitle
      } else{
        print("DATA FORMAT ISSUE : input vector ODD")}
    } else {
      source            <- useful.part[1:evens * 2]
      source            <- append(source, dateInfo, after = 0)
      assign(paste("Week", useful.part[title]), source)
      result            <- qpcR:::cbind.na(result, source)
      storeTitle[i + 1] <- paste0("Week", useful.part[title], "")
      names(result)     <- storeTitle
    }
    
    i <- i + 1
    input.vector <- important.part
    
    if (!is.na(dateInfo_delay)) {
      dateInfo <- dateInfo_delay  
    }
  }
  return(result)
}

## Function 3: formatter - translates the input df
formatter <- function(df) {
  trans           <- as_tibble(t(df))
  colnames(trans) <- trans[1, ]
  trans           <- trans[-1, ]
  trans[order(trans$`File Date `),]
}



## check for duplicates (taking out the first column of dates)
dupcheck <- function(df) {
  L  <- !(duplicated(df[, -1]) | duplicated(df[, -1], fromLast = TRUE))
  df <- df[L,]
  return(df)
}
## deletes the divergent values on same date by chronology.
dupcheck2 <- function(show) {
  while (sum(duplicated(show$`File Date`) != 0)) {
    index <- anyDuplicated(show$`File Date`)
    if (show[(index - 1), 1] > show[index, 1]) { 
      show <- show[-index,]
    } else {
      show <- show[-(index - 1),]
    }
  }
  return(show[, -1])
}

## provides the summary information
my.summary <- function(x, na.rm = TRUE) {
  result <- c(
    Mean   = round(mean(x, na.rm = na.rm), digits = 0)
    , SD     = round(sd(x, na.rm = na.rm), digits = 0)
    , Median = round(median(x, na.rm = na.rm), digits = 0)
    , Min    = round(min(x, na.rm = na.rm), digits = 0)
    , Max    = round(max(x, na.rm = na.rm), digits = 0) 
    , N      = round((length(x)), digits = 0))
  
  return(result)
}

## produces a formatted XLSX
composeFormattedXLSX <- function(
  file
  , data
  , sheet_name = "Processed Data"
  , col_names  = TRUE
  , row_names  = FALSE
){
  xlsx::write.xlsx(
    x         = data
    , file      = file
    , sheetName = sheet_name
    , row.names = row_names
    , col.names = col_names
  )
  
  # load in an jobjRef class to operate on
  wb <- loadWorkbook(file)
  
  # extract the rows, not column subsetting here in case additional styles
  # need to be applied to others
  all_rows <- getRows(
    sheet    = getSheets(wb)[[sheet_name]] # get the sheet we just wrote
    , rowIndex = seq(nrow(data)) + 1L        # skip the header if written
  )
  
  # Highlight columns that are zero ---------------------------------------
  
  negStyle <- CellStyle(wb) +
    Font(
      wb = wb
      , color = "red"
    ) +
    Fill(
      backgroundColor = "pink"
      , foregroundColor = "pink"
      , patter = "SOLID_FOREGROUND"
    )
  fmt_col <- which(names(data) %in% names(data))
  
  # determine which cells have the condition specified
  fmt_cells       <- getCells(row = all_rows, colIndex = fmt_col)
  fmt_cells_vals  <- map_dbl(fmt_cells, getCellValue) # use 'map_dbl' because a numeric is returned
  cells_with_cond <- fmt_cells[fmt_cells_vals == 0]
  
  # execute a call with side effects using 'walk'  
  walk(cells_with_cond, setCellStyle, cellStyle = negStyle)
  
  # finally, save the edits to the notebook
  saveWorkbook(wb, file)
}