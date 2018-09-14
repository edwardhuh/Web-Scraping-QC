# SKB QC Report Builder Documentation


## Introduction

The aim of this application is to create a dashboard that allows anyone to convert large amounts of customer loyalty data into formatted excel sheets of data in an intuitive and reliable manner. 
The simplicity of the application was made possible because of a [Shiny Application](http://lnx1532.infores.com/pdedw/prd/skb_qc_builder%28demo%29/).
I recommend you look click on this highlight to see the basic structure of the application prior to proceeding to the next chapter. 



## Instruction
These instructions are meant to help guide a person completely unfamiliar with this tool. But by no means is this the only way of working with this tool. The idea is to develop mstery with the foundations, and adapt this tool into your workflow. Don’t let the tool decide how your work needs to be done! If there are parts that you find overly pedantic, please feel free to skip steps.


### Step 0. Launch the application. Open your Outlook (or any other email handling device)
The [application](http://lnx1532.infores.com/pdedw/prd/skb_qc_builder%28demo%29/) should look like this:
If this is not what you are seeing, jump to the section Common Bugs.
![FirstScreen](/img/Original.JPG)
<If you already have the data that you want to generate your reports with, skip to Step 2>


### Step 1. Convert your email report into a text file (.txt).
You can download your emails. In doing so, you can also select the format of your download. Select drop down option of Text Only. The resulting saved file should have the extension of .txt, or `Type` as `Text Document` in your folder. Save it in a directory (a location on your computer) that you know you can easily access. I recommend the `My Documents` folder. 
The title of the document will automatically be the title of your email. Make sure you time stamp (add the date to the filename) for easy identification in Step 2! 

Do you have multiple emails, and want to combine all of them into a single text file? Go to the Tricks and Tips at the end of this section to learn more!


### Step 2. Generate your first report
After you have saved the report from your email into a text file, go to the [application](http://lnx1532.infores.com/pdedw/prd/skb-qc-builder%28demo%29/). 
Press the `Upload` button in the red circled area. This should show you all the text files saved in your folders.
![SecondScreen](/img/Circled.JPG)
Navigate your way to the folder you saved your reports in. Then attach them.
You can upload multiple text files at once! Do so by dragging on multiple files with your mouse. Or hold down the `Ctrl` button as you click the files.

### Step 3: Observe your Causal Data
Do you have CAUSAL data in your text files that you would like to explore, alongside the regular QC data?
If not, move to Step 4.
If yes, please check this box that is highlighted in purple:
![CausalScreen](/img/Circled Causal.JPG)


### Step 4: Observe your data in the dashboard. If you like it, download it!
You can do so by clicking on the Yellow box that is circled in the below image: 
![DownloadScreen](/img/DOWNLOad.JPG)
Note that the changes made in the dashboard will NOT be reflected in the excel file.




## Tricks and Tips
### Saving multiple email contents into a single text file

Make sure to time stamp your files, especially if your file consists of data from consecutive weeks.




## Common Bugs
### Nothing is happening when I click `download`!
1. Have you uploaded the files in its proper location? If you do not upload any text files as mentioned in Step 2 of the instructions, this could cause the application to not function.
2. The application may be struggling to attach the different reports you have fed it. Check if there are any columns that are omitted or included among the different reports. If there is a file that has more columns than others, make sure you change the file name so that it is read first by the program (an easy way to do this is adding a `0` in front of the file name).

Technical note: if the problem persists, observe the way the columns are being type-casted. The application can handle bindings that are in same order, but cannot handle permutations. 
To change this behavior, reference `server.R` from lines 36 - 50
````r

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
````
The current manner assumes that the first and third entry has class ‘Date’. Change this recursive algorithm to ensure that the appropriate columns are saved as Date, and the remainder are saved as numeric. 


### The summary statistics is not showing proper values.

Note, the standard deviation is only calculated when more than one entry is given. The standard deviation for one value will always be `0`.
The summary statistics was designed to always round its values. If this behavior needs to be changed, please reference the `my.summary` function in `global.R` (lines 140- 150).
````r

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
````
Taking out the `round` in front of all these elements will change that behavior.


### Nothing is coming up on the dashboard after uploading my files!

First, be patient. Just because the bar beneath your `Upload` has reached completion does not mean that the application has completed crunching the data. Larger data sets will require greater amounts of time.
If the problem persists, ensure that you are inputting text files in the first upload, and an excel file in the second upload. The application cannot guarentee its behavior for other file formats than what it is designed to be fed, so it may cause errors.

    