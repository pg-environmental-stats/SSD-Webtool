####                                        #
#### https://github.com/gcarrghub/SSD-Shiny  #
####                                        #

#these are objects that get written to the global environment
cleanUp <- function(){
  globalVars <- c("colorList","lineColors","pchOpens","pchSolids")
  invisible(sapply(globalVars,FUN = function(objname)if(exists(objname,envir = .GlobalEnv))rm(list=objname,envir = .GlobalEnv)))
}
if(FALSE)cleanUp()

####
#### In all cases, should be able to just do the following (as in, the equivalent to sourcing the file)
#### Details given in if(FALSE){} blocks are details details
#### Before running the tool need to check for required packages and install if necessary
#### Typically should only need to do this once per installation of R


if(FALSE){
  #can use this command to download a copy of the data -- only works inside of rstudio
  if(!require("rstudioapi"))install.packages("rstudioapi")
  downloadName <- "BVdemoData.xlsx"
  # select the directory where to put it, even if it is the one that opens first
  download.file("https://github.com/gcarrghub/BV-Shiny/raw/master/BV%20paper%20data.xlsx",
                destfile = paste(rstudioapi::selectDirectory(path = getwd()),downloadName,sep="/"))
  #one-liners that can be shared to launch the tool.  They all source this file, and should work
  #independent of OS, or R session from which it is run (terminal, Rgui, Rstudio).  Links will
  #show contents of this file, when used in a browser
  devtools::source_url("https://raw.github.com/gcarrghub/SSD-Shiny/master/SSDshinyByGIT.R")
  devtools::source_url("https://github.com/gcarrghub/BV-Shiny/blob/master/shinyFromGIT.R?raw=TRUE")
  devtools::source_url("https://raw.githubusercontent.com/gcarrghub/BV-Shiny/master/shinyFromGIT.R?raw=TRUE")
}
#### or in a code-formatted window:
#### https://github.com/gcarrghub/BV-Shiny/blob/master/shinyFromGIT.R
#### From this view, right click on the "Raw" button to save to a local file

#### Example data are at
#### https://github.com/gcarrghub/BV-Shiny/blob/master/BV%20paper%20data.xlsx
#### Click the download button on the page that opens to use it locally

#### For using git/github with Rstudio,
#### see https://happygitwithr.com/, https://www.r-bloggers.com/rstudio-and-github/



### once the packages above are installed
### the tool will run with only these two lines
### as long as chrome or firefox are your default browsers
### this should work in Rstudio, the Rgui, or even a terminal window on mac
library(shiny)
runGitHub(repo = "SSDV3", username = "gcarrghub",launch.browser=TRUE,ref ="main")
#runGitHub(repo = "PGEnvStats", username = "PGEnvStats",launch.browser=TRUE,ref ="main")

if(FALSE){
  ### when private, have to download/share the zip of the repository to local location,
  ### unzip, and then point to that folder:
  runApp(appDir="~/Downloads/SSDV3-main")
}
###   ref="main" is important.  GitHub changed their naming conventions where
###   originally the root archive location was called "master" then changed
###   to "main".  runGitHub currently defaults to "master" so my old tools
###   don't have the ref= argument.  In future, they should probably make it
###   required.  The errors are not intuitive.

if(FALSE){
  ### If Firefox or Chrome are not default browser, open tool with
  ### the following two lines, then copy the http web address
  ### from the resulting basic browser window into chrome or firefox address bar
  ###
  ### At least on mac, instead of copy-paste you could alternatively highlight
  ### the whole address and drag it into an open browser window too
  library(shiny)
  ### NOTE:  This will ONLY work in Rstudio
  runGitHub("SSD-Shiny", "gcarrghub",launch.browser = .rs.invokeShinyWindowViewer)

  ### If something other than Chrome or Firefox is your default browser and you
  ### don't want to change it, another option may be to set a browser option first
  ### as follows:
  library(shiny)
  ### The location in the following two commands may need to be modified to work on
  ### your MSwindows system.  It is up to you to find where chrome.exe or firefox.exe
  ### is located if these don't work.  Use only one of the following two depending on
  ### your preference on windows
  options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")
  options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
  options(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
  ### Use only one of the following two on mac
  options(browser = "/usr/bin/open -a '/Applications/Google Chrome.app'")
  options(browser = "/usr/bin/open -a '/Applications/Firefox.app'")
  #now the app will open in your above selected browser, independent of defaults
  runGitHub("SSD-Shiny", "gcarrghub",launch.browser=TRUE)

  ### it seems that an artifact of messing with browser option is that help pages now
  ### open in the browser too, instead of internal to Rstudio.  To get back to default
  ### behavior reset with this command:
  options(browser = function(url){.Call("rs_browseURL", url)})
}


