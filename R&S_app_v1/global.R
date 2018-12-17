library(shiny)
library(shinyjs)
library(leaflet)
library(mapview)
library(tidyverse)
library(sf)
library(plotly)
#library(raster)
library(DT)
library(readxl)
#library(FSA)

# Bring in modules
source('appModules/multipleDependentSelectizeArguments.R')
#source('appModules/map_mapview_sf.R')


# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}

#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct(c("2013-01-01 00:00:00 UTC","2018-12-31 23:59:59 UTC"),tz='UTC')
##################################################################################################################

withinAssessmentPeriod <- function(x){
  #print(unique(x$FDT_DATE_TIME))
  
  if((range(unique(x$FDT_DATE_TIME2))[1] < assessmentPeriod[1]) | 
     (range(unique(x$FDT_DATE_TIME2))[2] > assessmentPeriod[2])){
    print('Data included that falls outside of assessment period. Review input data.')
  }else{print('All input data falls within the assessment period.')}
}
