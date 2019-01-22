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
library(RColorBrewer)
library(FSA)
library(lubridate)
library(magrittr)

# Bring in modules
source('appModules/multipleDependentSelectizeArguments.R')
source('newBacteriaStandard_working.R')

modulesToReadIn <- c('temperature','pH','DO','SpCond','Salinity','TN','Ecoli','chlA','Enteroccoci', 'TP','sulfate')
for (i in 1:length(modulesToReadIn)){
  source(paste('appModules/',modulesToReadIn[i],'Module.R',sep=''))
}


# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}

#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct(c("2013-01-01 00:00:00 UTC","2018-12-31 23:59:59 UTC"),tz='UTC')
assessmentCycle <- '2020'
##################################################################################################################


WQSvalues <- tibble(CLASS = c('I',"II","II","III","IV","V","VI","VII"),
                    `Description Of Waters` = c('Open Ocean', 'Tidal Waters in the Chowan Basin and the Atlantic Ocean Basin',
                                                'Tidal Waters in the Chesapeake Bay and its tidal tributaries',
                                                'Nontidal Waters (Coastal and Piedmont Zone)','Mountainous Zone Waters',
                                                'Stockable Trout Waters','Natural Trout Waters','Swamp Waters'),
                    `Dissolved Oxygen Min (mg/L)` = c(5,4,NA,4,4,5,6,NA),
                    `Dissolved Oxygen Daily Avg (mg/L)` = c(NA,5,NA,5,5,6,7,NA),
                    `pH Min` = c(6,6,6.0,6.0,6.0,6.0,6.0,3.7),
                    `pH Max` = c(9.0,9.0,9.0,9.0,9.0,9.0,9.0,8.0),
                    `Max Temperature (C)` = c(NA, NA, NA, 32, 31, 21, 20, NA))



withinAssessmentPeriod <- function(x){
  if((range(unique(x$FDT_DATE_TIME2))[1] < assessmentPeriod[1]) | 
     (range(unique(x$FDT_DATE_TIME2))[2] > assessmentPeriod[2])){
    print('Data included that falls outside of assessment period. Review input data.')
  }else{print('All input data falls within the assessment period.')}
}

# Super Assessment function
assessmentDetermination <- function(parameterDF,parameterAssessmentDF,parameter,use){
  
  results <- data.frame(nSamples = nrow(parameterDF),nExceedance = nrow(parameterAssessmentDF))%>%
    mutate(exceedanceRate = (nExceedance/nSamples)*100)
  
  if(results$exceedanceRate > 10.5 & results$nSamples > 10){outcome <- paste('Water impaired for',parameter)}
  if(results$exceedanceRate < 10.5 & results$nSamples > 10){outcome <- paste('Water not impaired for',parameter)}
  if(results$nExceedance >= 2 & results$nSamples < 10){outcome <- paste('Water impaired for',parameter)}
  if(results$nExceedance < 2 & results$nSamples < 10){outcome <- paste('Water not impaired for',parameter)}
  
  results <- mutate(results,Assessment=outcome, Use= use)
  return(results)
}
#assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")



#### Temperature Assessment Functions ---------------------------------------------------------------------------------------------------

#Max Temperature Exceedance Function
temp_Assessment <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    mutate(TemperatureExceedance=ifelse(FDT_TEMP_CELCIUS > `Max Temperature (C)`,T,F))%>% # Identify where above max Temperature, 
    filter(TemperatureExceedance==TRUE) # Only return temp measures above threshold
  return(temp)
}

# Exceedance Rate Temperature
exceedance_temp <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS,`Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS)) #get rid of NA's
  temp_Assess <- temp_Assessment(x)
  
  temp_results <- assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")
  return(temp_results)
}


#### pH Assessment Functions ---------------------------------------------------------------------------------------------------

pH_rangeAssessment <- function(x){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,`pH Min`,`pH Max`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    rowwise()%>% mutate(interval=findInterval(FDT_FIELD_PH,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(pHrange=ifelse(interval==1,T,F))%>% # Highlight where pH doesn't fall into assessment range
    filter(pHrange==FALSE)%>% # Only return pH measures outside of assessement range
    dplyr::select(-c(interval,pHrange)) # Don't show user interval column, could be confusing to them, T/F in pHrange column sufficient
  return(pH)
}


exceedance_pH <- function(x){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,`pH Min`,`pH Max`)%>% # Just get relavent columns, 
    filter(!is.na(FDT_FIELD_PH)) #get rid of NA's
  pH_rangeAssess <- pH_rangeAssessment(x)
  pH_results <- assessmentDetermination(pH, pH_rangeAssess,"pH","Aquatic Life")
  return(pH_results)
}


#### DO Assessment Functions ---------------------------------------------------------------------------------------------------

# DO exceedance function
DO_Assessment_Min <- function(x){ 
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    mutate(DOExceedanceMin=ifelse(DO < `Dissolved Oxygen Min (mg/L)`,T,F))%>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    filter(DOExceedanceMin==TRUE) %>% # Only return DO measures below threshold
    dplyr::select(-c(DOExceedanceMin,FDT_DATE_TIME2,`Dissolved Oxygen Daily Avg (mg/L)`)) # Don't show user column, could be confusing to them
}

# Daily Average exceedance function
DO_Assessment_DailyAvg <- function(x){ 
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% #get rid of NA's
    mutate(date = as.Date(FDT_DATE_TIME2, format="%m/%d/%Y")) %>% 
    group_by(date) %>%
    mutate(n_Samples_Daily = n()) %>% # how many samples per day?
    filter(n_Samples_Daily > 1) %>%
    mutate(DO_DailyAverage = mean(DO), DOExceedanceDailyAvg=ifelse(DO_DailyAverage < `Dissolved Oxygen Daily Avg (mg/L)`,T,F)) %>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    ungroup() %>% 
    filter( DOExceedanceDailyAvg==TRUE) %>% # Only return DO measures below threshold
    dplyr::select(-c(FDT_DATE_TIME2, `Dissolved Oxygen Min (mg/L)`,date))
}

# Exceedance Rate DO, for all samples
exceedance_DO <- function(x){
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) #get rid of NA's
  DO_Assess <- DO_Assessment_Min(x)
  DO_results <- assessmentDetermination(DO,DO_Assess,"Dissolved Oxygen","Aquatic Life")
  return(DO_results)
}

# Exceedance Rate DO, for daily average samples
exceedance_DO_DailyAvg <- function(x){
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% #get rid of NA's
    mutate(date = as.Date(FDT_DATE_TIME2, format="%m/%d/%Y")) %>% 
    group_by(date) %>%
    mutate(n_Samples_Daily = n()) %>% # how many samples per day?
    filter(n_Samples_Daily > 1)  # only keep days with > 1 sample 
  DO_Assess <- suppressMessages(DO_Assessment_DailyAvg(x))
  DO_results <- assessmentDetermination(DO %>% distinct(date),DO_Assess,"Dissolved Oxygen Daily Average","Aquatic Life")
  return(DO_results)
}


#### E.coli OLD Assessment Functions ---------------------------------------------------------------------------------------------------

bacteria_ExceedancesGeomeanOLD <- function(x, bacteriaType, geomeanLimit){
  suppressWarnings(mutate(x, SampleDate = format(FDT_DATE_TIME2,"%m/%d/%y"), # Separate sampling events by day
                          previousSample=lag(SampleDate,1),previousSampleBacteria=lag(get(bacteriaType),1)) %>% # Line up previous sample with current sample line
                     rowwise() %>% 
                     mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
                     filter(sameSampleMonth == 0 | is.na(sameSampleMonth)) %>% # keep only rows with multiple samples per calendar month  or no previous sample (NA) to then test for geomean
                     # USING CALENDAR MONTH BC THAT'S HOW WRITTEN IN GUIDANCE, rolling 4 wk windows would have been more appropriate
                     mutate(sampleMonthYear = paste(month(as.Date(SampleDate,"%m/%d/%y")),year(as.Date(SampleDate,"%m/%d/%y")),sep='/')) %>% # grab sample month and year to group_by() for next analysis
                     group_by(sampleMonthYear) %>%
                     mutate(geoMeanCalendarMonth = FSA::geomean(as.numeric(get(bacteriaType))), # Calculate geomean
                            limit = geomeanLimit, samplesPerMonth = n()))
}

bacteria_ExceedancesSTV_OLD <- function(x, STVlimit){                                    
  x %>% rename(parameter = !!names(.[2])) %>% # rename columns to make functions easier to apply
    mutate(limit = STVlimit, exceeds = ifelse(parameter > limit, T, F)) # Single Sample Maximum 
}

bacteria_Assessment_OLD <- function(x, bacteriaType, geomeanLimit, STVlimit){
  bacteria <- dplyr::select(x,FDT_DATE_TIME2,bacteriaType)%>% # Just get relavent columns, 
    filter(!is.na(get(bacteriaType))) #get rid of NA's
  # Geomean Analysis (if enough n)
  bacteriaGeomean <- bacteria_ExceedancesGeomeanOLD(bacteria, bacteriaType, geomeanLimit) %>%     
    distinct(sampleMonthYear, .keep_all = T) %>%
    filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) %>% # minimum sampling rule for geomean to apply
    mutate(exceeds = TRUE) %>%
    select(sampleMonthYear, geoMeanCalendarMonth, limit, exceeds, samplesPerMonth)
  geomeanResults <- quickStats(bacteriaGeomean, bacteriaType) %>%
    mutate(`Assessment Method` = 'Old Monthly Geomean')
  geomeanResults[,4] <- recode(geomeanResults[,4], 'Review' = paste('Review if ', bacteriaType,'_VIO > 1',sep=''))
  
  # Single Sample Maximum Analysis
  bacteriaSSM <- bacteria_ExceedancesSTV_OLD(bacteria, STVlimit) 
  SSMresults <- quickStats(bacteriaSSM, bacteriaType) %>% mutate(`Assessment Method` = 'Old Single Sample Maximum')
  return( rbind(geomeanResults, SSMresults) )
}

conventionalsToBacteria <- function(x, bacteriaType){
  z <- dplyr::select(x, FDT_STA_ID, FDT_DATE_TIME2, bacteriaType) %>%
    rename(ID = FDT_STA_ID, `Date Time` = FDT_DATE_TIME2, Value = bacteriaType) %>%
    filter(!is.na(Value))
  z$`Date Time` <- as.Date(z$`Date Time`)
  z$Value <- as.numeric(z$Value)
  return(z)
}






quickStats <- function(parameterDataset, parameter){
  results <- data.frame(SAMP = nrow(parameterDataset),
                        VIO = nrow(filter(parameterDataset, exceeds == TRUE))) %>%
    mutate(exceedanceRate = (VIO/SAMP)*100)
  
  if(results$exceedanceRate > 10.5 & results$SAMP > 10){outcome <- 'Review'}
  if(results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
  if(results$VIO >= 2 & results$SAMP < 10){outcome <- 'Review'}
  if(results$VIO < 2 & results$SAMP < 10){outcome <- 'Review'}
  
  results <- mutate(results, STAT = outcome)
  names(results) <- c(paste(parameter,names(results)[1], sep = '_'),
                      paste(parameter,names(results)[2], sep = '_'),
                      paste(parameter,names(results)[3], sep = '_'),
                      paste(parameter,names(results)[4], sep = '_'))
  #rename based on parameter entered
  return(results)
}

#Max Temperature Exceedance Function
tempExceedances <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    rename(parameter = !!names(.[2]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above max Temperature, 
  
  quickStats(temp, 'TEMP')
}
#tempExceedances(x)

pHExceedances <- function(x){
  pH <- dplyr::select(x,FDT_DATE_TIME,FDT_FIELD_PH,`pH Min`,`pH Max`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    rowwise() %>% mutate(interval=findInterval(FDT_FIELD_PH,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(exceeds=ifelse(interval == 1, F, T)) # Highlight where pH doesn't fall into assessment range
  
  quickStats(pH, 'PH')
}
#pHExceedances(x)


DOExceedances_Min <- function(x){
  DO <- dplyr::select(x,FDT_DATE_TIME2,DO,`Dissolved Oxygen Min (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    rename(parameter = !!names(.[2]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter < limit, T, F)) # Identify where below min DO 
  
  quickStats(DO, 'DO')
}
#DOExceedances_Min(x)