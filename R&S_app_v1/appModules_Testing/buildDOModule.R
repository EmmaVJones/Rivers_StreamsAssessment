source('testingDataset.R')

AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 




#Min DO Exceedance Function ###############

##########################  NEED TO DEAL WITH DAILY AVERAGE MIN   ##################################
DO_Assessment <- function(x){ 
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO))%>% #get rid of NA's
    mutate(DOExceedanceMin=ifelse(DO < `Dissolved Oxygen Min (mg/L)`,T,F))%>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    filter(DOExceedance==TRUE) # Only return temp measures above threshold
  
  DO$FDT_DATE_TIME <- as.character(DO$FDT_DATE_TIME2)
  DO <- dplyr::select(DO,-c(DOExceedance,FDT_DATE_TIME2)) # Don't show user column, could be confusing to them
  
  return(DO)
}

# Exceedance Rate DO
exceedance_DO <- function(x, qualifier){
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,DO,LakeStratification)%>% # Just get relevant columns, 
    filter(!is.na(DO))%>% #get rid of NA's
    filter(LakeStratification %in% qualifier)
  ##filter(LakeStratification %in% c("Epilimnion",NA)) # Only assess Epilimnion or NaN (no stratification)
  ##filter(LakeStratification=="Epilimnion") # Only assess Epilimnion
  DO_Assess <- DO_Assessment(x,qualifier)
  DO_results <- assessmentDetermination(DO,DO_Assess,"Dissolved Oxygen","Aquatic Life")
  return(DO_results)
}

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

