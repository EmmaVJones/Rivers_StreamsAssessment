source('testingDataset.R')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO

# Single station data ----------------------------------------------------------------------
AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 
#------------------------------------------------------------------------------------------
#parameterDataset <- bacteriaGeomean
#parameter <- 'ECOLI'

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

#bacteria geomean testing
bacteria1 <- bacteria
bacteria1[1,] <- c('2011-02-02 12:30:00',25)
bacteria1[2,] <- c('2011-02-04 12:30:00',432)
bacteria1[3,] <- c('2011-02-05 12:30:00',555)
bacteria1[4,] <- c('2011-02-06 12:30:00',18)
bacteria1[5,] <- c('2011-02-018 12:30:00',333)
bacteria1[30,] <- c('2013-06-01 15:25:00',25)
bacteria1[31,] <- c('2013-06-04 15:25:00',444)
bacteria1[32,] <- c('2013-06-10 15:25:00',555)
bacteria1[33,] <- c('2013-06-18 15:25:00',666)
bacteria1[34,] <- c('2013-06-22 15:25:00',777)
bacteria1[35,] <- c('2013-06-29 15:25:00',98)

# How bacteria is assessed
bacteria_Assessment_OLD <- function(x){
  bacteria <- dplyr::select(x,FDT_DATE_TIME2,E.COLI)%>% # Just get relavent columns, 
    filter(!is.na(E.COLI)) #get rid of NA's
  # Geomean Analysis (if enough n)
  bacteriaGeomean <- suppressWarnings(mutate(bacteria, SampleDate = format(FDT_DATE_TIME2,"%m/%d/%y"), # Separate sampling events by day
                            previousSample=lag(SampleDate,1),previousSampleECOLI=lag(E.COLI,1)) %>% # Line up previous sample with current sample line
    rowwise() %>% 
    mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
    filter(sameSampleMonth == 0 | is.na(sameSampleMonth)) %>% # keep only rows with multiple samples per calendar month  or no previous sample (NA) to then test for geomean
    # USING CALENDAR MONTH BC THAT'S HOW WRITTEN IN GUIDANCE, rolling 4 wk windows would have been more appropriate
    mutate(sampleMonthYear = paste(month(as.Date(SampleDate,"%m/%d/%y")),year(as.Date(SampleDate,"%m/%d/%y")),sep='/')) %>% # grab sample month and year to group_by() for next analysis
    group_by(sampleMonthYear) %>%
    mutate(geoMeanCalendarMonth = FSA::geomean(as.numeric(E.COLI)), # Calculate geomean
           limit = 126, samplesPerMonth = n()) %>% 
    distinct(sampleMonthYear, .keep_all = T) %>%
    filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) %>% # minimum sampling rule for geomean to apply
    mutate(exceeds = TRUE) %>%
    select(sampleMonthYear, geoMeanCalendarMonth, limit, exceeds, samplesPerMonth))
  geomeanResults <- quickStats(bacteriaGeomean, 'ECOLI') %>% mutate(ECOLI_STAT = recode(ECOLI_STAT, 'Review' = 'Review if ECOLI_VIO > 1' ),
                                                                    `Assessment Method` = 'Old Monthly Geomean')
  
  # Single Sample Maximum Analysis
  bacteriaSSM <- bacteria %>% rename(parameter = !!names(.[2])) %>% # rename columns to make functions easier to apply
    mutate(limit = 235, exceeds = ifelse(parameter > limit, T, F)) # Single Sample Maximum 
  SSMresults <- quickStats(bacteriaSSM, 'ECOLI') %>% mutate(`Assessment Method` = 'Old Single Sample Maximum')
  return( rbind(geomeanResults, SSMresults) )
}

bacteria_Assessment_OLD(x)
