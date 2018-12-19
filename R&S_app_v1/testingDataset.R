# Run in R 3.5.1
conventionals <- suppressWarnings(read_csv('data/CONVENTIONALS_20171010.csv'))
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
stationTable <- read_csv('data/BRRO_Sites_AU_WQS.csv')


conventionals_HUC<- filter(conventionals, Huc6_Vahu6 %in% 'JM01') %>%
  left_join(dplyr::select(stationTable, FDT_STA_ID, ID305B_1, ID305B_2, ID305B_3), by='FDT_STA_ID')
AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')
stationData <-filter(conventionals_HUC, FDT_STA_ID %in% '2-JMS279.41')

