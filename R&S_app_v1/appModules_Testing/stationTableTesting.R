source('testingDataset.R')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO
stationTable <- read_csv('data/BRRO_Sites_AU_WQS.csv')
conventionals_HUC<- left_join(conventionals, dplyr::select(stationTable, FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3), by='FDT_STA_ID')
x <-filter(conventionals_HUC, FDT_STA_ID %in% '2-JKS033.06')%>% 
  left_join(WQSvalues, by = 'CLASS') #'2-JMS279.41')#
x2 <- filter(conventionals_HUC, FDT_STA_ID %in% '2-JKS028.69')%>% 
  left_join(WQSvalues, by = 'CLASS')

StationTableResults <- cbind(tempExceedances(x), DOExceedances_Min(x), pHExceedances(x),
                             bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'E.COLI', 126, 235),'E.COLI'),
                             bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'ENTEROCOCCI', 35, 104),'ENTEROCOCCI'),
                             metalsExceedances(filter(WCmetals, FDT_STA_ID %in% x$FDT_STA_ID) %>% 
                                                 dplyr::select(`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`), 'WAT_MET'),
                             # Placeholder for water toxics
                             metalsExceedances(filter(Smetals, FDT_STA_ID %in% x$FDT_STA_ID) %>% 
                                                 dplyr::select(`ACENAPHTHENE`:ZINC), 'SED_MET')
                             # Placeholder for sediment toxics
                             # Placeholder for fish metals
                             # Placeholder for fish toxics
                             # Placeholder for Benthic status
)%>%
  select(-ends_with('exceedanceRate'))


z <- cbind(data.frame(StationID = unique(x$FDT_STA_ID)), siteData$StationTableResults) 
datatable(z, extensions = 'Buttons', escape=F, rownames = F, editable = TRUE,
          options= list(scrollX = TRUE, pageLength = nrow(z),
                        # hide certain columns
                        columnDefs = list(list(targets = 6, visible = FALSE)),
                        dom='Bt', buttons=list('copy',
                                               list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                               list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep=''))))) %>% 
  # format cell background color based on hidden column
  formatStyle(c('TEMP_SAMP','TEMP_VIO','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c('DO_SAMP','DO_VIO','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c('PH_SAMP','PH_VIO','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c('E.COLI_SAMP','E.COLI_VIO','E.COLI_STAT'), 'E.COLI_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c('ENTEROCOCCI_SAMP','ENTEROCOCCI_VIO','ENTEROCOCCI_STAT'), 'ENTEROCOCCI_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c('WAT_MET_SAMP','WAT_MET_VIO','WAT_MET_STAT'), 'WAT_MET_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c('SED_MET_SAMP','SED_MET_VIO','SED_MET_STAT'), 'SED_MET_STAT', backgroundColor = styleEqual(c('Review'), c('red')))