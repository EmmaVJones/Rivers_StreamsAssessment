source('testingDataset.R')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO
stationTable <- read_csv('data/BRRO_Sites_AU_WQS.csv')
conventionals_HUC<- left_join(conventionals, dplyr::select(stationTable, FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3), by='FDT_STA_ID')
x <-filter(conventionals_HUC, FDT_STA_ID %in% '2-JKS033.06')%>% 
  left_join(WQSvalues, by = 'CLASS') #'2-JMS279.41')#
x2 <- filter(conventionals_HUC, FDT_STA_ID %in% '2-JKS028.69')%>% 
  left_join(WQSvalues, by = 'CLASS')

StationTableResults <- cbind(tempExceedances(x), DOExceedances_Min(x), pHExceedances(x),
                             bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'E.COLI', 126, 235),'E.COLI') %>% 
                               dplyr::rename('ECOLI_VIO' = 'E.COLI_VIO', 'ECOLI_SAMP'='E.COLI_SAMP', 'ECOLI_STAT'='E.COLI_STAT'),
                             bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'ENTEROCOCCI', 35, 104),'ENTEROCOCCI') %>% 
                               dplyr::rename('ENTER_VIO' = 'ENTEROCOCCI_VIO', 'ENTER_SAMP'='ENTEROCOCCI_SAMP', 'ENTER_STAT'='ENTEROCOCCI_STAT'),
                             
                             metalsExceedances(filter(WCmetals, FDT_STA_ID %in% x$FDT_STA_ID) %>% 
                                                 dplyr::select(`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`), 'WAT_MET'),
                             acuteNH3exceedance(x) %>% 
                               dplyr::select(AcuteAmmonia_VIO, AcuteAmmonia_STAT) %>% 
                               dplyr::rename('WAT_TOX_VIO' ='AcuteAmmonia_VIO','WAT_TOX_STAT' = 'AcuteAmmonia_STAT'),#data.frame(WAT_TOX_VIO='Not Analyzed by App', WAT_TOX_STAT='Not Analyzed by App'),# Placeholder for water toxics
                             
                            # Placeholder for water toxics
                             metalsExceedances(filter(Smetals, FDT_STA_ID %in% x$FDT_STA_ID) %>% 
                                                 dplyr::select(`ACENAPHTHENE`:ZINC), 'SED_MET'),
                             
                             data.frame(SED_TOX_VIO='Not Analyzed by App', SED_TOX_STAT='Not Analyzed by App'),# Placeholder for sediment toxics
                             data.frame(FISH_MET_VIO='Not Analyzed by App', FISH_MET_STAT='Not Analyzed by App'), # Placeholder for fish metals
                             data.frame(FISH_TOX_VIO='Not Analyzed by App', FISH_TOX_STAT='Not Analyzed by App'),# Placeholder for fish toxics
                             benthicAssessment(x,conventionals_sf,VSCI,VCPMI),
                             countTP(x),
                             countchla(x),
                             #data.frame(NUT_TP_VIO='Not Analyzed by App',NUT_TP_SAMP= 'Not Analyzed by App', NUT_TP_STAT='Not Analyzed by App'), # Placeholder bc only applies to Lakes or Cbay
                             #data.frame(NUT_CHLA_VIO='Not Analyzed by App', NUT_CHLA_SAMP='Not Analyzed by App', NUT_CHLA_STAT='Not Analyzed by App'),# Placeholder bc only applies to Lakes or Cbay
                             data.frame(COMMENTS= 'Not Analyzed by App') # Assessor Comments
                             )%>%
  dplyr::select(-ends_with('exceedanceRate'))

x1 <- filter(conventionals_sf, FDT_STA_ID %in% x$FDT_STA_ID)#'2-JKS033.06') #'2-JMS279.41')##
if(nrow(x1) >0){
  x2 <- benthicResultMetrics(x1,VSCI,VCPMI)$data
  if (!is.na(x2)){return(data.frame(BENTHIC_STAT='Review'))
  }else{return(data.frame(BENTHIC_STAT=NA))}
} else{return(data.frame(BENTHIC_STAT=NA))}
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
  formatStyle(c('ECOLI_SAMP','ECOLI_VIO','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c('ENTER_SAMP','ENTER_VIO','ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c('WAT_MET_SAMP','WAT_MET_VIO','WAT_MET_STAT'), 'WAT_MET_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c('SED_MET_SAMP','SED_MET_VIO','SED_MET_STAT'), 'SED_MET_STAT', backgroundColor = styleEqual(c('Review'), c('red')))