source('testingDataset.R')

#conventionals_HUC<- filter(conventionals, Huc6_Vahu6 %in% 'JM01') %>%
#  left_join(dplyr::select(stationTable, FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3), by='FDT_STA_ID')

#AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
#  left_join(WQSvalues, by = 'CLASS')

# class III
#x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 

# Class VI
#conventionals_HUC<- filter(conventionals, Huc6_Vahu6 %in% 'JU11') %>%
#  left_join(dplyr::select(stationTable, FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3), by='FDT_STA_ID')

#AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-I04R_JKS03A00' | 
#                   ID305B_2 %in% 'VAW-I04R_JKS04A00' | 
#                   ID305B_2 %in% 'VAW-I04R_JKS04A00')%>% 
#  left_join(WQSvalues, by = 'CLASS')

#x2 <-filter(AUData, FDT_STA_ID %in% '2-JKS030.65') 


# Assessment functions based on WQS Class
# Mike's access queries from WCRAW_2016DATA.mdb
#AMMONIA, TOTAL (MG/L AS N)- 2016 Raw Data
#AMMONIA, TOTAL Com - 2016 Raw Data
#WQS Acute NH3-N: IIf([Field Ph] Is Null Or [AMMONIA, TOTAL (MG/L AS N)] Is Null,Null,IIf([STDCLASS]="5" Or [STDCLASS]="6",(0.275/(1+10^(7.204-[Field Ph])))+(39/(1+10^([Field Ph]-7.204))),IIf([STDCLASS] Is Null,Null,(0.411/(1+10^(7.204-[Field Ph])))+(58.4/(1+10^([Field Ph]-7.204))))))
#NH3-N Assess: IIf([Field Ph] Is Null Or [AMMONIA, TOTAL (MG/L AS N)] Is Null Or [STDCLASS] Is Null,Null,IIf([AMMONIA, TOTAL (MG/L AS N)]>[WQS Acute NH3-N],"EXCEEDS","FS"))

acuteNH3limit <- function(x){
  # Trout absent scenario, freshwater
  if(unique(x$CLASS) %in% c("III","IV")){
    return(dplyr::select(x, FDT_DATE_TIME, FDT_DEPTH, FDT_FIELD_PH, AMMONIA) %>%
      mutate(NH3limit = (0.411/(1+10^(7.204-FDT_FIELD_PH)))+(58.4/(1+10^(FDT_FIELD_PH-7.204)))))  }
  # Trout present scenario, freshwater
  if(unique(x$CLASS) %in% c("V","VI")){
    return(dplyr::select(x, FDT_DATE_TIME, FDT_DEPTH, FDT_FIELD_PH, AMMONIA) %>%
      mutate(NH3limit = (0.275/(1+10^(7.204-FDT_FIELD_PH)))+(39/(1+10^(FDT_FIELD_PH-7.204)))))  }
}

acuteNH3exceedance <- function(x){
  # Trout absent scenario, freshwater
  if(unique(x$CLASS) %in% c("III","IV")){
    ammonia <- acuteNH3limit(x) %>%
      filter(!is.na(AMMONIA)) %>% #get rid of NA's
      rename(parameter = !!names(.[4]), limit = !!names(.[5])) %>% # rename columns to make functions easier to apply
      mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
    return(quickStats(ammonia, 'AcuteAmmonia'))  }
  # Trout present scenario, freshwater
  if(unique(x$CLASS) %in% c("V","VI")){
    ammonia <- acuteNH3limit(x) %>%
      filter(!is.na(AMMONIA)) %>% #get rid of NA's
      rename(parameter = !!names(.[4]), limit = !!names(.[5])) %>% # rename columns to make functions easier to apply
      mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
    return(quickStats(ammonia, 'AcuteAmmonia'))
  }
}


#acuteNH3limit(x)
#acuteNH3exceedance(x)



AmmoniaPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('Ammonia_oneStationSelectionUI')),
      plotlyOutput(ns('Ammoniaplotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All ammonia records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('AmmoniaRangeTableSingleSite')))),
        column(4, h5('Individual ammonia exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               tableOutput(ns("stationAmmoniaExceedanceRate"))))
    )
  )
}


AmmoniaPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$Ammonia_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Ammonia_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  Ammonia_oneStation <- reactive({
    req(ns(input$Ammonia_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$Ammonia_oneStationSelection)})
  
  output$Ammoniaplotly <- renderPlotly({
    req(input$Ammonia_oneStationSelection, Ammonia_oneStation())
    dat <- acuteNH3limit(Ammonia_oneStation()) %>%
      filter(!is.na(AMMONIA)) %>%
      mutate(over=ifelse(AMMONIA > NH3limit, '#D11814', '#535559'))# 'VIOLATION', 'GOOD'))
    dat$SampleDate <- as.POSIXct(as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"), format="%m/%d/%y")
    
    if(nrow(dat) > 0){ 
      plot_ly(data=dat)%>%
        add_markers(data=dat, x= ~SampleDate, y= ~AMMONIA,mode = 'scatter', name="Ammonia (mg/L as N)",marker = list(color= ~over),#'#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Ammonia: ",AMMONIA,"mg/L as N"),
                                                 paste('Acute Ammonia Limit: ',format(NH3limit, digits=3),"mg/L as N"),
                                                 paste('pH: ', FDT_FIELD_PH, '(unitless)')))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Ammonia (mg/L as N)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    }
    
  })
  
  
  
  output$AmmoniaRangeTableSingleSite <- renderTable({
    req(Ammonia_oneStation())
    acuteNH3limit(Ammonia_oneStation()) %>%
      mutate(Exceedance = ifelse(AMMONIA > NH3limit, TRUE, FALSE)) %>%
      filter(Exceedance == TRUE)})
  
  output$stationAmmoniaExceedanceRate <- renderTable({
    req(input$Ammonia_oneStationSelection, Ammonia_oneStation())
    acuteNH3exceedance(Ammonia_oneStation()) %>%
      dplyr::select(1:3) %>%# don't give assessment determination for single station
      dplyr::rename(nSamples = AcuteAmmonia_SAMP,nExceedance= AcuteAmmonia_VIO,exceedanceRate= AcuteAmmonia_exceedanceRate)}) # make it match everything else
  
  
}




ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  AmmoniaPlotlySingleStationUI('Ammonia')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_HUO02A02' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(AmmoniaPlotlySingleStation,'Ammonia', AUData, stationSelected)
  
}

shinyApp(ui,server)
