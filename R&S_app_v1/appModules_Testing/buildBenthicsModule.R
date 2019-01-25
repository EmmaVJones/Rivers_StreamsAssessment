source('testingDataset.R')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO

# Single station data ----------------------------------------------------------------------
conventionals_HUC<- left_join(conventionals, dplyr::select(stationTable, FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3), by='FDT_STA_ID')

AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-I04R_JKS01A00' | 
                   ID305B_2 %in% 'VAW-I04R_JKS01A00' | 
                   ID305B_2 %in% 'VAW-I04R_JKS01A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(conventionals_HUC, FDT_STA_ID %in% '2-JKS023.61')#'2-JKS033.06') #'2-JMS279.41')#
#x2 <- filter(conventionals_HUC, FDT_STA_ID %in% '2-JKS028.69')


# Bring in Level 3 ecoregion
library(sf)
ecoregion <- st_read('data/VA_level3ecoregion.shp')

# add ecoregion to conventionals info
conventionals_sf <- st_as_sf(conventionals_HUC, coords = c("Longitude", "Latitude"), 
                             remove = F, # don't remove these lat/lon cols from df
                             crs = 4326)  # add projection, needs to be geographic bc entering lat/lng

conventionals_HUC <- bind_cols(conventionals_HUC, )
st_within(conventionals_sf, ecoregion)


bind_cols(conventionals_HUC, )

bind_cols(
  centers,
  singapore[as.numeric(st_within(centers_sf, singapore)),]) %>% 
  select(lng, lat, inc_crc, subzone_name=SUBZONE_N) %>% 
  mutate(subzone_name = str_to_title(subzone_name))


# Bring in latest EDAS VSCI and (combined) VCPMI queries
VSCI <- read_excel('data/Family Metrics VSCI Calculation.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110)
VCPMI <- read_excel('data/Family Metrics - CPMI Combined.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110)

x2 <- filter(VSCI, StationID %in% x$FDT_STA_ID) 
x3 <- filter(VCPMI, StationID %in% x$FDT_STA_ID)

# Assessment Functions



# Sampling Metrics functions







BenthicsPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('Benthics_oneStationSelectionUI')),
      plotlyOutput(ns('Benthicsplotly'))  )
  )
}


BenthicsPlotlySingleStation <- function(input,output,session, AUdata){
  ns <- session$ns
  
  # Select One station for individual review
  output$Benthics_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('Benthics_oneStationSelection'),strong('Select Station to Review'),choices=unique(AUdata())$FDT_STA_ID,width='300px')})
  
  Benthics_oneStation <- reactive({
    req(ns(input$Benthics_oneStationSelection))
    if()
    VSCI <- filter(AUdata(),FDT_STA_ID %in% input$Benthics_oneStationSelection)})
  
  output$Benthicsplotly <- renderPlotly({
    req(input$Benthics_oneStationSelection, Benthics_oneStation())
    dat <- Benthics_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    
    maxheight <- ifelse(max(dat$FDT_SPECIFIC_CONDUCTANCE, na.rm=T) < 500, 600, max(dat$FDT_SPECIFIC_CONDUCTANCE, na.rm=T)* 1.2)
    box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(500, maxheight, maxheight, 500))
    box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(350, 500, 500, 350))
    box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(250, 350, 350, 250))
    box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 250, 250, 0))
    
    
    plot_ly(data=dat)%>%
      add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
      add_markers(data=dat, x= ~SampleDate, y= ~FDT_SPECIFIC_CONDUCTANCE,mode = 'scatter', name="Specific Conductivity (uS/cm)",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Specific Conductivity: ",FDT_SPECIFIC_CONDUCTANCE,"uS/cm")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Specific Conductivity (uS/cm)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
}




ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  BenthicsPlotlySingleStationUI('Benthics')
)

server <- function(input,output,session){
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(BenthicsPlotlySingleStation,'Benthics', AUData)
  
}

shinyApp(ui,server)

