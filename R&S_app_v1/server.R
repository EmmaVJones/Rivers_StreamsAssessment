# Run in R 3.5.1
source('global.R')
source('AUshapefileLocation.R')

assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
stationTable <- read_csv('data/BRRO_Sites_AU_WQS.csv')
#stationTable <- readRDS('data/BRROsites_ROA_sf.RDS')
conventionals <- suppressWarnings(read_csv('data/CONVENTIONALS_20171010.csv'))
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

mapviewOptions(basemaps = c( "OpenStreetMap",'Esri.WorldImagery'),
               vector.palette = colorRampPalette(brewer.pal(8, "Set1")),
               na.color = "magenta",
               legend=FALSE)



shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  # Reactive Value to store all user data
  userData <- reactiveValues()
  
  ## Data Upload Tab
  stationTable <- reactive({read_csv('data/BRRO_Sites_AU_WQS.csv')})#readRDS('data/BRROsites_ROA_sf.RDS')})
  # Where I will go after testing
  #stationTable <- reactive({
  #  req(input$stationsTable)
  #  inFile <- input$stationsTable
  #  readRDS(inFile$datapath)
  #})
  
  comments <- reactive({
    req(input$commentFile)
    inFile <- input$commentFile
    read_csv(inFile$datapath) })
  observe(userData$comments <- comments())
  
##### NEEED TO FIX #######################################################################  
  #output$stationTableMissingStations <- DT::renderDataTable({
  #  req(stationTable())
  #  # decide which region data was input from
  #  Region <- unique(stationTable()$Deq_Region)
  #  z <- filter(conventionals, Deq_Region == 'Blue Ridge') %>%
  #    distinct(FDT_STA_ID, .keep_all = TRUE) %>%
  #    filter(FDT_STA_ID %in% stationTable()$FDT_STA_ID) %>%
  #    select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME)
  #  DT::datatable(z,rownames = FALSE, options= list(scrollX = TRUE, pageLength = 20, scrollY = "200px", dom='Bt'))
  #})
############################################################################################  
  
  ## Watershed Selection Tab
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({assessmentLayer})
  region_filter <- shiny::callModule(dynamicSelect, "DEQregionSelection", the_data, "ASSESS_REG" )
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", region_filter, "Basin" )
  huc6_filter <- shiny::callModule(dynamicSelect, "HUC6Selection", basin_filter, "VAHU6" )
  AUs <- reactive({req(huc6_filter(), regionalAUs)
    suppressWarnings(st_intersection(st_zm(regionalAUs), huc6_filter()))})
 
  
  # Station Map
  output$VAmap <- renderLeaflet({
    req(region_filter(), basin_filter(), huc6_filter())
    m <- mapview(basin_filter(),label= basin_filter()$VAHU6, layer.name = 'Basin Chosen',
                 popup= popupTable(basin_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG"))) + 
      mapview(huc6_filter(), color = 'yellow',lwd= 5, label= huc6_filter()$VAHU6, layer.name = c('Selected HUC6'),
              popup= popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")))
    m@map %>% setView(st_bbox(huc6_filter())$xmax[[1]],st_bbox(huc6_filter())$ymax[[1]],zoom = 9) })
  
  # Table of AUs and Stations within Selected VAHU6
  output$AUSummary <-  DT::renderDataTable({ req(regionalAUs,AUs())
    DT::datatable(AUs() %>% st_set_geometry(NULL), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUs()), scrollY = "300px", dom='Bt')) 
  })
  
  output$stationSummary <- DT::renderDataTable({
    req(region_filter(), basin_filter(), huc6_filter())
    z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      distinct(FDT_STA_ID, .keep_all = TRUE) %>%
      select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='Bt'))  })
  
  observeEvent(input$reviewAUs,{
    showModal(modalDialog(
      title="Preview Assessment Units for Selected VAHU6",
      leafletOutput('AUmap'),
      easyClose = TRUE))  })
  
  output$AUmap <- renderLeaflet({
    req(region_filter(), basin_filter(), huc6_filter())
    m <- mapview(huc6_filter(), color = 'yellow',lwd= 5, label= NULL, layer.name = c('Selected HUC6'),
                 popup= popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG"))) + 
      mapview(AUs(), label= AUs()$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", legend=FALSE,
              popup= popupTable(AUs(), zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" )))
    m@map })
  
  ## Station Review Tab
  # Show selected AU
  output$selectedHUC <- DT::renderDataTable({
    datatable(huc6_filter() %>% st_set_geometry(NULL) %>% select(VAHU6, VaName, Basin),
              rownames = FALSE, options= list(pageLength = 20, scrollY = "35px", dom='Bt'))})
  
  # Pull Conventionals data for selected AU on click
  conventionals_HUC <- eventReactive( input$pullHUCdata, {
    z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      left_join(dplyr::select(stationTable(), FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3), by='FDT_STA_ID')})
  
  output$AUSelection_ <- renderUI({ req(conventionals_HUC())
    selectInput('AUSelection', 'Assessment Unit Selection', choices = conventionals_HUC()$ID305B_1)  })
  
  output$selectedAU <- DT::renderDataTable({req(conventionals_HUC(),input$AUSelection)
    z <- filter(regionalAUs, ID305B %in% input$AUSelection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "200px", dom='Bt'))})

  output$stationSelection_ <- renderUI({ req(conventionals_HUC(), input$AUSelection)
    z <- filter(conventionals_HUC(), ID305B_1 %in% input$AUSelection | 
                   ID305B_2 %in% input$AUSelection | 
                   ID305B_2 %in% input$AUSelection) %>%
      distinct(FDT_STA_ID)
    selectInput('stationSelection', 'Station Selection', choices = unique(z$FDT_STA_ID))  })
  
  AUData <- eventReactive( input$AUSelection, {
    filter(conventionals_HUC(), ID305B_1 %in% input$AUSelection | 
             ID305B_2 %in% input$AUSelection | 
             ID305B_2 %in% input$AUSelection) }) 
  
  stationData <- eventReactive( input$stationSelection, {
    filter(conventionals_HUC(), FDT_STA_ID %in% input$stationSelection) })
  
  output$stationInfo <- DT::renderDataTable({ req(stationData())
    z <- filter(stationTable(), FDT_STA_ID == input$stationSelection) %>% 
      t() %>% as.data.frame() %>% rename(`Station Information` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "200px", dom='Bt'))  })
  
  output$stationMap <- renderLeaflet({
    req(stationData())
    point <- select(stationData()[1,],  FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:ID305B_3 ) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
    segment <- filter(regionalAUs, ID305B %in% as.character(point$ID305B_1) |
                        ID305B %in% as.character(point$ID305B_2) |
                        ID305B %in% as.character(point$ID305B_3))
    map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                    popup= popupTable(segment, zcol=c("ID305B","MILES","CYCLE","WATER_NAME"))) + 
      mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'),
              popup=NULL)
    map1@map
  })
  
  #### Data Sub Tab
  
  # Display Data
  output$AURawData <- DT::renderDataTable({ AUData()
    DT::datatable(AUData(), extensions = 'Buttons', escape=F, rownames = F,
                  options= list(scrollX = TRUE, pageLength = nrow(AUData()), scrollY = "300px", 
                                dom='Btf', buttons=list('copy',
                                                        list(extend='csv',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                        list(extend='excel',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')))))})
  # Summarize data
  output$stationDataTableRecords <- renderText({
    req(AUData())
    paste(nrow(AUData()), 'records were retrieved for',as.character(input$AUSelection),sep=' ')})
  output$uniqueStationDataTableRecords <- renderTable({
    req(AUData())
    plyr::count(AUData(), vars = c("FDT_STA_ID"))%>%dplyr::rename('Number of Records'='freq')})
  output$stationDataTableAssessmentWindow <- renderText({
    req(AUData())
    withinAssessmentPeriod(AUData())})
  
  #### Temperature Sub Tab
  
  
})


#output$table <- renderPrint({
#  req(stationData())
#  stationData()  })



# Station Map, didn't use module bc could not figure it out with sf 
#output$VAmap <- renderLeaflet({
#  names(st_geometry(assessmentLayer)) = NULL

#  leaflet(assessmentLayer) %>% setView(-79.2,37.7,zoom=7)%>%
#    addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
#    addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
#    addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
#    addPolygons(data=assessmentLayer,color='yellow',fill=0.1,stroke=0.1,group="Virginia HUC6",
#                popup = popupTable(assessmentLayer,zcol=c(1,2,5,13,22)))
#    addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
#                     overlayGroups=c("Virginia HUC6"),
#                     options=layersControlOptions(collapsed=T),
#                     position='topleft')%>%
#    mapview::addMouseCoordinates(style='basic')
#  
#})




