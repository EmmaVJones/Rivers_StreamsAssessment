# Run in R 3.5.1
source('global.R')

#assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
#  st_transform( st_crs(4326))
#stationTable <- readRDS('data/BRROsites_ROA.RDS')
#stationTable_sf <- readRDS('data/BRROsites_ROA_sf.RDS')
#conventionals <- read_excel('data/CONVENTIONALS_20171010.xlsx')

shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  ## Data Upload Tab
  stationTable <- reactive({readRDS('data/BRROsites_ROA_sf.RDS')})
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
  
  output$stationTableMissingStations <- DT::renderDataTable({
    req(stationTable())
    # decide which region data was input from
    Region <- unique(stationTable()$Deq_Region)
    z <- filter(conventionals, Deq_Region == 'Blue Ridge') %>%
      distinct(FDT_STA_ID, .keep_all = TRUE) %>%
      filter(FDT_STA_ID %in% stationTable()$FDT_STA_ID) %>%
      select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME)
    DT::datatable(z,rownames = FALSE, options= list(scrollX = TRUE, pageLength = 20, scrollY = "200px", dom='Bt'))
  })
  
  
  ## Watershed Selection Tab
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({assessmentLayer})
  region_filter <- shiny::callModule(dynamicSelect, "DEQregionSelection", the_data, "ASSESS_REG" )
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", region_filter, "Basin" )
  huc6_filter <- shiny::callModule(dynamicSelect, "HUC6Selection", basin_filter, "VAHU6" )
  
  # Station Map
  output$VAmap <- renderLeaflet({
    req(region_filter(), basin_filter(), huc6_filter())
    m <- mapview(basin_filter(),label= basin_filter()$VAHU6, layer.name = 'Basin Chosen',
                 popup= popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG"))) + 
      mapview(huc6_filter(), color = 'yellow',lwd= 5, label= huc6_filter()$VAHU6, layer.name = c('Selected HUC6'),
              popup= popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")))
    m@map %>% setView(st_bbox(huc6_filter())$xmax[[1]],st_bbox(huc6_filter())$ymax[[1]],zoom = 9) })
  
  # Table of Stations within Selected AU
  output$AUstationSummary <- DT::renderDataTable({
    req(region_filter(), basin_filter(), huc6_filter())
    z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      distinct(FDT_STA_ID, .keep_all = TRUE) %>%
      select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = 20, scrollY = "300px", dom='Bt'))  })
  
  
  ## Station Review Tab
  # Show selected AU
  output$selectedAU <- DT::renderDataTable({
    datatable(huc6_filter() %>% st_set_geometry(NULL) %>% select(VAHU6, VaName, Basin),
              rownames = FALSE, options= list(pageLength = 20, scrollY = "35px", dom='Bt'))})
  
  # Pull Conventionals data for selected AU on click
  conventionals_AU <- eventReactive( input$pullAUdata, {
    filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) })
  
  output$stationSelection_ <- renderUI({ req(conventionals_AU())
    selectInput('stationSelection', 'Station Selection', choices = unique(conventionals_AU()$FDT_STA_ID))  })
  
  stationData <- eventReactive( input$stationSelection, {
    filter(conventionals_AU(), FDT_STA_ID %in% input$stationSelection) })
  
  output$stationInfo <- DT::renderDataTable({ req(stationData())
    z <- filter(stationTable(), FDT_STA_ID == input$stationSelection) %>% st_set_geometry(NULL) %>%
      t() %>% as.data.frame() %>% rename(`Station Information` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "200px", dom='Bt'))  })
  
  
  output$stationMap <- renderLeaflet({
    req(stationData())
    point <- select(stationData()[1,],  FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME ) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
    z <- filter(stationTable(), FDT_STA_ID %in% input$stationSelection)
    map1 <- mapview(z,label= 'Snapped WQS Stream Segment', layer.name = 'WQS',
            popup= popupTable(z, zcol=c("FDT_STA_ID","Buffer Distance",'OBJECTID',"GNIS_ID","WATER_NAME"))) + 
     mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'))
    map1@map
  })
  
  #output$table <- renderPrint({
  #  req(stationData())
  #  stationData()  })
  
  
  
})


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




