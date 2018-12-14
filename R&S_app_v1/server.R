# Run in R 3.5.1
source('global.R')

#assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
#  st_transform( st_crs(4326))

#assessmentLayer <- readRDS('data/VAHU6.RDS')
#assessmentLayer_sp <- rgdal::readOGR('GIS','AssessmentRegions_VA84_basins') # use S4 object bc sf didn't play nicely with modules

shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  ## Data Upload Tab
  stationTable <- reactive({readRDS('data/BRROsites_ROA.RDS')})
  # Where I will go after testing
  #stationTable <- reactive({
  #  req(input$stationsTable)
  #  inFile <- input$stationsTable
  #  readRDS(inFile$datapath)
  #})
  
  comments <- reactive({
    req(input$commentFile)
    inFile <- input$commentFile
    read_csv(inFile$datapath)
  })
  
  
  ## Watershed Selection Tab
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({assessmentLayer})
  region_filter <- shiny::callModule(dynamicSelect, "DEQregionSelection", the_data, "ASSESS_REG" )
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", region_filter, "Basin" )
  huc6_filter <- shiny::callModule(dynamicSelect, "HUC6Selection", basin_filter, "VAHU6" )
  
  
  output$table <- renderPrint({
    huc6_filter_sp()
  })
  
  huc6_filter_sp <- reactive({assessmentLayer_sp[assessmentLayer_sp$VAHU6 ==  huc6_filter()$VAHU6,]})
 
  # Station Map
  shiny::callModule(HUCmap, "VAmap", huc6_filter_sp(), assessmentLayer_sp)
 
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




