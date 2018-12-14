# Map 
HUCmapUI <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns('test')),
    leafletOutput(ns("map"), height =400, width = 650))
}

HUCmap <- function(input, output, session, userDataset, userDataset_sp){
  ns <- session$ns
  
  HUC6 <- reactive({userDataset_sp[userDataset_sp$VAHU6== userDataset()$VAHU6,]})
  
  output$test <- renderTable({
    HUC6()
  })
  
  
  output$map <- renderLeaflet({
      
    leaflet(userDataset_sp) %>% setView(-79.2,37.7,zoom=7)%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      #addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      #addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
      addPolygons(data=HUC6(),color='yellow',fillColor="orange",fillOpacity = 1, 
                  opacity=1, weight=2, stroke=TRUE, group="Selected VAHU6",
                  popup=popupTable(HUC6()@data,zcol=c(1,2))) %>%
    
      addPolygons(data=userDataset_sp,color='black',fillColor="gray",fillOpacity = 0.1, 
                  opacity=1, weight=2, stroke=TRUE, group="Virginia VAHU6",
                       popup=popupTable(userDataset_sp,zcol=c(1,2))) %>% hideGroup("Virginia VAHU6") %>%
      #addCircleMarkers(data=lakeStations_shp,radius=3,color='orange',fillColor="orange",fillOpacity = 1,stroke=0,
      #                 group="All Lake Monitoring Stations",layerId=~lakeStations_shp@data$DD_LAT,
      #                 popup=popupTable(lakeStations_shp@data,zcol=c(1,6,7,13,14,16)))%>%hideGroup('All Lake Monitoring Stations')%>%
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                       overlayGroups=c('Selected VAHU6','Virginia HUC6'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      #addHomeButton(extent(lakeStations_shp),"All Lake Monitoring Stations")%>%
      mapview::addMouseCoordinates(style='basic')
  })
}
