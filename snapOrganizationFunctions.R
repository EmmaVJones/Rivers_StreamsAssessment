# function to find sites with +1 segment
snapCheck <- function(successDataFrame){
  successDataFrame %>%
    group_by(`Point Unique Identifier`) %>%
    filter(n()>1)
}


AUselecter <- function(sfLines, sfPoint, i, nObjects) {
  ui <- miniPage(
    gadgetTitleBar("AU Selection", right = miniTitleBarButton("done", "Accept", primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel("Site", icon = icon("mouse-pointer"),#sliders
                   miniContentPanel(
                     uiOutput('stationCounter'), br(),hr(),
                     uiOutput('StationID'), br(),
                     radioButtons("auChosen", "Choose the correct assessment unit for the site", 
                                  choices = sfLines$ID305B, selected = NULL)#,
                     #miniButtonBlock(
                     #  actionButton("chooseAU", "Choose AU"))
                   )),
      miniTabPanel("Map", icon = icon("map-o"),
                   miniContentPanel(padding = 0,
                                    leafletOutput("map", height = "100%")
                   )),
      miniTabPanel('Table', icon = icon("table"),
                   miniContentPanel(
                     DT::dataTableOutput("table")))
    )
  )
  
  
  server <- function(input, output, session) {
    
    output$stationCounter <- renderUI({paste('Station Counter:',i, 'of',nObjects)  })
    
    output$StationID <- renderUI({strong(paste('StationID:',unique(sfLines$`Point Unique Identifier`)))})
    
    output$map <- renderLeaflet({
      m <- mapview(sfLines, label= sfLines$ID305B, layer.name = c('AUs snapped to selected site'), 
                   zcol = "ID305B", legend=FALSE,
                   popup= popupTable(sfLines, zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" )),
                   map.types = c("OpenStreetMap","Esri.WorldImagery")) +
        mapview(sfPoint, color = 'yellow',lwd= 5, label= sfPoint$FDT_STA_ID, 
                layer.name = c('Selected Site'),
                popup= popupTable(sfPoint, zcol=c('FDT_STA_ID','STA_DESC')),
                map.types = c("OpenStreetMap","Esri.WorldImagery"))
      m@map 
      
    })
    
    output$table <- DT::renderDataTable({
      z <- sfLines %>% st_set_geometry(NULL)
      DT::datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollY = "290px", dom='t'))
    })
    
    #observeEvent(input$chooseAU, {
    #   userValue <- data.frame(StationID = unique(sfPoint$FDT_STA_ID), ID305B = input$auChosen)
    #})
    
    observeEvent(input$done, {
      userValue <- data.frame(StationID = as.character(unique(sfPoint$FDT_STA_ID)), 
                              ID305B = as.character(input$auChosen))
      stopApp(userValue)
    })
  }
  
  runGadget(shinyApp(ui, server))
  
}



WQSselecter <- function(sfLines, sfPoint, i, nObjects) {
  ui <- miniPage(
    gadgetTitleBar("WQS Selection", right = miniTitleBarButton("done", "Accept", primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel("Site", icon = icon("mouse-pointer"),#sliders
                   miniContentPanel(
                     uiOutput('stationCounter'), br(),hr(),
                     uiOutput('StationID'), br(),
                     radioButtons("wqsChosen", "Choose the correct WQS segment for the site", 
                                  choices = sfLines$OBJECTID, selected = NULL))),
      miniTabPanel("Map", icon = icon("map-o"),
                   miniContentPanel(padding = 0,
                                    leafletOutput("map", height = "100%")
                   )),
      miniTabPanel('Table', icon = icon("table"),
                   miniContentPanel(
                     DT::dataTableOutput("table")))
    )
  )
  
  
  server <- function(input, output, session) {
    
    output$stationCounter <- renderUI({paste('Station Counter:',i, 'of',nObjects)  })
    
    output$StationID <- renderUI({strong(paste('StationID:',unique(sfLines$`Point Unique Identifier`)))})
    
    output$map <- renderLeaflet({
      sfLines <- mutate(sfLines, OBJID2 = as.character(OBJECTID)) # have to duplicate bc mapview doesnt want to lable using sfLines$OBJECTID
      m <- mapview(sfLines, label= sfLines$OBJID2, layer.name = c('WQS segmentss snapped to selected site'), 
                   zcol = "OBJECTID", legend=FALSE,
                   popup= popupTable(sfLines, zcol=c("OBJECTID","WQS_COMMEN","WATER_NAME","SEC","CLASS",'SPSTDS','SECTION_DE',
                                                     'PWS','Trout','StreamType','Tier_III')),
                   map.types = c("OpenStreetMap","Esri.WorldImagery")) +
        mapview(sfPoint, color = 'yellow',lwd= 5, label= sfPoint$FDT_STA_ID, 
                layer.name = c('Selected Site'),
                popup= popupTable(sfPoint, zcol=c('FDT_STA_ID','STA_DESC')),
                map.types = c("OpenStreetMap","Esri.WorldImagery"))
      m@map 
      
    })
    
    output$table <- DT::renderDataTable({
      z <- sfLines %>% st_set_geometry(NULL)
      DT::datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollY = "290px", dom='t'))
    })
    
    observeEvent(input$done, {
      userValue <- data.frame(StationID = as.character(unique(sfPoint$FDT_STA_ID)), 
                              OBJECTID = as.character(input$wqsChosen))
      stopApp(userValue)
    })
  }
  
  runGadget(shinyApp(ui, server))
  
}



basinNameSwitcher <- function(conventionalsName){
  if(conventionalsName == "James River Basin"){return('James')}
  if(conventionalsName == "Roanoke River Basin"){return('Roanoke')}
  if(conventionalsName == "Chowan and Dismal Swamp River Basin"){return('ChowanDismalSwamp')}
  if(conventionalsName == "New River Basin"){return('New')}
  if(conventionalsName == "Shenandoah River Basin"){return('Shenandoah')}
  if(conventionalsName == "Potomac River Basin"){return('Potomac')}
  if(conventionalsName == "Ches. Bay and Small Coastal Basin"){return('ChesBay')}
  if(conventionalsName == "Rappahannock River Basin"){return('Rappahannock')}
  if(conventionalsName == "Tennessee and Big Sandy River Basin"){return('TennesseeBigSandy')}
  if(conventionalsName == "York River Basin"){return('York')}
  if(is.na(conventionalsName)){return(NA)}
}





snapAndOrganizeWQS <- function(AUsnappedSites, WQSfileLocation, basinName, bufferDistances){
  # Bring in WQS for basin
  print('Bringing in appropriate WQS file')
  WQS <- st_read(paste(WQSfileLocation,'updated',basinName,'.shp', sep='')) %>%
    st_transform(crs = 102003) # convert to Albers Equal Area just for snapping
  # snapping logic
  print(paste('Snapping sites to WQS by:',min(bufferDistances),'to', max(bufferDistances), 'meters', sep=' '))
  snapList_WQS <- snap_Points_to_Feature_List(AUsnappedSites,'FDT_STA_ID',WQS, bufferDistances)
  
  if(nrow(snapList_WQS[['sf_output']]) > 0){
    tooMany <- snapCheck(snapList_WQS[['sf_output']])
    
    # perfect sites
    sites <- filter(snapList_WQS[['sf_output']], !(`Point Unique Identifier` %in% tooMany$`Point Unique Identifier`)) %>%
      st_set_geometry(NULL) %>%
      mutate(FDT_STA_ID=`Point Unique Identifier`)
    
    
    # deal with any sites that snapped to too many segments
    if(nrow(tooMany) > 0){
      tooMany <- tooMany %>%  st_transform(4326)# project to WGS84 for plotting
      
      # User fix sites that snapped to too many segments
      siteWithTooMany <- filter(AUsnappedSites, FDT_STA_ID %in% unique(tooMany$`Point Unique Identifier`)) %>%
        st_transform(4326)# project to WGS84 for plotting
      
      # Loop to build gadget for each site that snapped to too many sites
      # empty place to put results, save time on processing and dont put in dataframe immediately bc
      #   looping is already slow enough
      StationID <- as.character(nrow(siteWithTooMany))
      OBJECTID <- as.character(nrow(siteWithTooMany))
      for (i in 1:nrow(siteWithTooMany)){
        zz <- WQSselecter(filter(tooMany, `Point Unique Identifier` %in% siteWithTooMany[i,]), 
                          siteWithTooMany[i,], i , nrow(siteWithTooMany)) 
        StationID[i] <- as.character(zz[1][[1]])
        OBJECTID[i] <- as.character(zz[2][[1]])   }
      
      results  <- data.frame(StationID, OBJECTID, stringsAsFactors = FALSE)
      
      # Combine sites that snapped to a segement perfectly the first time
      results$OBJECTID <- as.integer(results$OBJECTID) # force to interger so join can happen on that column
      results_WQS <- left_join(results, WQS, by='OBJECTID') %>%
        mutate(`Buffer Distance` = 'User Selected', FDT_STA_ID=StationID) %>%
        dplyr::rename(`Point Unique Identifier` = 'StationID') %>%
        dplyr::select(`Point Unique Identifier`, `Buffer Distance`, everything(), -geometry) %>%
        bind_rows(sites)  # add perfect sites
      
      
    } else {
      results_WQS <- sites
    }
  }
  
  # Didn't snap to any WQS in max buffered distance
  noWQS <- snapList_WQS[['tbl_output']]
  
 
  # Use original basin AU dataset to include those sites that didnt snap to WQS segment within given buffer distances
  Sites_AU_WQS <- left_join(AUsnappedSites, results_WQS, by = 'FDT_STA_ID') %>%
    st_set_geometry(NULL)
  
  # final thing to give assessors for app 
  return(Sites_AU_WQS) # this is essentially a basin specific version of 'data/BRRO_Sites_AU_WQS.csv')
}
