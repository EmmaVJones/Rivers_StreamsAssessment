# Run in R 3.5.1
source('global.R')

shinyUI(fluidPage(theme="yeti.css",
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      # suppress error messages as data loads, hacky
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      navbarPage(paste("VDEQ ",assessmentCycle," Rivers and Streams Assessment Tool", sep=''),
                                 #tabPanel('Data Upload',
                                #          h3('Tool Overview'),
                                #          p("The Rivers and Streams Assessment Tool is designed to expedite analysis, assessment
                                #            decisions, and quality assurance/quality control (QA/QC) procedures for Virginia's
                                #            contribution to the 2020 Integrated Report (IR). The data window analyzed covers 
                                #            January 1, 2013 to December 31, 2018. Tool users can expect significant time savings
                                #            on repetitive procedures including: raw data organization from disparate databases, 
                                #            geospatial organization of stations by assessment unit, standard/criteria calculations, 
                                #            and data visualization."),
                                #          br(),br(),br(),
                                #          h3('Tool Inputs'),
                                #          p('In order to reduce processing time and facilitate peristent data storage, users must
                                #            upload certain datasets that follow a specified template. These include their regional
                                #            Stations Table 2.0 and Regional Assessment Unit shapefiles.'),
                                #          h5('Stations Table 2.0'),
                                #          helpText('This dataset is derived before any Rivers and Streams Assessment Tool analysis 
                                #                   procedures can commence using the ',span(strong('XXXXXX TOOL.')), 'After completing
                                #                   the requisite analyses from the ',span(strong('XXXXXX TOOL')),'once, users can 
                                #                   upload their results to the Rivers and Streams Assessment Tool each time they open
                                #                   the tool for analysis.'),
                                #          fileInput('stationsTable','Choose your Regional Stations Table 2.0.',
                                #                    accept = c(".csv")),
                                #          h5('Regional Assessment Units'),
                                #          helpText('This shapefile is the current working copy of the regional assessment units. It must
                                #                    be uploaded to the app on startup and/or any time any spatial changes to assessment
                                #                    units (split an assessment unit) are completed in ArcGIS to ensure the most up to 
                                #                    date assessment unit information is being utlized by the app.'),
                                #          fileInput('regionalAUshapefile','Choose your Regional Assessment Unit shapefile.',
                                #                    accept = c(".dbf",".prj",".sbn",".sbx",".shp","shp.xml",".shx")),
                                #          h5('Comment Files'),
                                #          helpText('Comment files are generated each time an assessor utlizes the Rivers and Streams 
                                #                   Assessment Tool comment fields. Though entering information into these fields is not
                                #                   necessary to complete any assessment actions, it is a best practice to include
                                #                   information relevant to assessment decisions or historical information for archiving
                                #                   in a single location. These files will be transferred with subsequent Rivers and 
                                #                   Streams Assessment Tool updates across IR windows to maintain regional assessment records.'),
                                #          fileInput('commentFile','Choose most recent comment file.',
                                #                    accept = c(".csv")),
                                #          hr(),br(),
                                #          h4('Station Table QA'),
                                #          helpText('This automated analysis section highlights any stations from the Conventionals dataset
                                #                   that do not have appropriate Station Table 2.0 information.',
                                #                   span(strong('Failure to address these stations will guarantee they will not be represented
                                #                               in subsequent analyses by the Rivers and Streams Assessment tool.'))),
                                #          DT::dataTableOutput('stationTableMissingStations')),
                                 tabPanel('Watershed Selection',
                                          sidebarPanel(
                                            dynamicSelectInput("DEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE),
                                            dynamicSelectInput("basinSelection", "Select Major Basin", multiple = FALSE),
                                            dynamicSelectInput("HUC6Selection", "Select VAHU6", multiple = FALSE),
                                            br(),
                                            actionButton('reviewAUs',"Preview Assesment Units",class='btn-block')),
                                          mainPanel(
                                            leafletOutput('VAmap'),
                                            br(),
                                            h5('Assessment Units in Selected VAHU6'),
                                            DT::dataTableOutput('AUSummary'),
                                            h5('Stations in Selected VAHU6'),
                                            DT::dataTableOutput('stationSummary')
                                          )
                                 ),
                                tabPanel('Assessment Unit Review',
                                         fluidRow(column(9, DT::dataTableOutput('selectedHUC')),
                                                  column(3,br(),actionButton('pullHUCdata','Select Watershed for analysis'))),
                                         hr(),
                                         uiOutput('AUSelection_'),
                                         DT::dataTableOutput('selectedAU'),br(),
                                         uiOutput('stationSelection_'),
                                         fluidRow(column(4, DT::dataTableOutput('stationInfo')),
                                                  column(4, leafletOutput('stationMap', height = 300, width = 300)),
                                                  column(4, DT::dataTableOutput('stationHistoricalInfo'))),
                                         hr(),
                                         h3('Station Results for Review'),
                                         helpText('This table outputs the site specific results for direct export to the Station Table. It also serves to highlight
                                                  where exceedances are present and should be reviewed in the individual parameter visualization tabs below.'),
                                         DT::dataTableOutput('stationTableDataSummary'),
                                         br(),hr(),br(),
                                         h3('Assessment Unit Raw Data Review and Visualization'),
                                         tabsetPanel(
                                           tabPanel('Conventionals Data',
                                                    tabsetPanel(
                                                      tabPanel('Raw Data',br(),
                                                               DT::dataTableOutput('AURawData'),
                                                               h4('Data Summary'),
                                                               h5('Records Retrieved in Assessment Unit:'),
                                                               fluidRow(column(1),column(10,textOutput('stationDataTableRecords'))),
                                                               h5('Field and Lab Data in Assessment Window:'),
                                                               fluidRow(column(1),column(10,tableOutput('uniqueStationDataTableRecords'))),
                                                               h5('Assessment Window:'),
                                                               fluidRow(column(1),column(10,textOutput('stationDataTableAssessmentWindow'))), br(),br()),
                                                      tabPanel('Temperature',
                                                               helpText('Review each site using the single site visualization section, then 
                                                             proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
                                                                        span(strong('NOTE: The temperature exceedance analysis results at the bottom of the page include data
                                                                         from ALL stations within the assessment unit.'))),
                                                               temperaturePlotlySingleStationUI('temperature'),
                                                               br(),hr(),br(),
                                                               temperatureExceedanceAnalysisUI('temperature_ExceedanceAnalysis')),
                                                      tabPanel('pH',
                                                               helpText('Review each site using the single site visualization section, then proceed to the bottom of the page 
                                                             to find exceedance rate for the entire assessment unit.',br(), 
                                                                        span(strong('NOTE: The pH exceedance analysis results at the bottom of the page include data from 
                                                                         ALL stations within the assessment unit.'))),
                                                               pHPlotlySingleStationUI('pH'),
                                                               br(),hr(),br(),
                                                               pHExceedanceAnalysisUI('pH_ExceedanceAnalysis')),
                                                      tabPanel("DO",
                                                               helpText('Review each site using the single site visualization section, then proceed to the bottom of the page 
                                                             to find exceedance rate for the entire assessment unit.',br(),
                                                                        span(strong('NOTE: The DO exceedance analysis results at the bottom of the page include data from ALL 
                                                                         stations within the assessment unit.'))),
                                                               DOPlotlySingleStationUI('DO'),
                                                               br(),hr(),br(),
                                                               DOExceedanceAnalysisUI('DO_ExceedanceAnalysis')),
                                                      tabPanel("Specific Conductance",
                                                               helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
                                                               SpCondPlotlySingleStationUI('SpCond')),
                                                      tabPanel("Salinity",
                                                               helpText('Review each site using the single site visualization section. There are no WQS for Salinity.'),
                                                               salinityPlotlySingleStationUI('salinity')),
                                                      tabPanel("Nitrogen",
                                                               helpText('Review each site using the single site visualization section. There are no WQS for Total Nitrogen.'),
                                                               TNPlotlySingleStationUI('TN')),
                                                      tabPanel("Ammonia"),
                                                      tabPanel("NH3"),
                                                      tabPanel("Phosphorus"),
                                                      tabPanel("Fecal Coliform"),
                                                      tabPanel("E. Coli"),
                                                      tabPanel("Enterococci"),
                                                      tabPanel("Chlorophyll a"),
                                                      tabPanel("SSC"),
                                                      tabPanel("Nitrite"),
                                                      tabPanel("Chloride"),
                                                      tabPanel("Sulfate")
                                                      )),
                                           tabPanel('EDAS Data', br(), br(), br(), h4('Placeholder'),br(), br(), br()),
                                           tabPanel('Metals Data', br(), br(), br(), h4('Placeholder'),br(), br(), br()),
                                           tabPanel('Fish Tissue Data', br(), br(), br(), h4('Placeholder'),br(), br(), br()),
                                           tabPanel('Toxics Data', br(), br(), br(), h4('Placeholder'),br(), br(), br())
                                         )
                                         
                                           
                                         )))
                    )))


#verbatimTextOutput("table"),