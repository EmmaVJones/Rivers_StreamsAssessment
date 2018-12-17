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
                      navbarPage("VDEQ 2020 Rivers and Streams Assessment Tool",
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
                                #            Stations Table 2.0 and Comment Files from any previous analysis session.'),
                                #          h5('Stations Table 2.0'),
                                #          helpText('This dataset is derived before any Rivers and Streams Assessment Tool analysis 
                                #                   procedures can commence using the ',span(strong('XXXXXX TOOL.')), 'After completing
                                #                   the requisite analyses from the ',span(strong('XXXXXX TOOL')),'once, users can 
                                #                   upload their results to the Rivers and Streams Assessment Tool each time they open
                                #                   the tool for analysis.'),
                                #          fileInput('stationsTable','Choose your Regional Stations Table 2.0.',
                                #                    accept = c(".RDS")),
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
                                            dynamicSelectInput("HUC6Selection", "Select Major Basin", multiple = FALSE)),
                                          mainPanel(
                                            leafletOutput('VAmap'),
                                            br(),
                                            DT::dataTableOutput('AUstationSummary')
                                          )
                                 ),
                                tabPanel('Station Review',
                                         fluidRow(column(9, DT::dataTableOutput('selectedAU')),
                                                  column(3,br(),actionButton('pullAUdata','Select Watershed for analysis'))),
                                         hr(),
                                         uiOutput('stationSelection_'),
                                         fluidRow(column(4, DT::dataTableOutput('stationInfo')),
                                                  column(4, leafletOutput('stationMap', height = 300, width = 300)),
                                                  column(4,h4('Comments:'), 
                                                         textAreaInput('commentBox', label = NULL, height = 275, 
                                                                       placeholder = 'Comments'))),
                                         verbatimTextOutput("table"))
                      )))
))


#verbatimTextOutput("table"),