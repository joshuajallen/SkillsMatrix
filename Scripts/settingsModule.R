


shinyAppName = "WorkSmarter: Skills Matrix"
shinyAppVersion = "v1.0"
shinyAppYear = "2019"
shinyAppOwnerEmail = "joshua.allen, [@bankofengland.co.uk]. BoE theme by shinyDashboardThemes."


settingsModuleUI <- function(id) {
  ns <- NS(id)
 
  
  fluidRow(tabBox(
    
    id = "tbsExample"
    ,height = "100%"
    ,width = 12
    ,side = "left"
    ,selected = "Skills Matrix"
    
    ,tabPanel(
      
      title = "Skills Matrix",
  
      fluidRow(
      column(
        width=12,
        
        box( width=12,
             h5(strong("Intro")),   textSettingsModule,      hr()
             
        ))

              ))
,
  tabPanel(
  
  title = "Instructions",
  
 fluidRow(column(12, box( width=12,
       #                 h4("Instructions"),solidHeader = T,
       h4(strong("Get Data tab")),   textDataModule,      hr(),
       h4(strong("Skills Stock Take")),   textStockModule,      hr(),
       h4(strong("Skills GAP Analysis")),   textGapModule,      hr(),
       h4(strong("Skills Temporal Analysis")),   textTemporalModule,       hr(),
       h4(strong("Drilldown")),   textDrilldownModule,       hr()
       
  ))))
),
      
  # Footer.
  div(
    style = "
    background: white;
    color: black;
    text-align: right;
    flex-shrink: 0;
    padding-top: 0.3em;
    padding-bottom: 0.2em;
    padding-right: 1em;
    ",
    
    paste(
      shinyAppName,
      shinyAppVersion,
      shinyAppYear,
      paste("Contact:",shinyAppOwnerEmail),
      sep = " - "
    )
  ))
  
    
    
  
}

settingsModule <- function(input, output, session,values) {
  
  
  
  
  
  
  
}