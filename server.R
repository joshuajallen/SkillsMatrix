#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



set.seed(0)
options(stringsAsFactors = FALSE)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # privacy notice  
  showModal(modalDialog(
    title = "Privacy Notice",
    textPrivacyNortice,
    easyClose = TRUE,
    footer = NULL
  ))
  
  values<-reactiveValues()
  values$emp.no<- Sys.info()[["user"]]
  values$SkillsDF=data.frame()
  values$SkillsDFanonymised=data.frame()
  values$weHaveData=FALSE
  values$load=FALSE
  values$processed=FALSE
  values$nScales=NULL
  values$nTotalMat=NULL
  values$nGroups=NULL
  values$YearLatest = NULL
  values$YearPrevious = NULL
  values$Scale = NULL
  values$Team = NULL
  values$nYears = NULL
  

  callModule(dataModule, "data",values)
  callModule(statsModule, "stats",values)
  callModule(gapModule, "gap",values)
  callModule(temporalModule, "temporal",values)
  callModule(drilldownModule, "drilldown",values)
  callModule(settingsModule, "makechart",values)
  
  
  ### changing user info in sidebar
  output$uiUserInfo <- renderUI({
    if (!is.null(Sys.info()[["user"]])){
      sidebarUserPanel(
        image = "user-white.png",
        span(
          "Logged in as "
          ,Sys.info()[["user"]]
        )
      )
      
    }
    
  })
  
  ### changing theme
  output$uiChangeTheme <- renderUI({
    
    shinyDashboardThemes(
      
      theme = input$dbxChangeTheme
      
    )
    
  })
  
  
})
