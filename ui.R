source("../environment.R")
boeCheckpoint("2019-02-12")

# History of versions:
# v1.0 20/08/2018
# v1.1 20/09/2019 
# v2.0 01/11/2019 

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(zoo)
#library(shiny)
library(shinydashboard)
library(forcats)
library(shinyWidgets)
library(formattable)
library(shinythemes)
library(flexdashboard)
library(plotly)
library(glue)
library(DT)
library(matrixStats)
library(stringi)
library(rlang)
library(shinyalert)
library(ggrepel)
#library(ggalt)
library(widyr)
library(igraph)
library(visNetwork)
library(scales)

#themes and functions 
source("./Scripts/dashboardthemesModule.R")
source("./Scripts/helperFunsShiny.R")
#app modules
source("./Scripts/dataModule.R")
source("./Scripts/statsModule.R")
source("./Scripts/gapModule.R")
source("./Scripts/temporalModule.R")
source("./Scripts/drilldownModule.R")
#settings and instructions 
source("./Scripts/settingsModule.R")

dbHeader <- dashboardHeader(title = "Skills Matrix",
                            tags$li(a(href = 'https://www.bankofengland.co.uk/',
                                      img(src = 'logo_paul.png',
                                          title = "Company Home", height = "20px")),
                                     # style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    useShinyalert(),
    ### username information from server
    uiOutput("uiUserInfo")
    
    ,br(),
    menuItem("Get data", tabName = "data", icon = icon("database")),
    menuItem("Skills Stock Take", icon = icon("bar-chart-o"), tabName = "stats"),
    menuItem("Skills GAP Analysis", icon = icon("circle"), tabName = "gap"),
    menuItem("Skills Temporal Analysis", icon = icon("line-chart"), tabName = "temporal"),
    menuItem("Drilldown", icon = icon("arrow-down"), tabName = "drilldown"),
    
    menuItem("Instructions", icon = icon("gear"), tabName = "instructions"),
    menuItem("Privacy Notice", icon = icon("bars"), tabName = "privacynotice")
  )
)

body <- dashboardBody(
  
 # shinyDashboardThemes(
 #    theme = "boe_website"
 #  ),  
  
  uiOutput("uiChangeTheme"),
  
  #receiveSweetAlert(messageId = "privacyNotice"),
  tabItems(
    tabItem(tabName = "data",
            dataModuleUI("data"), icon=icon("database")

    ),
    tabItem(tabName = "stats",
            statsModuleUI("stats"), icon=icon("bar-chart-o")
    ),
    
    tabItem(tabName = "gap",
            gapModuleUI("gap"), icon=icon("circle")
    ),

    tabItem(tabName = "temporal",
            temporalModuleUI("temporal"), icon=icon("line-chart")
    ),
    
    tabItem(tabName = "drilldown",
            drilldownModuleUI("drilldown"), icon=icon("arrow-down")
    ),

    tabItem(tabName = "instructions",
            settingsModuleUI("instructions"), icon=icon("gear")
    ),
    tabItem(tabName ="privacynotice",
            
             box(title="Privacy notice", 
                 status="danger"
                 ,width = 12
                 ,textPrivacyNortice
                 )
    )))
    
  


# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dbHeader,
  sidebar,
  body
))
