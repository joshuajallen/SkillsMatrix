dataModuleUI <- function(id, values) {
  ns <- NS(id)
  
  
  fluidPage(
    
    #  fluidRow(column(12, offset=10, actionButton(ns("info"), "Info"), icon=icon("info"))),
    div(style="display:inline-block;width:98%;text-align: right;",actionButton(ns("info"), label = "Info", icon = icon("info"))),
    
    fluidRow(
      column(8, 
             h5(
               "Please select a theme for the dashboard "
             ),
             selectInput(
                   inputId = "dbxChangeTheme"
                   ,label = NULL
                   ,choices = c(
                     "BoE website" = "boe_website"
                     ,"Blue gradient" = "blue_gradient"
                     ,"Grey light" = "grey_light"
                     ,"Grey dark" = "grey_dark"
                     ,"OneNote" = "onenote"
                     ,"Poor man's Flatly" = "poor_mans_flatly"
                     ,"Purple gradient" = "purple_gradient"
                   )
                   ,selected = "boe_website"
                 ))
    ), 
    

    hr(),

    br(),
    
    h3(
      "Please load the Data and Statistics Division (DSD only) processed data by clicking the option below ... "
    ),
    
    fluidRow( box( title="Load data from source file", 
                   status="danger"
                   ,width = 6
                   ,fluidRow(column(4,actionButton(ns("get"),"Load processed data file", icon = icon("play-circle")))) 
                   
    )), 
    h1(verbatimTextOutput("warning2")),

    h3(
      "Please load your processed data from a .csv file by clicking the option below ... "
    ),
    
   fluidRow( box( title="Load Skills Matrix data", 
                   status="danger"
                   ,width = 6
                   ,fluidRow(column(12,fileInput(ns("data"),label=NULL,multiple = FALSE,   accept = ".csv",
                              buttonLabel = "Load data from csv file ...", placeholder = "No file selected"))
                   )
                   ), 
   h1(verbatimTextOutput("warning1")) 
             ),

   
   br(),  hr(), br(),
   p(strong("The tables below show the respondant sample of the processed data, by team and by scale")), 
   
   fluidRow(
     column(6, dataTableOutput(ns("teamData"))), 
     column(6, dataTableOutput(ns("scaleData")))

       )
  )

  
}



dataModule <- function(input, output, session,values){

  #initiate merge
  weHaveData=FALSE
  SkillsDF<-data.frame()
  SkillsDFanonymised<-data.frame()
  load=FALSE
  nScales=NULL
  nTotalMat=NULL
  nGroups=NULL
  YearLatest = NULL
  YearPrevious = NULL
  nYears = NULL

  isolate({
    values$weHaveData<-weHaveData
    values$SkillsDF<-SkillsDF
    values$load<- load
    values$nScales=nScales
    values$nTotalMat=nTotalMat
    values$nGroups=nGroups
    values$YearLatest = YearLatest
    values$YearPrevious = YearPrevious
    values$SkillsDFanonymised = SkillsDFanonymised
    values$nYears = nYears
    
  })

  observeEvent(input$info, {
    showModal(modalDialog(
      title = "Overview", footer = modalButton("Got it!"),
      textDataModule
      
      ))})
# wait for the user to input the calendar  
# make some basic checks
 

  observeEvent(input$data, {
    
    withProgress(message = 'Progress', value = 0, {
      
      # inputs --------------------------------------------------------------
      incProgress(1/3, detail = paste("Loading Skills Data"))

    inFile <- input$data
    SkillsRAW<- read.csv(inFile$datapath)    
    
    incProgress(2/3, detail = paste("Cleaning data ..."))
    
    
    cols <- c("ID","Scale","High.Level.Cat","Specific.Skill","Rank.Now","Rank.Future","Team","Rank.Now.Num","Rank.Future.Num","Gap","Year")

    # make sure we have all columns we need
    if(any(!cols %in% colnames(SkillsRAW)) ){
      
      text<-paste0("Please re-download your skills matrix, the following columns are missing: ",paste0(colnames(SkillsRAW)[!cols %in% colnames(SkillsRAW)],collapse = ", "))
      SkillsDF<-data.frame()
      
    } else{
      
      colnames(SkillsRAW) <- make.names(colnames(SkillsRAW), unique=TRUE)
      SkillsRAW<-SkillsRAW[!is.na(SkillsRAW$ID),]
      
      if("Name" %in% colnames(SkillsRAW)){
        
        SkillsDF <- SkillsRAW %>%
          dplyr:: select(ID, Name, Scale, High.Level.Cat, Specific.Skill, Rank.Now, Rank.Future, Team, Rank.Now.Num, Rank.Future.Num, Gap, Year ) %>%
          dplyr:: mutate(Year = as.character(Year)) %>% 
          tidyr:: drop_na(Rank.Now.Num)
        
        SkillsDFanonymised <- SkillsRAW %>%
          dplyr:: select(ID, Name, Scale, High.Level.Cat, Specific.Skill, Rank.Now, Rank.Future, Team, Rank.Now.Num, Rank.Future.Num, Gap, Year ) %>%
          dplyr:: mutate(Year = as.character(Year)) %>%
          tidyr:: drop_na(Rank.Now.Num)
        
      } else{
        
        SkillsDF <- SkillsRAW %>%
          dplyr:: select(ID, Scale, High.Level.Cat, Specific.Skill, Rank.Now, Rank.Future, Team, Rank.Now.Num, Rank.Future.Num, Gap, Year ) %>%
          dplyr:: mutate(Year = as.character(Year)) %>%
          tidyr:: drop_na(Rank.Now.Num)
        
        SkillsDFanonymised <- SkillsRAW %>%
          dplyr:: select(ID, Scale, High.Level.Cat, Specific.Skill, Rank.Now, Rank.Future, Team, Rank.Now.Num, Rank.Future.Num, Gap, Year ) %>%
          dplyr:: mutate(Year = as.character(Year)) %>%
          dplyr:: group_by(Team, Scale, Year) %>%
          dplyr:: mutate(N = n_distinct(ID)) %>%
          dplyr:: filter(N != 1) %>%
          tidyr:: drop_na(Rank.Now.Num)
        
      }

    }
  
    
    incProgress(3/3, detail = paste("Making the final data set available"))
    
    if(nrow(SkillsDF)>1){
      
      shinyalert(
        title = "success",
        text = "The data has been loaded successfully!",
        type = "success"
      )
    }
    
    
    else {
      
      text<-paste0("Please re-download your skills matrix, the following columns are missing: ",paste0(colnames(SkillsDF)[!cols %in% colnames(SkillsDF)],collapse = ", "))
      shinyalert(
        title="Oops!",
        text = text,
        type = "error"
      )
    }
    
    })
    
    if(nrow(SkillsDF) > 1){
    
    weHaveData <- (nrow(SkillsDF) > 1)
    load <- (nrow(SkillsDF) > 0)
    nScales=length(unique(SkillsDF$Scale))
    nTotalMat=length(unique(SkillsDF$ID))
    nGroups=length(unique(SkillsDF$Team))
    YearLatest = max(unique(SkillsDF$Year))
    
    if(length(unique(SkillsDF$Year)) > 1){
      YearPrevious  = max(unique(SkillsDF$Year)[which(!unique(SkillsDF$Year) %in% max(unique(SkillsDF$Year)))])
    } else{
      YearPrevious = max(unique(SkillsDF$Year))
    }   
  
    nYears = length(unique(SkillsDF$Year))
    
    } else{
      
      weHaveData=FALSE
      SkillsDF<-data.frame()
      SkillsDFanonymised<-data.frame()
      load=FALSE
      nScales=NULL
      nTotalMat=NULL
      nGroups=NULL
      YearLatest = NULL
      YearPrevious = NULL
      nYears = NULL
 
    }
    
    

    isolate({
      values$SkillsDF <- SkillsDF
      values$SkillsDFanonymised <- SkillsDFanonymised
      values$weHaveData<- weHaveData
      values$load<- load
      values$nScales<-nScales
      values$nTotalMat<-nTotalMat
      values$nGroups<-nGroups
      values$YearLatest = YearLatest
      values$YearPrevious = YearPrevious
      values$nYears = nYears
      
    })
    
    if(length(SkillsDF) == 1){
      
      output$warning1 = renderPrint({
        
        print("You do not have permission to view this data")
        
      })
    }

  })
    

  
  # get the remaining data
  observeEvent( input$get, {
    
    withProgress(message = 'Progress', value = 0, {
      
      # inputs --------------------------------------------------------------
      incProgress(1/4, detail = paste("Loading Skills Data"))

     
      
      readCSV <- function(csvFile) {
        file <- boeGetPathFromUnc(csvFile)
        tryCatch({
          read.csv(file)
        }, error = function(e) {
          as.data.frame("You do not have permission to view this data")
        })
      }
      
      SkillsRAW <- readCSV(csvFile = "\\\\MFSD\\data\\DATA\\Cross Divisional Work\\SkillsMatrix\\ProcessedData\\ProcessedData.csv")
      
      incProgress(2/3, detail = paste("Cleaning data ..."))
      
      cols <- c("ID","Scale","High.Level.Cat","Specific.Skill","Rank.Now","Rank.Future","Team","Rank.Now.Num","Rank.Future.Num","Gap","Year")
      
      # make sure we have all columns we need
      if(any(!cols %in% colnames(SkillsRAW)) ){
        
        text<-paste0("Please re-download your skills matrix, the following columns are missing: ",paste0(colnames(SkillsRAW)[!cols %in% colnames(SkillsRAW)],collapse = ", "))
        SkillsDF<-data.frame()
        
      } else{
        
        colnames(SkillsRAW) <- make.names(colnames(SkillsRAW), unique=TRUE)
        SkillsRAW<-SkillsRAW[!is.na(SkillsRAW$ID),]
        
        if("Name" %in% colnames(SkillsRAW)){
          
          SkillsDF <- SkillsRAW %>%
            dplyr:: select(ID, Name, Scale, High.Level.Cat, Specific.Skill, Rank.Now, Rank.Future, Team, Rank.Now.Num, Rank.Future.Num, Gap, Year ) %>%
            dplyr:: mutate(Year = as.character(Year))
          
          SkillsDFanonymised <- SkillsRAW %>%
            dplyr:: select(ID, Name, Scale, High.Level.Cat, Specific.Skill, Rank.Now, Rank.Future, Team, Rank.Now.Num, Rank.Future.Num, Gap, Year ) %>%
            dplyr:: mutate(Year = as.character(Year)) %>%
            dplyr:: group_by(Team, Scale, Year) 
          
        } else{
          
          SkillsDF <- SkillsRAW %>%
            dplyr:: select(ID, Scale, High.Level.Cat, Specific.Skill, Rank.Now, Rank.Future, Team, Rank.Now.Num, Rank.Future.Num, Gap, Year ) %>%
            dplyr:: mutate(Year = as.character(Year))
          
          SkillsDFanonymised <- SkillsRAW %>%
            dplyr:: select(ID, Scale, High.Level.Cat, Specific.Skill, Rank.Now, Rank.Future, Team, Rank.Now.Num, Rank.Future.Num, Gap, Year ) %>%
            dplyr:: mutate(Year = as.character(Year)) %>%
            dplyr:: group_by(Team, Scale, Year) %>%
            dplyr:: mutate(N = n_distinct(ID)) %>%
            dplyr:: filter(N != 1)
          
        }
      }

      incProgress(3/3, detail = paste("Making the final data set available"))
      
      if(nrow(SkillsDF)>1){
        
        shinyalert(
          title = "success",
          text = "The data has been loaded successfully!",
          type = "success"
        )
      }
      
      
      else {
      
        shinyalert(
          title = "failed",
          text = "You do not have permission to view this data ",
          type = "error"
        )
      }
      
      
    })
      
    
    if(nrow(SkillsDF) > 1){
      
      weHaveData <- (nrow(SkillsDF) > 1)
      processed <- (nrow(SkillsDF) > 0)
      nScales=length(unique(SkillsDF$Scale))
      nTotalMat=length(unique(SkillsDF$ID))
      nGroups=length(unique(SkillsDF$Team))
      YearLatest = max(unique(SkillsDF$Year))
      nYears = length(unique(SkillsDF$Year))
      
      
      if(length(unique(SkillsDF$Year)) > 1){
        YearPrevious  = max(unique(SkillsDF$Year)[which(!unique(SkillsDF$Year) %in% max(unique(SkillsDF$Year)))])
    } else{
        YearPrevious = max(unique(SkillsDF$Year))
        }
      
    } else{
      
      weHaveData=FALSE
      SkillsDF<-data.frame()
      SkillsDFanonymised<-data.frame()
      processed=FALSE
      nScales=NULL
      nTotalMat=NULL
      nGroups=NULL
      YearLatest = NULL
      YearPrevious = NULL
      nYears = NULL
      
      
    }

      isolate({
        values$SkillsDF<-SkillsDF
        values$SkillsDFanonymised <- SkillsDFanonymised
        values$weHaveData<- weHaveData
        values$processed<- processed
        values$nScales<-nScales
        values$nTotalMat<-nTotalMat
        values$nGroups<-nGroups
        values$YearLatest = YearLatest
        values$YearPrevious = YearPrevious
        values$nYears = nYears
        
      })
      
      if(length(SkillsDF) == 1){
        
        output$warning1 = renderPrint({
          
          print("You do not have permission to view this data")
          
        })
      }


  })

  
  observeEvent(values$load | values$processed, {
    
    
    if(values$weHaveData){
      
      SkillsDF = values$SkillsDF
      SkillsDFanonymised = values$SkillsDFanonymised
      
      YearLatest = values$YearLatest
      YearPrevious = values$YearPrevious 
      
      
      output$teamData <- DT::renderDataTable({
        
        LatestYear <- rlang:: sym(as.character(YearLatest))

        summarySt<- SkillsDF %>%
          dplyr:: group_by(Team, Year) %>%
          dplyr:: summarise(NTeam=round(n_distinct(ID),2)) %>%
          dplyr:: ungroup() %>%
          dplyr:: group_by(Year) %>%
          dplyr:: bind_rows(summarise_all(., list(~ if(is.numeric(.)) sum(.) else "Total"))) %>% 
          tidyr:: drop_na() %>%
          tidyr:: spread(key = Year, value = NTeam) %>%
          dplyr:: arrange(desc(!! LatestYear)) 
          
        
        
        create_data_table(summarySt) %>%
          
          formatStyle(names(dplyr::select_if(create_data_table(summarySt)$`x`$data,is.numeric)),
                      background = styleColorBar(c(0,40), 'lightblue'),
                      backgroundSize = '98% 60%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'left') 
      })

      
      output$scaleData <- DT::renderDataTable({
        
        LatestYear <- rlang:: sym(as.character(YearLatest))
        
        summarySt<-SkillsDF %>%
          dplyr:: group_by(Scale, Year) %>%
          dplyr:: summarise(NScale=round(n_distinct(ID),2)) %>%
          dplyr:: ungroup() %>%
          dplyr:: group_by(Year) %>%
          dplyr:: bind_rows(summarise_all(., list(~ if(is.numeric(.)) sum(.) else "Total"))) %>% 
          dplyr:: arrange(desc(NScale)) %>%
          tidyr:: drop_na() %>%
          tidyr:: spread(key = Year, value = NScale) %>%
          dplyr:: arrange(desc(!! LatestYear)) 
        
        
        
        create_data_table(summarySt) %>%
          
          formatStyle(names(dplyr::select_if(create_data_table(summarySt)$`x`$data,is.numeric)),
                      background = styleColorBar(c(0,40), 'lightblue'),
                      backgroundSize = '98% 60%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'left') 
      })
      

    }
    
  })
  
}
  
