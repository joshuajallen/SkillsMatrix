temporalModuleUI <- function(id, values) {
  ns <- NS(id)
  
  ### Dashboard row 1 ---------------------------------------------------------
  fluidPage(
    
    fluidRow(
      
      
      div(style="display:inline-block;width:98%;text-align: right;",
          actionButton(ns("info"), label = "Info", icon = icon("info"))),
      
      column(6, box(
        ##
        h3("Required Inputs:"), 
        p(strong("Please select skills catagories")),
        
        
        selectizeInput(ns("HighSkill3"),
                       "High Level Skill Catagory",
                       choices = "All",
                       multiple = TRUE), width = 12)),
      
      column(6,box(
        ##
        h3("Optional Inputs:"), 
        p(strong("Please select a scale(s), team(s) or both")),
        
        
        
        selectizeInput(ns("scale3"),
                       "Select A Scale",
                       choices = "All",
                       multiple = TRUE), 
        
        selectizeInput(ns("team3"),
                       "Select a Team",
                       choices = "All",
                       multiple = TRUE), width = 12))
    ),
    
    
    
    hr(),
    
    fluidRow(
      tabBox(
        
        id = "tbsExample"
        ,height = "100%"
        ,width = 12
        ,side = "left"
        ,selected = "Current Scores Temporal"
        
        
        ,tabPanel(
          
          title = "Current Scores Temporal"
          ,icon = icon("table"),
          
          h4(
            "The chart/data table below shows the average current scores by skill catagory as a time series, you can select the following inputs to disaggregate the data by; "
          ),
          
          h5(tags$ul(
            tags$li("Skill catagory - upper level of the skills hierarchy, e.g. soft skills"),
            tags$li("Scale - select one or more scales across the business area"),
            tags$li("Team - select one or more teams across the business area")
            
          )),
          
          plotOutput(ns("temporal_now_plot"), height = "800px", width = "100%"),
          
          br(),
          hr(),
          h4(
            "The data table below shows the average scores by skill catagory with the year on year change, you can toggle the order by clicking on the column; "
          ),
          
          
          fluidRow(column(12,  box(collapsible = TRUE,  
            solidHeader = TRUE
            ,width = 12
            
            ,dataTableOutput(ns("temporal_now"))
          )))
          
        )
        
        ,tabPanel(
          
          title = "Future Scores Temporal"
          ,icon = icon("table"),
          
          h4(
            "The chart/data table below shows the average expected scores by skill catagory as a time series, you can select the following inputs to disaggregate the data by; "
          ),
          
          h5(tags$ul(
            tags$li("Skill catagory - upper level of the skills hierarchy, e.g. soft skills"),
            tags$li("Scale - select one or more scales across the business area (only the first element is used for the dumbell plot)"),
            tags$li("Team - select one or more teams across the business area (only the first element is used for the dumbell plot)")
            
          )),
          
          
          plotOutput(ns("temporal_future_plot"), height = "800px", width = "100%"),
          
          
          br(),
          hr(),
          h4(
            "The data table below shows the average expected scores by skill catagory with the year on year change, you can toggle the order by clicking on the column; "
          ),
          
          fluidRow(column(12,  box(collapsible = TRUE,  
            solidHeader = TRUE
            ,width = 12
            
            ,dataTableOutput(ns("temporal_future"))
          )))
        )
        ,tabPanel(
          
          title = "GAP Analysis Temporal"
          ,icon = icon("table"),
          
          h4(
            "The chart/data table below shows the skills GAPS by skill catagory as a time series, you can select the following inputs to disaggregate the data by; "
          ),
          
          h5(tags$ul(
            tags$li("Skill catagory - upper level of the skills hierarchy, e.g. soft skills"),
            tags$li("Scale - select one or more scales across the business area (only the first element is used for the dumbell plot)"),
            tags$li("Team - select one or more teams across the business area (only the first element is used for the dumbell plot)")
            
          )),
          
          h4(
            "Note that the GAP is the average distance between the current and future scores, a small gap is a lower deficit"
          ),
          
          
          plotOutput(ns("temporal_gap_plot"), height = "800px", width = "100%"),
          
          
          br(),
          hr(),
          h4(
            "The data table below shows the average gap by skill category with the year on year change, note that a positive change is a reduction in skill gap. You can toggle the order by clicking on the column; "
          ),
          
          
          fluidRow(column(12,  box(collapsible = TRUE,  
            solidHeader = TRUE
            ,width = 12
            
            ,dataTableOutput(ns("temporal_gap"))
          )))
        )



      )
    )
  )
  
  
  
  
  
 

}

temporalModule <- function(input, output, session, values) {

  observeEvent(values$weHaveData, {
    
    if(values$weHaveData){
      
      SkillsDF=values$SkillsDF
      
      updateSelectizeInput(session, "HighSkill3", choices=c(unique(SkillsDF$High.Level.Cat)), selected = unique(SkillsDF$High.Level.Cat)[2])
      updateSelectizeInput(session, "scale3", choices=c("All", unique(SkillsDF$Scale)), selected = "All")
      updateSelectizeInput(session, "team3", choices=c("All", unique(SkillsDF$Team)), selected = "All")
      
      
    }
    
  })
  
  observeEvent(values$weHaveData, {
    
    if(values$weHaveData == TRUE){
      
      Scale <-input$scale3
      Team <-input$team3
      SkillsDF <- values$SkillsDF
      YearLatest <- values$YearLatest 
      YearPrevious <- values$YearPrevious
      nYears <- values$nYears
      
      isolate(
        {
          values$Scale <- Scale
          values$Team <- Team
          values$YearPrevious <- YearPrevious
          values$YearLatest <- YearLatest
          values$nYears <- nYears
          
        })
    }
    
  })
  
  observeEvent(values$weHaveData, {
    
    if(values$weHaveData == TRUE){
      
      SkillsDF <- values$SkillsDF
      SkillsDFanonymised <- values$SkillsDFanonymised
      YearLatest = as.character(max(unique(SkillsDF$Year)))
      YearPrevious = as.character(max(unique(SkillsDF$Year)[which(!unique(SkillsDF$Year) %in% max(unique(SkillsDF$Year)))]))
      #--------------------------------------------------------------------------------------------------------------------------------------------------
      #Current dsitribution
      
      output$temporal_now <- renderDataTable({
        
        validate(
          need(input$HighSkill3, 'Select at least one Catagory'),
          need(input$scale3, 'Select at least one Scale'),
          need(input$team3, 'Select at least one Team'), 
          need(values$nYears > 1, 'Please load data with more than one year')
          
        )
        
        
        if(input$scale3[1] == "All" & input$team3[1] == "All"){
          
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year")) %>% 
            dplyr::filter(High.Level.Cat %in% input$HighSkill3)  
          
        } else if(input$scale3[1] != "All"  & input$team3[1] == "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Scale")) %>% 
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Scale %in% input$scale3) 
          
        } else if(input$scale3[1] == "All"  & input$team3[1] != "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Team")) %>% 
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Team %in% input$team3)   
          
        } else if(input$scale3[1] != "All"  & input$team3[1] != "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDFanonymised, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Scale", "Team")) %>% 
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Team %in% input$team3, Scale %in% input$scale3) 
        }
        
        YearFirst <- rlang:: sym(as.character(YearLatest))
        YearSecond <- rlang:: sym(as.character(YearPrevious))

        if(as.character(YearLatest) %in% unique(DT$Year) & as.character(YearPrevious) %in% unique(DT$Year) & nrow(DT) > 1){
          
          DT <- DT %>% 
            tidyr:: drop_na() %>%
            dplyr:: select(-meanGap, -Mean_Future) %>%
            tidyr:: spread(key = Year, value = Mean_Now) %>%
            dplyr:: mutate(Change = round(!!YearFirst - !!YearSecond,2))
          
          DF<- formattable(DT %>% dplyr:: arrange(Change),
                           
                           list(Change = color_tile( "lightpink", "lightgreen")))
          
          create_formattable(table_input = DF)
 
        } else{
          
          data.table:: as.data.table("No data to show in table")
          
        }


      })
      
      output$temporal_now_plot <- renderPlot({
        
        validate(
          need(input$HighSkill3, 'Select at least one Catagory'),
          need(input$scale3, 'Select just one Scale'),
          need(input$team3, 'Select just one Team'), 
          need(values$nYears > 1, 'Please load data with more than one year')
          
        )
        
        if(input$scale3[1] == "All" & input$team3[1] == "All"){
          
          
          DT <- dplyr::filter(SkillsDF, High.Level.Cat %in% input$HighSkill3) 
          DT <- makeTemporalDT(mytable = DT, grouping = c("Specific.Skill","High.Level.Cat", "Year"))   
          
        } else if(input$scale3[1] != "All"  & input$team3[1] == "All"){
          
          DT <- dplyr::filter(SkillsDF, High.Level.Cat %in% input$HighSkill3, Scale %in% input$scale3) 
          DT <- makeTemporalDT(mytable = DT, grouping = c("Specific.Skill","High.Level.Cat", "Year"))       
          
        } else if(input$scale3[1] == "All"  & input$team3[1] != "All"){
          
          DT <- dplyr::filter(SkillsDF, High.Level.Cat %in% input$HighSkill3, Team %in% input$team3) 
          DT <- makeTemporalDT(mytable = DT, grouping = c("Specific.Skill","High.Level.Cat", "Year")) 
          
        } else if(input$scale3[1] != "All"  & input$team3[1] != "All"){
          
          DT <- dplyr::filter(SkillsDFanonymised, High.Level.Cat %in% input$HighSkill3, Team %in% input$team3, Scale %in% input$scale3) 
          DT <- makeTemporalDT(mytable = DT, grouping = c("Specific.Skill","High.Level.Cat", "Year")) 
            

        }
        
        YearFirst <- rlang:: sym(as.character(YearLatest))
        YearSecond <- rlang:: sym(as.character(YearPrevious))
        
        if(as.character(YearLatest) %in% unique(DT$Year) & as.character(YearPrevious) %in% unique(DT$Year) & nrow(DT) > 1){
          
          YearFirst <- rlang:: sym(as.character(YearLatest))
          
          DT <- DT  %>% 
            tidyr:: drop_na() %>%
            dplyr:: select(-meanGap, -Mean_Future) %>%
            tidyr:: spread(key = Year, value = Mean_Now) %>%
            dplyr:: ungroup()
          
          DT <- dplyr:: arrange(DT, desc(eval(YearFirst)))
          DT <- dplyr:: mutate(DT, Specific.Skill=factor(Specific.Skill, levels=rev(Specific.Skill)))  
          
          df2 <-  DT %>% dplyr:: select(Specific.Skill, contains("20")) %>% tidyr::gather(group, value, -Specific.Skill)
          
          P<- makeDumbellPlot(DT = DT, df2 = df2, year1 = as.character(YearLatest),  year2 = as.character(YearPrevious), grouping = "Specific.Skill") +
            labs(x="Average Current Score")
          P
          
        } else{
          
          text = paste("No rows in DF")
          ggplot() + 
            annotate("text",size=8, label = text) + 
            theme_bw() +   theme(panel.grid.major=element_blank(),
                                 panel.grid.minor=element_blank())
        }


        
      })
      
      #--------------------------------------------------------------------------------------------------------------------------------------------------
      #Future dsitribution
      
      output$temporal_future <- renderDataTable({
        
        validate(
          need(input$HighSkill3, 'Select at least one Catagory'),
          need(input$scale3, 'Select just one Scale'),
          need(input$team3, 'Select just one Team'), 
          need(values$nYears > 1, 'Please load data with more than one year')
          
        )
        
        if(input$scale3[1] == "All" & input$team3[1] == "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year")) %>% 
            dplyr::filter(High.Level.Cat %in% input$HighSkill3)  
          
        } else if(input$scale3[1] != "All"  & input$team3[1] == "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Scale")) %>% 
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Scale %in% input$scale3[1]) 
          
        } else if(input$scale3[1] == "All"  & input$team3[1] != "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Team")) %>% 
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Team %in% input$team3[1])
          
        } else if(input$scale3[1] != "All"  & input$team3[1] != "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDFanonymised, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Scale", "Team")) %>% 
            dplyr::filter(High.Level.Cat %in% input$HighSkill3[1], Team %in% input$team3, Scale %in% input$scale3[1])
        }
        
        YearFirst <- rlang:: sym(as.character(YearLatest))
        YearSecond <- rlang:: sym(as.character(YearPrevious))
        
        if(as.character(YearLatest) %in% unique(DT$Year) & as.character(YearPrevious) %in% unique(DT$Year) & nrow(DT) > 1){
          
          DT <-  DT %>% 
            tidyr:: drop_na() %>%
            dplyr:: select(-meanGap, -Mean_Now) %>%
            tidyr:: spread(key = Year, value = Mean_Future) %>%
            dplyr:: mutate(Change = round(!!YearFirst - !!YearSecond,2))

          DF<- formattable(DT %>% dplyr:: arrange(Change),
                           
                           list(Change = color_tile( "lightpink", "lightgreen")))
          
          create_formattable(table_input = DF)
          
        } else{
          
          data.table:: as.data.table("No data to show in table")
          
        }
        
      })
      
      output$temporal_future_plot <- renderPlot({
        
        validate(
          need(input$HighSkill3, 'Select at least one Catagory'),
          need(input$scale3, 'Select at least one Scale'),
          need(input$team3, 'Select at least one Team'), 
          need(values$nYears > 1, 'Please load data with more than one year')
          
        )

        
        if(input$scale3[1] == "All" & input$team3[1] == "All"){
          
          DT <- dplyr::filter(SkillsDF, High.Level.Cat %in% input$HighSkill3) 
          DT <- makeTemporalDT(mytable = DT, grouping = c("Specific.Skill","High.Level.Cat", "Year"))   
          
        } else if(input$scale3[1] != "All"  & input$team3[1] == "All"){
          
          DT <- dplyr::filter(SkillsDF, High.Level.Cat %in% input$HighSkill3, Scale %in% input$scale3) 
          DT <- makeTemporalDT(mytable = DT, grouping = c("Specific.Skill","High.Level.Cat", "Year"))       
          
        } else if(input$scale3[1] == "All"  & input$team3[1] != "All"){
          
          DT <- dplyr::filter(SkillsDF, High.Level.Cat %in% input$HighSkill3, Team %in% input$team3) 
          DT <- makeTemporalDT(mytable = DT, grouping = c("Specific.Skill","High.Level.Cat", "Year")) 
          
        } else if(input$scale3[1] != "All"  & input$team3[1] != "All"){
          
          DT <- dplyr::filter(SkillsDFanonymised, High.Level.Cat %in% input$HighSkill3, Team %in% input$team3, Scale %in% input$scale3) 
          DT <- makeTemporalDT(mytable = DT, grouping = c("Specific.Skill","High.Level.Cat", "Year")) 

        }

        
        if(as.character(YearLatest) %in% unique(DT$Year) & as.character(YearPrevious) %in% unique(DT$Year) & nrow(DT) > 1){
          
          YearFirst <- rlang:: sym(as.character(YearLatest))
          
          DT <- DT %>% 
            tidyr:: drop_na() %>%
            dplyr:: select(-meanGap, -Mean_Now) %>%
            tidyr:: spread(key = Year, value = Mean_Future) %>%
            dplyr:: ungroup()
          
          DT <- dplyr:: arrange(DT, desc(eval(YearFirst)))
          DT <- dplyr:: mutate(DT, Specific.Skill=factor(Specific.Skill, levels=rev(Specific.Skill)))  
          
          df2 <-  DT %>% dplyr:: select(Specific.Skill, contains("20")) %>% tidyr::gather(group, value, -Specific.Skill)
          
          P<- makeDumbellPlot(DT = DT, df2 = df2, year1 = as.character(YearLatest),  year2 = as.character(YearPrevious), grouping = "Specific.Skill") +
            labs(x="Average Future Score")
          P
          
        } else{
          
          text = paste("No rows in DF")
          ggplot() + 
            annotate("text",size=8, label = text) + 
            theme_bw() +   theme(panel.grid.major=element_blank(),
                                 panel.grid.minor=element_blank())
        }
        
       
      })
      
      # GAP temporally 
      
      output$temporal_gap_plot <- renderPlot({
        
        
        
        validate(
          need(input$HighSkill3, 'Select at least one Catagory'),
          need(input$scale3, 'Select at least one Scale'),
          need(input$team3, 'Select at least one Team'), 
          need(values$nYears > 1, 'Please load data with more than one year')
          
        )

        YearFirst <- rlang:: sym(as.character(YearLatest))
        YearSecond <- rlang:: sym(as.character(YearPrevious))
        
        
        if(input$scale3[1] == "All" & input$team3[1] == "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year")) %>%
            dplyr::filter(High.Level.Cat %in% input$HighSkill3) %>%
              tidyr:: drop_na() %>%
              dplyr:: select(-Mean_Now, -Mean_Future) %>%
              tidyr:: spread(key = Year, value = meanGap) 
          
          if(YearLatest %in% colnames(DT) & YearPrevious %in% colnames(DT)){
            
            DT <- DT %>%
              dplyr:: mutate(class = ifelse((!! YearFirst - !! YearSecond) < 0, "green", "red")) %>%
              tidyr:: gather(key = Year, value = Position, - class, -Specific.Skill, -High.Level.Cat)
          }
          
          
        } else if(input$scale3[1] != "All"  & input$team3[1] == "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Scale")) %>%
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Scale %in% input$scale3) %>%
            tidyr:: drop_na() %>%
            dplyr:: select(-Mean_Now, -Mean_Future) %>%
            tidyr:: spread(key = Year, value = meanGap) 
          
          if(YearLatest %in% colnames(DT) & YearPrevious %in% colnames(DT)){
            
            DT <- DT %>%
              dplyr:: mutate(class = ifelse((!! YearFirst - !! YearSecond) < 0, "green", "red")) %>%
              tidyr:: gather(key = Year, value = Position, - class, -Specific.Skill, -High.Level.Cat, -Scale)
          }
          
          
        } else if(input$scale3[1] == "All"  & input$team3[1] != "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Team")) %>%
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Team %in% input$team3) %>%
            tidyr:: drop_na() %>%
            dplyr:: select(-Mean_Now, -Mean_Future) %>%
            tidyr:: spread(key = Year, value = meanGap) 
          
          if(YearLatest %in% colnames(DT) & YearPrevious %in% colnames(DT) & nrow(DT) > 1){
            
            DT <- DT %>%
              dplyr:: mutate(class = ifelse((!! YearFirst - !! YearSecond) < 0, "green", "red")) %>%
              tidyr:: gather(key = Year, value = Position, - class, -Specific.Skill, -High.Level.Cat, -Team)
          }
          
          
        } else if(input$scale3[1] != "All"  & input$team3[1] != "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDFanonymised, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Scale", "Team")) %>%
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Scale %in% input$scale3, Team %in% input$team3) %>%
            tidyr:: drop_na() %>%
            dplyr:: select(-Mean_Now, -Mean_Future) %>%
            tidyr:: spread(key = Year, value = meanGap) 
          
          if(YearLatest %in% colnames(DT) & YearPrevious %in% colnames(DT)){
            
            DT <- DT %>%
              dplyr:: mutate(class = ifelse((!! YearFirst - !! YearSecond) < 0, "green", "red")) %>%
              tidyr:: gather(key = Year, value = Position, - class, -Specific.Skill, -High.Level.Cat, -Scale, -Team)
          }
        }
        
    if("Year" %in% colnames(DT)){
 
        if(YearLatest %in% unique(DT$Year) & YearPrevious %in% unique(DT$Year)){
        
        P<-   makeSlopePlot(DT = DT, xaxis = "Year", yaxis = "Position", group = "Specific.Skill", Year1 = as.character(YearLatest), Year2 = as.character(YearPrevious))
        P   }
      
        } else{
          
          text = paste("No rows in DF")
          ggplot() + 
            annotate("text",size=8, label = text) + 
            theme_bw() +   theme(panel.grid.major=element_blank(),
                                 panel.grid.minor=element_blank())
        }
      })

      output$temporal_gap <- renderDataTable({
        
        
        
        validate(
          need(input$HighSkill3, 'Select at least one Catagory'),
          need(input$scale3, 'Select at least one Scale'),
          need(input$team3, 'Select at least one Team'), 
          need(values$nYears > 1, 'Please load data with more than one year')
          
        )
        
        if(input$scale3[1] == "All" & input$team3[1] == "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year")) %>%
            dplyr::filter(High.Level.Cat %in% input$HighSkill3) 
          
        } else if(input$scale3[1] != "All"  & input$team3[1] == "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Scale")) %>%
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Scale %in% input$scale3) 
          
          
        } else if(input$scale3[1] == "All"  & input$team3[1] != "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDF, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Team")) %>%
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Team %in% input$team3)         
          
          
        } else if(input$scale3[1] != "All"  & input$team3[1] != "All"){
          
          DT <- makeTemporalDT(mytable = SkillsDFanonymised, grouping = c("Specific.Skill","High.Level.Cat", "Year", "Scale", "Team")) %>%
            dplyr::filter(High.Level.Cat %in% input$HighSkill3, Scale %in% input$scale3, Team %in% input$team3)          
        }
        
        YearFirst <- rlang:: sym(as.character(YearLatest))
        YearSecond <- rlang:: sym(as.character(YearPrevious))
        
        if(YearLatest %in% unique(DT$Year) & YearPrevious %in% unique(DT$Year) & nrow(DT) > 1){
        
        DT <-  DT %>%
          tidyr:: drop_na() %>%
          dplyr:: select(-Mean_Now, -Mean_Future) %>%
          tidyr:: spread(key = Year, value = meanGap) %>%
          dplyr:: ungroup() %>%
          dplyr:: mutate(Change = round(!!YearFirst - !!YearSecond,2))
        
        # create_formatted_table(DT %>% dplyr:: arrange(desc(Change))) %>%
        #   
        #   formatStyle("Change",
        #               background = styleColorBar(c(0,0.3), 'lightblue'),
        #               backgroundSize = '98% 60%',
        #               backgroundRepeat = 'no-repeat',
        #               backgroundPosition = 'left') 
        
        DF<- formattable(DT %>% dplyr:: arrange(Change),
                         
                         list(Change = color_tile("lightpink", "lightgreen")))
        
        create_formattable(table_input = DF)
        } else{
          
          data.table:: as.data.table("No data to show in table")
          
        }
        
      })
      

    }
      
      
     
    
  })

}