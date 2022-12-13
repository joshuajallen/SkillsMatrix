statsModuleUI <- function(id,values) {
  ns <- NS(id)
  
  
  ### Dashboard row 1 ---------------------------------------------------------
  fluidPage(
    
    fluidRow(
      
      
      div(style="display:inline-block;width:98%;text-align: right;",
          actionButton(ns("info"), label = "Info", icon = icon("info"))),
      
      column(6, box(
        ##
        h3("Required Inputs:"), 
        p(strong("Please select skills catagories or year")),
        
        
        selectizeInput(ns("HighSkill2"),
                       "High Level Skill Catagory",
                       choices = "All",
                       multiple = TRUE), 
        
        selectizeInput(ns("year2"),
                       "Select A Year",
                       choices = "2019",
                       multiple = FALSE), width = 12)),
      
      column(6,box(
        ##
        h3("Optional Inputs:"), 
        p(strong("Please select a scale(s), team(s) or both")),
        
        
        
        selectizeInput(ns("scale2"),
                       "Select A Scale",
                       choices = "All",
                       multiple = TRUE), 
        
        selectizeInput(ns("team2"),
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
        ,selected = "Current Skill Distribution"
        

    ,tabPanel(
      
      title = "Current Skill Distribution"
      ,icon = icon("bar-chart-o"),

      h4(
        "The chart/data table below shows the current count of individuals across each skill catagory, by level, you can select the following inputs to disaggregate the data by; "
      ),
      h5(tags$ul(
        tags$li("Skill catagory - upper level of the skills hierarchy, e.g. soft skills"),
        tags$li("Scale - select one or more scales across the business area"),
        tags$li("Team - select one or more teams across the business area")
        
      )),
      br(),
      hr(),
      
      fluidRow(column(12,  box(collapsible = TRUE, 
                               solidHeader = TRUE
                               ,width = 12
                               ,plotlyOutput(ns("stock_now_plot"), height = "800px", width = "100%")
      ))),

      br(),
      hr(),
      
      fluidRow(column(12,  box(collapsible = TRUE, 
        solidHeader = TRUE
        ,width = 12
        
        ,dataTableOutput(ns("stock_now"))
      )))
        
    )
    
    ,tabPanel(
      
      title = "Expected Skill Distribution"
      ,icon = icon("bar-chart-o"),
      
      h4(
        "The chart/data table below shows the expected count of individuals across each skill catagory, by level, you can select the following inputs to disaggregate the data by; "
      ),
      h5(tags$ul(
        tags$li("Skill catagory - upper level of the skills hierarchy, e.g. soft skills"),
        tags$li("Scale - select one or more scales across the business area"),
        tags$li("Team - select one or more teams across the business area")
        
      )),
      br(),
      hr(),
      
      fluidRow(column(12,  box(collapsible = TRUE, 
                               solidHeader = TRUE
                               ,width = 12
                               ,plotlyOutput(ns("stock_future_plot"), height = "800px", width = "100%")
      ))),

      br(),
      hr(),

      
      fluidRow(column(12,  box(collapsible = TRUE, 
        solidHeader = TRUE
        ,width = 12
        
        ,dataTableOutput(ns("stock_future"))
      )))
    )
      
      ,tabPanel(
        
        title = "Year on Year Stock Comparison"
        ,icon = icon("line-chart"),

        h4(
          "The chart/data table below shows the total counts of individual by skill catagory as a time series, you can select the following inputs to disaggregate the data by; "
        ),
        h5(tags$ul(
          tags$li("Skill catagory - upper level of the skills hierarchy, e.g. soft skills"),
          tags$li("Scale - select one or more scales across the business area"),
          tags$li("Team - select one or more teams across the business area")
          
        )),
        
        
        plotlyOutput(ns("stock_comparison_plot"), height = "1000px", width = "100%"),
        
        
        br(),
        hr(),
        
        
        fluidRow(column(12,  box(collapsible = TRUE, 
          solidHeader = TRUE
          ,width = 12
          
          ,dataTableOutput(ns("stock_comparison"))
        )))
      )
    )
      )
    )
  


  
  
}

statsModule <- function(input, output, session, values) {
  
    observeEvent(values$weHaveData, {
        
        if(values$weHaveData){
          
          SkillsDF=values$SkillsDF

          
          updateSelectizeInput(session, "HighSkill2", choices=c(unique(SkillsDF$High.Level.Cat)), selected = unique(SkillsDF$High.Level.Cat)[2])
          updateSelectizeInput(session, "year2", choices=c(unique(SkillsDF$Year)), selected = max(unique(SkillsDF$Year)))
          updateSelectizeInput(session, "scale2", choices=c("All", unique(SkillsDF$Scale)), selected = "All")
          updateSelectizeInput(session, "team2", choices=c("All", unique(SkillsDF$Team)), selected = "All")
          
          
        }
        
      })
      
      observeEvent(values$weHaveData, {
        
        if(values$weHaveData == TRUE){
          
          Scale <-input$scale2
          Team <-input$team2
          SkillsDF <- values$SkillsDF
          YearLatest = max(unique(SkillsDF$Year))
          YearPrevious = max(unique(SkillsDF$Year)[which(!unique(SkillsDF$Year) %in% max(unique(SkillsDF$Year)))])
          SkillsDFanonymised <- values$SkillsDFanonymised
          
          isolate(
            {
              values$Scale <- Scale
              values$Team <- Team
              values$YearLatest = YearLatest
              values$YearPrevious = YearPrevious
            })
        }
        
      })
      
      observeEvent(values$weHaveData, {
        
        if(values$weHaveData == TRUE){
          
          SkillsDF <- values$SkillsDF
          SkillsDFanonymised <- values$SkillsDFanonymised
          YearLatest = max(unique(SkillsDF$Year))
          YearPrevious = max(unique(SkillsDF$Year)[which(!unique(SkillsDF$Year) %in% max(unique(SkillsDF$Year)))])
          #--------------------------------------------------------------------------------------------------------------------------------------------------
          #Current dsitribution
          
          output$stock_now <- renderDataTable({
            
            validate(
              need(input$HighSkill2, 'Select at least one Catagory'),
              need(input$scale2, 'Select at least one Scale'),
              need(input$team2, 'Select at least one Team')
            )
            
            if(input$scale2[1] == "All" & input$team2[1] == "All"){
              
              grouping=c("Specific.Skill", "High.Level.Cat", "Rank.Now")
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = FALSE ,filter0Now = FALSE)
              
              
            } else if(input$scale2[1] != "All"  & input$team2[1] == "All"){
              
              grouping=c("Specific.Skill", "Scale", "High.Level.Cat", "Rank.Now")
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = FALSE, filter0Now = FALSE)
              
              
            } else if(input$scale2[1] == "All"  & input$team2[1] != "All"){
            
              grouping=c("Specific.Skill","Team", "High.Level.Cat", "Rank.Now")
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = input$team2, filter0Now = FALSE)
              
              
            } else if(input$scale2[1] != "All"  & input$team2[1] != "All"){
              
              grouping=c("Specific.Skill", "Scale","Team", "High.Level.Cat", "Rank.Now")
              DT<- makeStockDT(mytable = SkillsDFanonymised, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = input$team2, filter0Now = FALSE)
              
              
            }
            
            
            if(!is.null(DT)){
              
              DF<- formattable(DT %>%  arrange(desc(Total)),
                               
                               list(Total = color_bar("lightblue")))
              
              create_formattable(table_input = DF)
              
            } else{
              
              data.table:: as.data.table("No data to show in table")
              
            }
            
          })
          
          output$stock_now_plot <- renderPlotly({
            
            validate(
              need(input$HighSkill2, 'Select at least one Catagory'),
              need(input$scale2, 'Select at least one Scale'),
              need(input$team2, 'Select at least one Team')
            )
            
            if(input$scale2[1] == "All" & input$team2[1] == "All"){
              
              grouping=c("Specific.Skill", "High.Level.Cat", "Rank.Now")
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = FALSE ,filter0Now = FALSE)

              
            } else if(input$scale2[1] != "All"  & input$team2[1] == "All"){
              
              grouping=c("Specific.Skill", "Scale", "High.Level.Cat", "Rank.Now")
              
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = FALSE, filter0Now = FALSE)
      
              
            } else if(input$scale2[1] == "All"  & input$team2[1] != "All"){
              
              grouping=c("Specific.Skill", "Team", "High.Level.Cat", "Rank.Now")
              
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = input$team2, filter0Now = FALSE)
          
              
            } else if(input$scale2[1] != "All"  & input$team2[1] != "All"){
              
              grouping=c("Specific.Skill", "Scale","Team", "High.Level.Cat", "Rank.Now")
              
              DT<- makeStockDT(mytable = SkillsDFanonymised, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = input$team2, filter0Now = FALSE)

              
            }

            
            if(!is.null(DT)){
              
              labels = c("No idea but want to learn (0)", "Basic (1)", "Good (2)", "High (3)","Expert (4)")
              
              summarySt <- DT %>%  
                dplyr:: select(Specific.Skill, High.Level.Cat, contains( "No idea but want to learn (0)"), 
                               contains("Basic (1)"), contains("Good (2)"), contains("High (3)"), contains("Expert (4)" )) %>%
                tidyr:: gather(key = Score, value = Position, -Specific.Skill, -High.Level.Cat) %>%
                dplyr:: mutate(rank = rank(Position, ties.method = 'first')) %>%
                dplyr:: mutate(Score = factor(Score, levels = labels)) 
  
              P<- makeStockBarWithLevel(df = summarySt, xlabel = "Specific.Skill")
              ggplotly(P)
              
              
            } else{
              
              text = paste("No rows in DF")
              ggplot() + 
                annotate("text",size=8, label = text) + 
                theme_bw() +   
                theme(panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank())
            }
            
          })
          
          #--------------------------------------------------------------------------------------------------------------------------------------------------
          #Future dsitribution
          
          output$stock_future <- renderDataTable({
            
            validate(
              need(input$HighSkill2, 'Select at least one Catagory'),
              need(input$scale2, 'Select at least one Scale'),
              need(input$team2, 'Select at least one Team')
            )
            
            
            if(input$scale2[1] == "All" & input$team2[1] == "All"){
              
              grouping=c("Specific.Skill", "High.Level.Cat", "Rank.Future")
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = FALSE ,filter0Now = FALSE, filter0Future = TRUE)
              
              
            } else if(input$scale2[1] != "All"  & input$team2[1] == "All"){
              
              grouping=c("Specific.Skill", "Scale", "High.Level.Cat", "Rank.Future")
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = FALSE, filter0Now = FALSE, filter0Future = TRUE)
              
              
            } else if(input$scale2[1] == "All"  & input$team2[1] != "All"){
              
              grouping=c("Specific.Skill","Team", "High.Level.Cat", "Rank.Future")
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = input$team2, filter0Now = FALSE, filter0Future = TRUE)
              
              
            } else if(input$scale2[1] != "All"  & input$team2[1] != "All"){
              
              grouping=c("Specific.Skill", "Scale","Team", "High.Level.Cat", "Rank.Future")
              DT<- makeStockDT(mytable = SkillsDFanonymised, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = input$team2, filter0Now = FALSE, filter0Future = TRUE)
              
              
            }
            
            if(!is.null(DT)){
              
            DF<- formattable(DT %>%  arrange(desc(Total)),
                             
                             list(Total = color_bar("lightblue")))
            
            create_formattable(table_input = DF)
            } else{
              
              data.table:: as.data.table("No data to show in table")
              
            }
            
          })
          
          output$stock_future_plot <- renderPlotly({
            
            validate(
              need(input$HighSkill2, 'Select at least one Catagory'),
              need(input$scale2, 'Select at least one Scale'),
              need(input$team2, 'Select at least one Team')
            )
            
            if(input$scale2[1] == "All" & input$team2[1] == "All"){
              
              grouping=c("Specific.Skill", "High.Level.Cat", "Rank.Future")
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = FALSE ,filter0Future = TRUE)
              
            } else if(input$scale2[1] != "All"  & input$team2[1] == "All"){
              
              grouping=c("Specific.Skill", "Scale", "High.Level.Cat", "Rank.Future")
              
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = FALSE, filter0Future = TRUE)
              
            } else if(input$scale2[1] == "All"  & input$team2[1] != "All"){
              
              grouping=c("Specific.Skill", "Team", "High.Level.Cat", "Rank.Future")
              
              DT<- makeStockDT(mytable = SkillsDF, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = input$team2, filter0Future = TRUE)
              
            } else if(input$scale2[1] != "All"  & input$team2[1] != "All"){
              
              grouping=c("Specific.Skill", "Scale","Team", "High.Level.Cat", "Rank.Future")
              
              DT<- makeStockDT(mytable = SkillsDFanonymised, grouping = grouping, SelectYear = input$year2, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = input$team2, filter0Future = TRUE)

              
            }
            
            if(!is.null(DT)){
              
              labels = c("No idea but want to learn (0)", "Basic (1)", "Good (2)", "High (3)","Expert (4)")
              
              summarySt <- DT %>%  
                dplyr:: select(Specific.Skill, High.Level.Cat, contains( "No idea but want to learn (0)"),
                               contains("Basic (1)"), contains("Good (2)"), contains("High (3)"), contains("Expert (4)" )) %>%
                tidyr:: gather(key = Score, value = Position, -Specific.Skill, -High.Level.Cat) %>%
                dplyr:: mutate(rank = rank(Position, ties.method = 'first')) %>%
                dplyr:: mutate(Score = factor(Score, levels = labels)) 
              
              P<- makeStockBarWithLevel(df = summarySt, xlabel = "Specific.Skill")
              ggplotly(P)
              
              
            } else{
              
              text = paste("No rows in DF")
              ggplot() + 
                annotate("text",size=8, label = text) + 
                theme_bw() +   
                theme(panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank())
            }
              
      })
          
          #--------------------------------------------------------------------------------------------------------------------------------------------------
          #Now vs Future dsitribution

          output$stock_comparison <- renderDataTable({

            validate(
              need(input$HighSkill2, 'Select at least one Catagory'),
              need(input$scale2, 'Select at least one Scale'),
              need(input$team2, 'Select at least one Team')
            )


            if(input$scale2[1] == "All" & input$team2[1] == "All"){

              grouping=c("Specific.Skill", "High.Level.Cat")
              DT<- makeStockDiffDT(mytable = SkillsDF, grouping = grouping, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = FALSE ,filter0Now = FALSE, filter0Future = TRUE)

            } else if(input$scale2[1] != "All"  & input$team2[1] == "All"){

              grouping=c("Specific.Skill", "Scale", "High.Level.Cat")
              DT<- makeStockDiffDT(mytable = SkillsDF, grouping = grouping, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = FALSE ,filter0Now = FALSE, filter0Future = TRUE)


            } else if(input$scale2[1] == "All"  & input$team2[1] != "All"){

              grouping=c("Specific.Skill","Team", "High.Level.Cat")
              DT<- makeStockDiffDT(mytable = SkillsDF, grouping = grouping, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = input$team2 ,filter0Now = FALSE, filter0Future = TRUE)


            } else if(input$scale2[1] != "All"  & input$team2[1] != "All"){

              grouping=c("Specific.Skill", "Scale","Team", "High.Level.Cat")
              DT<- makeStockDiffDT(mytable = SkillsDFanonymised, grouping = grouping, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = input$team2 ,filter0Now = FALSE, filter0Future = TRUE)


            }

            if(!is.null(DT)){

              DF<- formattable(DT %>%  arrange(desc(Change)),

                               list(Change = color_bar("lightblue"),
                                    `% Change` = color_bar("#CAC0B6"))
              )

              create_formattable(table_input = DF)

            } else{

              data.table:: as.data.table("No data to show in table")

            }

          })
          
          
          output$stock_comparison_plot <- renderPlotly({
              
            validate(
              need(input$HighSkill2, 'Select at least one Catagory'),
              need(input$scale2, 'Select at least one Scale'),
              need(input$team2, 'Select at least one Team')
            )
              
              
              if(input$scale2[1] == "All" & input$team2[1] == "All"){
                
                grouping=c("Year","High.Level.Cat","Specific.Skill", "Rank.Now")
                DT<- makeBreakdownDT(df = SkillsDF, grouping = grouping, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = FALSE, filter0Now = FALSE, filter0Future = FALSE) 
                
              } else if(input$scale2[1] != "All"  & input$team2[1] == "All"){
                
                grouping=c("Year","High.Level.Cat","Specific.Skill","Scale", "Rank.Now")
                DT<- makeBreakdownDT(df = SkillsDF, grouping = grouping, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = FALSE, filter0Now = FALSE, filter0Future = FALSE) 
                
              } else if(input$scale2[1] == "All"  & input$team2[1] != "All"){
                
                grouping=c("Year","High.Level.Cat","Specific.Skill","Team", "Rank.Now")
                DT<- makeBreakdownDT(df = SkillsDF, grouping = grouping, SelectHighCat = input$HighSkill2, SelectScale = FALSE, SelectTeam = input$team2, filter0Now = FALSE, filter0Future = FALSE) 
                
              } else if(input$scale2[1] != "All"  & input$team2[1] != "All"){
                
                grouping=c("Year","High.Level.Cat","Specific.Skill", "Scale", "Team", "Rank.Now")
                DT<- makeBreakdownDT(df = SkillsDFanonymised, grouping = grouping, SelectHighCat = input$HighSkill2, SelectScale = input$scale2, SelectTeam = input$team2, filter0Now = FALSE, filter0Future = FALSE) 
                
              }
              
              if(!is.null(DT)){

                P<- makeBreakdownChart(df = DT, xlabel = "Specific.Skill", ylabel = "Count") + 
                    labs(title="Quantity of people with this skill per year", x = "Count", y = "Specific Skill") +
                    theme(panel.grid.minor = element_line(colour = "grey50", size = 0.25))
                
                ggplotly(P)
                
                
              } else{
                
                text = paste("No rows in DF")
                ggplot() + 
                  annotate("text",size=8, label = text) + 
                  theme_bw() +   
                  theme(panel.grid.major=element_blank(),
                        panel.grid.minor=element_blank())
              }
              
        })
          
          
        }}   
      )}  





          
  

      