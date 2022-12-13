
gapModuleUI <- function(id,values) {
  ns <- NS(id)
  
  
  ### Dashboard row 1 ---------------------------------------------------------
  fluidPage(
    
    fluidRow(
      
      column(6, box(collapsible = TRUE, 
        ##
        h3("Required Inputs:"), 
        p(strong("Please select skills catagories or year")),
        
        
        selectizeInput(ns("HighSkill1"),
                       "High Level Skill Catagory",
                       choices = "All",
                       multiple = TRUE), 
        
        selectizeInput(ns("year1"),
                       "Select A Year",
                       choices = "2019",
                       multiple = FALSE), width = 12)),
      
      column(6,box(collapsible = TRUE, 
        ##
        h3("Optional Inputs:"), 
        p(strong("Please select a scale(s), team(s) or both")),
        
        
        
        selectizeInput(ns("scale"),
                       "Select A Scale",
                       choices = "All",
                       multiple = TRUE), 
        
        selectizeInput(ns("team"),
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
        ,selected = "Skill GAP Analysis"
        
        
        ,tabPanel(
          

          
          title = "Skill GAP Analysis"
          ,icon = icon("table"),
          
          
          h4(
            "Please select the required/optional user inputs below to filter the data. The metrics in the chart/table below can be intepreted as follows; "
          )
          
          ,br()
          
          ,h5(tags$ul(
            tags$li("Count - number of people in the population selected"),
            tags$li("Mean gap - is the average difference between the current and future scores across the selected population, a negative gap means that the current score is less than the future score"),
            tags$li("Abs gap (%) - is the % of people with a gap, of the population shown in the 'Count' column"),
            tags$li("Mean future - is the average future score, indicating the level of the skill catagory")
            
          )),
         
          fluidRow(column(6, box(collapsible = TRUE, plotlyOutput(ns("gap_scale_plot"), height = "900px", width = "100%"), width = 12)), 
                   column(6, box(collapsible = TRUE, plotlyOutput(ns("gap_team_plot"), height = "900px", width = "100%"), width = 12))
),
          
          fluidRow(column(12,  box(collapsible = TRUE,  
            solidHeader = TRUE
            ,width = 12

            ,dataTableOutput(ns("gap_scale"))
          )))

        
        )
        
        ,tabPanel(
          
          title = "GAPS distribution"
          ,icon = icon("bar-chart-o"),
          
          p(code("Please select high level skills catagory from the drop down")),
          br(), 
          hr()
          
          ,verbatimTextOutput(ns("gap_dist_metrics"))
        
          
          ,fluidRow(column(6, box(title = "Box plot of skill gaps, by user defined grouping", collapsible = TRUE, plotlyOutput(ns("gap_distribution_plot"), height = "900px", width = "100%"), width = 12)), 
                   column(6, box(title = "Diverging lollipop chart of skill gaps, by user defined grouping", collapsible = TRUE, plotlyOutput(ns("gap_lollipop_plot"), height = "900px", width = "100%"), width = 12))
            
          )
          
        )

      )
    )
)

  
  
}

gapModule <- function(input, output, session, values) {
  
  observeEvent(values$weHaveData, {
    
    if(values$weHaveData){
      
      SkillsDF=values$SkillsDF
      
      updateSelectizeInput(session, "HighSkill1", choices=c(unique(SkillsDF$High.Level.Cat)), selected = unique(SkillsDF$High.Level.Cat)[2])
      updateSelectizeInput(session, "year1", choices=c(unique(SkillsDF$Year)), selected = max(unique(SkillsDF$Year)))
      updateSelectizeInput(session, "scale", choices=c("All", unique(SkillsDF$Scale)), selected = "All")
      updateSelectizeInput(session, "team", choices=c("All", unique(SkillsDF$Team)), selected = "All")
      
      
    }
    
  })
    
    observeEvent(values$weHaveData, {
      
      if(values$weHaveData == TRUE){
 
          Scale <-input$scale
          Team <-input$team
          
          isolate(
            {
              values$Scale <- Scale
              values$Team <- Team
              
            })
      }
          
        })


    
    observeEvent(values$weHaveData, {
      
      if(values$weHaveData){
        
        SkillsDF=values$SkillsDF
        Skill1= values$Skill1 
        SkillsDFanonymised <- values$SkillsDFanonymised
        
        #------------------------------------------------------------------------------------------------------------------------

        
        output$gap_scale <- renderDataTable({
          
          validate(
            need(input$HighSkill1, 'Select at least one catagory'),
            need(input$scale, 'Select at least one scale'),
            need(input$team, 'Select at least one Team')
            
          )
          
          if(input$scale[1] == "All" & input$team[1] == "All"){
            
         DT<- makeGapDT(mytable = SkillsDF, grouping = c("High.Level.Cat", "Specific.Skill", "Year")) %>%
              dplyr:: filter(High.Level.Cat %in% input$HighSkill1, Year %in% input$year1) %>%
              dplyr:: arrange(-desc(meanGap))
            
          } else if(input$scale[1] != "All"  & input$team[1] == "All"){
            
         DT<- makeGapDT(mytable = SkillsDF, grouping = c("High.Level.Cat", "Specific.Skill", "Scale",  "Year")) %>%
              dplyr:: filter(High.Level.Cat %in% input$HighSkill1, Year %in% input$year1, Scale %in% input$scale) %>%
              dplyr:: arrange(-desc(meanGap))
            
          } else if(input$scale[1] == "All"  & input$team[1] != "All"){
            
            
            DT<- makeGapDT(mytable = SkillsDF, grouping = c("High.Level.Cat", "Specific.Skill", "Team",  "Year")) %>%
              dplyr:: filter(High.Level.Cat %in% input$HighSkill1, Year %in% input$year1, Team %in% input$team) %>%
              dplyr:: arrange(-desc(meanGap))
            
          } else if(input$scale[1] != "All"  & input$team[1] != "All"){
            
            DT<- makeGapDT(mytable = SkillsDFanonymised, grouping = c("High.Level.Cat", "Specific.Skill", "Team","Scale",  "Year")) %>%
              dplyr:: filter(High.Level.Cat %in% input$HighSkill1, Year %in% input$year1, Team %in% input$team, Scale %in% input$scale) %>%
              dplyr:: arrange(-desc(meanGap))
            
          }
          
          if(nrow(DT) > 0){
 
          
          DF<- formattable(DT,
                           
                           list(Count = color_bar("#CAC0B6"),
                                 meanGap = color_tile("lightpink", "lightgreen"),
                                `AbsGap %` = color_tile("lightgreen", "lightpink"),
                                Mean_Now = color_bar("lightblue"),
                                Mean_Future = color_bar("lightblue")))
          
          create_formattable(table_input = DF)
          
          } else{
            
            data.table:: as.data.table("No data to show in table")
            
          }
          
        })
        
        output$gap_scale_plot <- renderPlotly({
          
          validate(
            need(input$HighSkill1, 'Select at least one catagory'),
            need(input$scale, 'Select at least one scale')
            
          )

          DT<- makeGapDT(mytable = SkillsDF, grouping = c("High.Level.Cat", "Specific.Skill", "Scale",  "Year")) %>%
            dplyr:: filter(High.Level.Cat %in% input$HighSkill1, Year %in% input$year1) %>%
            dplyr:: arrange(Specific.Skill, -desc(meanGap))
          
          if(nrow(DT) >0){
            
            
            P<- makeHeatMap(DT = DT, xaxis = "Scale", yaxis = "Specific.Skill")  + 
              labs(title="HeatMap of skills GAPs, by Scale", 
                   subtitle="GAP = calculate as difference between current and future score", 
                   y="Skill", 
                   x="Scale") + 
              theme(axis.text.x = element_text(angle = 45), 
                    axis.text.y = element_text(angle = 30))
            
            ggplotly(P)
            
          } else{
            
            text = paste("No rows in DF")
            ggplot() + 
              annotate("text",size=8, label = text) + 
              theme_bw() +   theme(panel.grid.major=element_blank(),
                                   panel.grid.minor=element_blank())
          }
          

          
        })

        output$gap_team_plot <- renderPlotly({

          validate(
            need(input$HighSkill1, 'Select at least one catagory'),
            need(input$team, 'Select at least one team')

          )

          DT<- makeGapDT(mytable = SkillsDF, grouping = c("High.Level.Cat", "Specific.Skill", "Team",  "Year")) %>%
            dplyr:: filter(High.Level.Cat %in% input$HighSkill1, Year %in% input$year1) %>%
            dplyr:: arrange(Specific.Skill, -desc(meanGap))

        if(nrow(DT) >0){
          
          
          P<- makeHeatMap(DT = DT, xaxis = "Team", yaxis = "Specific.Skill") +
            labs(title="HeatMap of skills GAPs, by Team",
                 subtitle="GAP = calculate as difference between current and future score",
                 y="Skill",
                 x="Team") + 
            theme(axis.text.x = element_text(angle = 45), 
                  axis.text.y = element_text(angle = 30))
          
          ggplotly(P)
          
        } else{
          
          text = paste("No rows in DF")
          ggplot() + 
            annotate("text",size=8, label = text) + 
            theme_bw() +   theme(panel.grid.major=element_blank(),
                                 panel.grid.minor=element_blank())
        }


        })
        
        output$gap_dist_metrics <- renderText({
          
        "The interpration of the following chart is as follows; \n
         The box plot shows the distribution of skill gaps for the selected population. A gap less than zero (dotten line) suggests the current score is less than the future score for that skill \n
         The diverging lollipop chart shows the average skill gaps for the selected population, in ascending order "
          

        })
        
        output$gap_distribution_plot <- renderPlotly({
          
          validate(
            need(input$HighSkill1, 'Select at least one catagory'),
            need(input$team, 'Select at least one team'),
            need(input$scale, 'Select at least one scale')
            
            
          )
          
          if(input$scale == "All" & input$team == "All"){
            
           DT<- 
              SkillsDF %>%       
              dplyr:: filter(High.Level.Cat %in% input$HighSkill1, Year %in% input$year1)
            
            
          } else if(input$scale != "All" & input$team == "All"){
            
  
            DT<- 
              SkillsDF %>%       
              dplyr:: filter(High.Level.Cat %in% input$HighSkill1, Year %in% input$year1, Scale %in% input$scale)
            
          } else if(input$scale == "All" & input$team != "All"){
            
          DT<- 
            SkillsDF %>%       
            dplyr:: filter(High.Level.Cat %in% input$HighSkill1, Year %in% input$year1, Team %in% input$team)
          
        } else if(input$scale != "All" & input$team != "All"){


          DT<- 
            SkillsDFanonymised %>%       
            dplyr:: filter(High.Level.Cat %in% input$HighSkill1, Year %in% input$year1, Team %in% input$team, Scale %in% input$scale)
          
        }
          
          if(nrow(DT) > 0){

          P<- 
            makeBoxPlot(DT, xaxis = "Specific.Skill", yaxis = "Gap") + 
            labs(title="", 
                 subtitle="GAP = calculate as difference between current and future score", 
                 y="Average GAP", 
                 x="Skill") 
          
          
          ggplotly(P)
          
          } else{
            
            text = paste("No rows in DF")
            ggplot() + 
              annotate("text",size=8, label = text) + 
              theme_bw() +   theme(panel.grid.major=element_blank(),
                                   panel.grid.minor=element_blank())
          }
            
 
          
        })
        
        output$gap_lollipop_plot <- renderPlotly({
          
          validate(
            need(input$HighSkill1, 'Select at least one catagory'),
            need(input$team, 'Select at least one team'),
            need(input$scale, 'Select at least one scale')
            
            
          )
          
          if(input$scale[1] == "All" & input$team[1] == "All"){
            
            DT <- dplyr:: filter(SkillsDF, High.Level.Cat %in% input$HighSkill1, Year %in% input$year1) 
            DT <- makeGapDT(mytable = DT, grouping = c("High.Level.Cat", "Specific.Skill", "Year")) %>%
                  dplyr:: arrange(-desc(meanGap))
            
            
          } else if(input$scale[1] != "All" & input$team[1] == "All"){
            
            DT <- dplyr:: filter(SkillsDF, High.Level.Cat %in% input$HighSkill1, Year %in% input$year1,  Scale %in% input$scale)
            DT <- makeGapDT(mytable = DT, grouping = c("High.Level.Cat", "Specific.Skill", "Year")) %>%
                  dplyr:: arrange(-desc(meanGap))
            
          } else if(input$scale[1] == "All" & input$team[1] != "All"){

            DT <- dplyr:: filter(SkillsDF, High.Level.Cat %in% input$HighSkill1, Year %in% input$year1, Team %in% input$team)
            DT <- makeGapDT(mytable = DT, grouping = c("High.Level.Cat", "Specific.Skill", "Year")) %>%
                  dplyr:: arrange(-desc(meanGap))

          } else if(input$scale[1] != "All" & input$team[1] != "All"){
            
            DT <- dplyr:: filter(SkillsDFanonymised, High.Level.Cat %in% input$HighSkill1, Year %in% input$year1, Team %in% input$team, Scale %in% input$scale)
            DT <- makeGapDT(mytable = DT, grouping = c("High.Level.Cat", "Specific.Skill", "Year")) %>%
                  dplyr:: arrange(-desc(meanGap))

          }
          
          if(nrow(DT) > 0){
            
            P<- ggplot(DT, aes(x=reorder(Specific.Skill, -meanGap), y= meanGap, label=meanGap)) + 
              geom_point(stat='identity', fill= "#A51140", size=12)  +
              geom_segment(aes(y = 0, 
                               x = Specific.Skill, 
                               yend = meanGap, 
                               xend = Specific.Skill), 
                           color = "#A51140") +
              geom_text(color="white", size=4) +
              labs(title="", 
                   subtitle="Gaps", y = "Average Skill Gap", x = "Specific.Skill") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
              coord_flip() + 
              theme_skillsMatrix(base_size = 12) +     
              theme(
                panel.grid.major.x = element_blank(),
                panel.border = element_blank(),
                axis.ticks.x = element_blank(), 
                panel.grid.major = element_blank()) 
            
            ggplotly(P)
            
          } else{
            
            text = paste("No rows in DF")
            ggplot() + 
              annotate("text",size=8, label = text) + 
              theme_bw() +   theme(panel.grid.major=element_blank(),
                                   panel.grid.minor=element_blank())
          }
          
          
          
        })

    
      }
      
    })
    

}
