drilldownModuleUI <- function(id,values) {
  ns <- NS(id)
  
  
  ### Dashboard row 1 ---------------------------------------------------------
  fluidPage(

    
    fluidRow(
      tabBox(
        
        id = "tbsExample"
        ,height = "100%"
        ,width = 12
        ,side = "left"
        ,selected = "Skill Level Analysis"
        
        
        ,tabPanel(

          title = "Skill Level Analysis"
          ,icon = icon("table"),
          
          fluidRow(
          box(collapsible = TRUE,        
            
          selectizeInput(ns("skill"),
                           "Select A Skill",
                           choices = "Please select",
                           multiple = FALSE),
          
          selectizeInput(ns("year3"),
                         "Select A Year",
                         choices = "2019",
                         multiple = FALSE), width = 6)
          ),

          p(strong("Please select a skill and year from the dropdown above")),
          hr(), 
          
          fluidRow(column(12, box("The table below shows the experienced users of the selected skill", collapsible = TRUE, dataTableOutput(ns("skill_users")), width = 12))),
          hr(), 
          
          fluidRow(column(6, box("The table below shows the summary stats for the selected skill", collapsible = TRUE,dataTableOutput(ns("skill_summary")), width = 12)), 
                   column(6, box(collapsible = TRUE,plotlyOutput(ns("skill_distribution"), height = "400px", width = "100%"), width = 12))),
          
          fluidRow(column(6, box("The table below shows the count of individuals current scores, by level", collapsible = TRUE,plotlyOutput(ns("skill_distribution_now"), height = "400px", width = "100%"), width = 12)), 
                   column(6, box("The table below shows the count of individuals future state scores, by level", collapsible = TRUE,plotlyOutput(ns("skill_distribution_future"), height = "400px", width = "100%"), width = 12))
          )

          
          
        )

        ,tabPanel(
          
          title = "Data Science"
          ,icon = icon("microscope"),
          
          fluidRow(
            box(collapsible = TRUE,        

              selectizeInput(ns("year4"),
                             "Select A Year",
                             choices = "2019",
                             multiple = TRUE), width = 6)
          ),
          
          
          fluidRow(box(collapsible = TRUE,dataTableOutput(ns("data_science_summary")), width = 12)
                   
          ),
          fluidRow(box(collapsible = TRUE,title =  "Diverging Lollipop Chart, data science", plotlyOutput(ns("data_science_plot"), height = "600px", width = "100%"), width = 12))
                   
          )
        
        
        ,tabPanel(
          
          title = "Network Analysis"
          ,icon = icon("chart-network")
          
          ,box(collapsible = TRUE,        
            
            selectizeInput(ns("HighSkill4"),
                           "Select A Skill Catagory",
                           choices = "Please select",
                           multiple = TRUE), width = 12)
          
          ,p(strong("Please select a catagory from the dropdown above")),
          hr(), 
          h4(
            "The network diagram below shows the strength of relationship between reported skills, by catagory. The thicker to edge, the stronger the relationship. "
          ),
          
          
          fluidRow(box(collapsible = TRUE,visNetworkOutput(ns("skill_network"), height = "1200px", width = "100%"), width = 12))

          
        )
        
        
      )
      
    )
  )
  
  
  


}

drilldownModule <- function(input, output, session, values) {
  
  observeEvent(values$weHaveData, {
    
    if(values$weHaveData){
      
      SkillsDF=values$SkillsDF
      
      updateSelectizeInput(session, "skill", choices=c(unique(SkillsDF$Specific.Skill)), selected = unique(SkillsDF$Specific.Skill)[1])
      updateSelectizeInput(session, "HighSkill4", choices=c(unique(SkillsDF$High.Level.Cat)), selected = unique(SkillsDF$High.Level.Cat)[2])
      updateSelectizeInput(session, "year3", choices=c(unique(SkillsDF$Year)), selected = max(unique(SkillsDF$Year)))
      updateSelectizeInput(session, "year4", choices=c(unique(SkillsDF$Year)), selected = max(unique(SkillsDF$Year)))
      
      
    }
    
  })
  
  observeEvent(values$weHaveData, {
    
    if(values$weHaveData){
      
      SkillsDF=values$SkillsDF
      Skill1= values$Skill1 
      
      #------------------------------------------------------------------------------------------------------------------------

      output$skill_distribution <- renderPlotly({
        
        validate(
          need(input$skill, 'Select at least one skill'), 
          need(input$year3, 'Select at least one year')

        )
        
        summarySt <- SkillsDF %>%  
          dplyr:: filter(Specific.Skill == input$skill)
        
        Years <- unique(summarySt$Year)
        
        if(input$year3 %in% Years){
        
        summarySt <- summarySt %>%  
          dplyr:: filter(Year == input$year3) %>%
          dplyr:: select(Specific.Skill, Rank.Now.Num, Rank.Future.Num) %>%
          tidyr:: gather(key = Metric, value = Position, -Specific.Skill) %>%
          dplyr:: filter(Position != 0)
        
        g <- ggplot(summarySt, aes(Position))
        g + geom_density(aes(fill=factor(Metric)), alpha=0.5) + 
          labs(title="Density plot of Current & Future Scores", 
               subtitle="Scoes on 0-4 scale",
               caption="Source: mpg",
               x="User Scores",
               fill="State") + 
          theme_skillsMatrix(base_size = 12) + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) + 
          scale_fill_manual(values = c("Rank.Now.Num"="#A51140", "Rank.Future.Num"="#1E1E1E" ),
                            labels = c("Current Score", "Future Score"))
        
        } else{
          
          text = paste("No rows in DF")
          ggplot() + 
            annotate("text",size=8, label = text) + 
            theme_bw() +   theme(panel.grid.major=element_blank(),
                                 panel.grid.minor=element_blank())
        }
        
        
      })

      output$skill_distribution_now <- renderPlotly({
        
        validate(
          need(input$skill, 'Select at least one skill')
          
        )

        grouping=c("Specific.Skill", "Year", "Rank.Now")
        
        Total <- SkillsDF %>%
          dplyr:: filter(Specific.Skill == input$skill, Rank.Now.Num != 0) %>%
          dplyr:: group_by(.dots= c("Specific.Skill", "Year")) %>%
          dplyr:: tally() %>%
          dplyr:: rename("Total" = "n")
        
        # aggregate  
        summarySt <- SkillsDF %>%  
          dplyr:: filter(Specific.Skill == input$skill, Rank.Now.Num != 0) %>%
          tidyr:: drop_na() %>%
          dplyr:: group_by(.dots= c(grouping)) %>% 
          dplyr:: tally() %>%
          dplyr:: rename("Count" = "n") %>%
          tidyr:: spread(key = Rank.Now, value = Count) %>%
          dplyr:: ungroup() %>%
          dplyr:: mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>%
          dplyr:: inner_join(Total, by = grouping[-length(grouping)])
        
        
        if(nrow(summarySt) >0){

          labels = c("Basic (1)", "Good (2)", "High (3)","Expert (4)")
          
          DF <- summarySt %>%  
            dplyr:: select(Specific.Skill, Year,
                           contains("Basic (1)"), contains("Good (2)"), contains("High (3)"), contains("Expert (4)" )) %>%
            tidyr:: gather(key = Score, value = Count, -Specific.Skill, -Year) %>%
            dplyr:: mutate(rank = rank(Count, ties.method = 'first')) %>%
            dplyr:: mutate(Score = factor(Score, levels = labels)) 

          p<- 
            ggplot(data = DF, aes(x =  reorder(Score, Count), y = Count), group = Year) +
            geom_bar(aes(fill= factor(Year)), width = 0.5, stat = "identity", position = "dodge") +
            labs(title="Count of Current scores", x = "") +
            ggplot2::scale_fill_manual(values = unname(boe_cols, length(unique(df$Score)))) + 
            coord_flip() +
            theme_skillsMatrix(base_size = 12) + 
            theme(panel.grid.major=element_blank()) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) 

          ggplotly(p)

          
        } else{
          
          text = paste("No rows in DF")
          ggplot() + 
            annotate("text",size=8, label = text) + 
            theme_bw() +   theme(panel.grid.major=element_blank(),
                                 panel.grid.minor=element_blank())
        }
        
        
  
      })
      
      output$skill_distribution_future <- renderPlotly({
        
        validate(
          need(input$year3, 'Select at least one year'),
          need(input$skill, 'Select at least one skill')
          
        )

        grouping=c("Specific.Skill", "Year", "Rank.Future")
        
        Total <- SkillsDF %>%
          dplyr:: filter(Specific.Skill == input$skill, Rank.Future.Num != 0) %>%
          dplyr:: group_by(.dots= c("Specific.Skill", "Year")) %>%
          dplyr:: tally() %>%
          dplyr:: rename("Total" = "n")
        
        # aggregate  
        summarySt <- SkillsDF %>%  
          dplyr:: filter(Specific.Skill == input$skill, Rank.Future.Num != 0) %>%
          tidyr:: drop_na() %>%
          group_by(.dots= c(grouping)) %>% 
          dplyr:: tally() %>%
          dplyr:: rename("Count" = "n") %>%
          tidyr:: spread(key = Rank.Future, value = Count) %>%
          dplyr:: ungroup() %>%
          dplyr:: mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>%
          dplyr:: inner_join(Total, by = grouping[-length(grouping)])
        
        
        if(nrow(summarySt) >0){
 
          labels = c("Basic (1)", "Good (2)", "High (3)","Expert (4)")
          
          DF <- summarySt %>%  
            dplyr:: select(Specific.Skill, Year,
                           contains("Basic (1)"), contains("Good (2)"), contains("High (3)"), contains("Expert (4)" )) %>%
            tidyr:: gather(key = Score, value = Count, -Specific.Skill, -Year) %>%
            dplyr:: mutate(rank = rank(Count, ties.method = 'first')) %>%
            dplyr:: mutate(Score = factor(Score, levels = labels)) 
          
          p<- 
            ggplot(data = DF, aes(x =  reorder(Score, Count), y = Count), group = Year) +
            geom_bar(aes(fill= factor(Year)), width = 0.5, stat = "identity", position = "dodge") +
            labs(title="Count of Future state scores", x = "") +
            ggplot2::scale_fill_manual(values = unname(boe_cols, length(unique(df$Score)))) + 
            coord_flip() +
            theme_skillsMatrix(base_size = 12) + 
            theme(panel.grid.major=element_blank()) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) 
          
          ggplotly(p)

        } else{
          
          text = paste("No rows in DF")
          ggplot() + 
            annotate("text",size=8, label = text) + 
            theme_bw() +   theme(panel.grid.major=element_blank(),
                                 panel.grid.minor=element_blank())
        }
        
        
      })
      
      output$skill_users <- renderDataTable({
        
        validate(
          need(input$skill, 'Select at least one skill ...'), 
          need("Name" %in% colnames(SkillsDF), 'No personal information available in the loaded dataset ...')
        )

        summarySt <- dplyr:: filter(SkillsDF, Rank.Now.Num != 0)
        summarySt <-  dplyr:: filter(summarySt, Rank.Future.Num != 0)
        
        if(nrow(summarySt) > 0 & values$weHaveData & "Name" %in% colnames(SkillsDF)){

          summarySt <- summarySt %>%   
            dplyr:: filter(Specific.Skill == input$skill) %>%
            dplyr:: group_by(.dots = c("High.Level.Cat", "Specific.Skill", "Year")) %>% 
            dplyr:: arrange(desc(Rank.Now.Num)) %>% 
            dplyr:: select(-ID, -Rank.Now.Num, -Rank.Future.Num)

        } else{
          
          summarySt <- data.table:: as.data.table("No data to show in table")
          
        }
        
        create_data_table(summarySt) 
          
        
      })
      
      output$skill_summary <- renderDataTable({
        
        validate(
          need(input$skill, 'Select at least one skill ...')
        )
        
        
        summarySt <- dplyr:: filter(SkillsDF, Rank.Now.Num != 0)
        summarySt <-  dplyr:: filter(summarySt, Rank.Future.Num != 0)

        if(nrow(summarySt) > 0 & values$weHaveData){
          
          
          summarySt <- summarySt %>%   
            dplyr:: filter(Specific.Skill == input$skill) %>%
            dplyr:: group_by(.dots = c("High.Level.Cat", "Specific.Skill", "Year")) %>% 
            dplyr::summarise(
              Count = sum(Rank.Future.Num > 0,na.rm = T), 
              meanGap = round(mean(Gap, na.rm = T), 2),
              `AbsGap %` = round(sum(Gap < 0, na.rm = T)/sum(Rank.Future.Num > 0,na.rm = T) *100),
              Mean_Now = round(mean(Rank.Now.Num, na.rm = T),2),
              Mean_Future = round(mean(Rank.Future.Num, na.rm = T),2)) %>%
            tidyr:: gather(key = Metric, value = Position, -Specific.Skill, -High.Level.Cat, -Year) %>%
            tidyr:: spread(key = Year, value = Position)
          
          if(c("High.Level.Cat") %in% colnames(summarySt)){
            
            validate(need(nrow(summarySt) >0, 'please load some data ...'))
            
            cols <- colnames(summarySt[ , purrr::map_lgl(summarySt, is.numeric)])

            create_data_table(summarySt) %>%
              
              formatStyle(cols,
                          background = styleColorBar(c(0,40), 'lightblue'),
                          backgroundSize = '98% 60%',
                          backgroundRepeat = 'no-repeat',
                          backgroundPosition = 'left') 
            
          }

        } else{
          
          data.table:: as.data.table("No data to show in table")
          
        }
        
      })
      
      output$skill_network <- renderVisNetwork({
        
        validate(
          need(input$HighSkill4, 'Select at least one year')
        )
        
        plot <- generate_network_plot(mytable = SkillsDF, SelectYear = FALSE, SelectHighCat = input$HighSkill4, filter0Now = TRUE)
        plot

      })
      
      output$data_science_summary <- renderDataTable({
        
        validate(
          need(input$year4, 'Select a a year ...')
          
        )
        
        DSSkills <- 
          c("Programming tools for analysis (R)", 
            "Tableau",
            "Programming tools for product development (R, Shiny)", 
            "Database query tools (SQL)",
            "Programming tools for big data (e.g. Spark)",
            "Version control (git, TFS)",                                                   
            "Programming notebooks (R markdown)",
            "Workflow-based tools ( KNIME, Alteryx)",
            "Data science",
            "Mathematics",
            "Data Engineering", 
            "Machine Learning", 
            "Computer science", 
            "Statistical methods")
        
        
        DT<- makeGapDT(mytable = SkillsDF, grouping = c("High.Level.Cat", "Specific.Skill", "Year")) %>%
          dplyr:: filter(Year %in% input$year4, Specific.Skill %in% DSSkills) %>%
          dplyr:: arrange(-desc(meanGap))

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

      output$data_science_plot<- renderPlotly({
        
        validate(
          need(input$year4, 'Select a a year ...')
          
        )
        
        DSSkills <- 
          c("Programming tools for analysis (R)", 
            "Tableau",
            "Programming tools for product development (R, Shiny)", 
            "Database query tools (SQL)",
            "Programming tools for big data (e.g. Spark)",
            "Version control (git, TFS)",                                                   
            "Programming notebooks (R markdown)",
            "Workflow-based tools ( KNIME, Alteryx)",
            "Data science",
            "Mathematics",
            "Data Engineering", 
            "Machine Learning", 
            "Computer science", 
            "Statistical methods")
        
        DT<- dplyr:: filter(SkillsDF, Year %in% input$year4, Specific.Skill %in% DSSkills) 
        DT<- makeGapDT(mytable = DT, grouping = c("High.Level.Cat", "Specific.Skill")) %>%
             dplyr:: arrange(-desc(meanGap))
        
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