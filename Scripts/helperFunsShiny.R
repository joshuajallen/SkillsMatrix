### ------ Functions to clean and summarise data
#

#' @title BOE colour pallete used for charting  

boe_cols <- c(
  red            = "#A51140",
  black          = "#1E1E1E",
  stone          = "#CAC0B6",
  dark_teal      = "#005E6E",
  dark_blue      = "#002A42",
  plum           = "#752864",
  mid_blue       = "#165788",
  maroon         = "#6C0721",
  purple         = "#4E3780",
  grey           = "#999999",
  green          = "#006663", 
  orange         = "#D55E00", 
  orange2        = "#E69F00", 
  blue           = "#56B4E9")


create_formatted_table <- function(table_input){
  
  df = as.data.frame(table_input)
  
  if(nrow(df) > 0){
    
    datatable(df,
      extensions = c("Buttons", "Scroller"),filter = "bottom",
              
      options = list(
        # Table components can be added/removed here
        dom = "Blfrtip",
        
        # Freezing panes
        fixedHeader = TRUE,
        
        # Specify buttons on top of table
        buttons = c("copy", "excel", I("colvis")),
      
        
        # Page length and menu
        paging = TRUE,
        pageLength = 50,
        lengthMenu = list(
          c(15, 25, 50, -1),
          list("15", "25", "50", "All")
        )
      )
    )


  }
  else{
    "No data to show in table"
  }
}

create_data_table <- function(table_input) {
  
  df = as.data.frame(table_input)
  
  if(nrow(df) > 0){
    
    datatable(df, filter = "bottom",
              
              options = list(
                # Table components can be added/removed here
                dom = "Blfrtip",
                
                # Freezing panes
                fixedHeader = TRUE,
                
                # Specify buttons on top of table
                buttons = c("copy", "excel", I("colvis")),
                
                
                # Page length and menu
                paging = FALSE

              )
    )
    
    
  }
  else{
    DT<- as.data.frame("No data to show in table")
    colnames(DT)[1] <- ""
    DT
  }
}

create_formattable <- function(table_input){
  
   df = table_input

    as.datatable(df,
              extensions = c("Buttons", "Scroller"),filter = "bottom",
              
              # options = list(
              #   # Table components can be added/removed here
              #   dom = 'Bfrtip',
              #   buttons = c('copy', 'excel'),
              #   # Scroll heights
              #   
              #   # Page length and menu
              #   #paging = TRUE,
              #   pageLength = 15,
              #   lengthMenu = c(5, 15, 25, 50, 100), scrollX = TRUE))
    
    options = list(
      # Table components can be added/removed here
      dom = "Blfrtip",
      
      # Freezing panes
      fixedHeader = TRUE,
      
      # Specify buttons on top of table
      buttons = c("copy", "excel", I("colvis")),

      
      # Page length and menu
      paging = TRUE,
      pageLength = 50,
      lengthMenu = list(
        c(15, 25, 50, -1),
        list("15", "25", "50", "All")
      )
    )
    )
    
 
}

# this function gets data from processed folder or process data
getData<-function(
  folderRawData="N:/DATA/Cross Divisional Work/SkillsMatrix",
  folderProcessedData="N:/DATA/Cross Divisional Work/SkillsMatrix/ProcessedData/",
  loadProcessedData=FALSE,
  processedDataDate="2019-07-09", 
  Year = "2019"){
  
  require(readxl)
  require(dplyr)
  
  if(!loadProcessedData){  
    
    # load raw data
    file.list <- list.files(path = folderRawData, pattern='*.xlsx|*.XLSX',recursive = TRUE,full.names = TRUE)
    df.list <- lapply(file.list, read_excel, sheet=2,col_types = "text")
    
    ## add name of folder
    attr(df.list, "names") <- file.list
    names(df.list) <- file.list
    df.list <-
      mapply(`[<-`, df.list, 'TeamFolder', value = names(df.list), SIMPLIFY = FALSE)
    
    
    ## add ramdom identifier
    names(df.list) <- 1:length(file.list)
    rawData <- dplyr:: bind_rows(df.list, .id = "id")[,1:10]
    
    
    ## clean names
    colnames(rawData) = c("ID","Scale","Role", "Team", "High.Level.Cat","Specific.Skill","Rank.Now","Rank.Future","Additional.information","TeamFolder")
    
    
    ## clean scales
    rawData<- dplyr:: filter(rawData, !is.na(Scale))
    ## clean groups
    #rawData<- rawData %>% dplyr:: mutate(Team=unlist(lapply(strsplit(TeamFolder, split = "/"),'[[',6)))
    
    
    ## add new groups
    #Team=c("RDG", "MCG", "FSG", "DST", "BDCG", "DCT","DMT","OCT_DSDCentral_CA")
    
    
    ## make the values numeric
    labels<-c("Not applicable (0)"=0, "Basic (1)"=1 , "Good (2)"=2, "High (3)"=3 , "Expert (4)"=4)    
    rawData<- rawData %>% 
      dplyr:: mutate(Rank.Now.Num = labels[Rank.Now],
                     Rank.Future.Num = labels[Rank.Future])
    
    
    
    
    ## add extra variables
    rawData<- rawData %>% 
      dplyr:: mutate(Gap=Rank.Now.Num-Rank.Future.Num) %>%
      dplyr:: mutate(Year = Year)
    
    # save them in the N drive 
    processedData=rawData
    save(processedData, file=paste0(folderProcessedData,"ProcessedData_", Sys.Date(),".Rda"))
    
  } else{
    
    # load processed data
    load(paste0("N:/DATA/Cross Divisional Work/SkillsMatrix/ProcessedData/ProcessedData_",processedDataDate,".Rda"))
  }  
  
  processedData
  
}


CleanSkillsData <- function(DF){
  
  cols <- c("ID","Scale","High.Level.Cat","Specific.Skill","Rank.Now","Rank.Future","Team","Rank.Now.Num","Rank.Future.Num","Gap","Year")
  
  ColumnTest <- all(cols %in% colnames(DF))
  
  # if(ColumnTest == TRUE){
  #   
  #   YearTest <- nrow(unique(DF$Year)) > 1
  #   
  # } else{
  #   
  #   YearTest = "Column test failed, please review processed data "
  #   
  # }
  
  if(ColumnTest == TRUE){
    
    CleanDT <- DF %>%
      dplyr:: mutate(Specific.Skill = recode(Specific.Skill, 
                                             "Visualisation tools (Excel, tableau, power BI, SSRS)" =  "Tableau", 
                                             "Database query tools (SQL type tools , NoSQL, MDx, SSAS)" = "Database query tools (SQL)", 
                                             "Bank collaboration tools: Sharepoint, Collaborate, My Service"= "My Service", 
                                             "Project Management (scoping, task setting, development follow-up, budget)" = "Project Management", 
                                             "Data Preparation (e.g. Virtual, Imputation, Anonimization)" = "Data Preparation", 
                                             "Structured thinking (e.g. business process opt, taxonomy)" = "Structured thinking", 
                                             "Programming tools for analysis (R, Python, Matlab)" = "Programming tools for analysis (R)", 
                                             "Programming tools for product development (R, Python, Matlab, Shiny)" = "Programming tools for product development (R, Shiny)", 
                                             "Programming notebooks (Jupyter notebooks, R markdown)" = "Programming notebooks (R markdown)", 
                                             "Version control (git)" = "Version control (git, TFS)", 
                                             "Office tools: \nPowerpoint\nAccess\nWord" = "Office tools (microsoft)", 
                                             "Office tools: \r\nPowerpoint\r\nAccess\r\nWord" = "Office tools (microsoft)", 
                                             "Coaching" = "Coaching and Mentoring",
                                             "Mentoring" = "Coaching and Mentoring",
                                             "RWM" = "CRM/RWM", 
                                             "PRA supervisory process: insurance" = "PRA supervisory process: Insurance")) %>%
      dplyr:: mutate(High.Level.Cat = if_else(Specific.Skill %in% c("Data Analysis: Exploration", 
                                                                    "Data Analysis: Modelling", 
                                                                    "Data Ownership & Stewardship (advisory)", 
                                                                    "Data Preparation", 
                                                                    "Data Preparation (e.g. Manipulation, Aggregation)", 
                                                                    "Data Publishing/Sharing", 
                                                                    "Data Quality (Plausie)"), "Data Analysis and Preperation", High.Level.Cat)) %>%
      dplyr:: mutate(Scale = recode(Scale, "Industrial placement" = "Scale IP/JP/J/K")) %>%
      dplyr:: mutate(Scale = recode(Scale, "Scale J/JP/K" = "Scale IP/JP/J/K")) %>%
      dplyr:: mutate(Scale = recode(Scale, "Scale J" = "Scale IP/JP/J/K")) %>%
      dplyr:: mutate(Scale = recode(Scale, "Scale IP/JP/K" = "Scale IP/JP/J/K")) %>%
      dplyr:: mutate(Team = recode(Team, "OCT_DSDCentral_CA" = "DSDCentral")) %>%
      dplyr:: mutate(High.Level.Cat = recode(High.Level.Cat, "Analytical" = "Analytical tools")) %>%
      dplyr:: select(cols) %>%
      dplyr:: mutate(Year = as.character(Year), 
                     ID = as.character(ID))
    
  } else{
    
    CleanDT = NULL
  }
  
  
  return( CleanDT)
  
  
}


# this function sumarises the data before plotting
makeDataPlot <-  function(mytable,grouping,filter0Now=FALSE, filter0Future=FALSE){
  
  # if needed filter out 0    
  if (filter0Now) filter(mytable, Rank.Future.Num>0)
  if (filter0Future) filter(mytable, Rank.Future.Future>0)
  
  # aggregate    
  summarySt <- mytable %>%
    group_by(.dots=grouping) %>%
    mutate(nPeople = n_distinct(ID)) %>% 
    ungroup()
  
  summarySt <- summarySt %>%       
    mutate(CreditSkillNum=ifelse(CreditSkill, Gap,NA),
           DebitSkillNum=ifelse(DebitSkill, Gap,NA)) %>%    
    group_by(.dots = c(   grouping, "nPeople")) %>% 
    dplyr::summarise(
      meanGap = round(mean(Gap, na.rm = T), 2),
      #meanGap2 = round(mean(Gap2, na.rm = T), 2), 
      meanGapC = round(mean(CreditSkillNum, na.rm = T), 2),
      meanGapD = round(mean(DebitSkillNum, na.rm = T), 2),
      AbsGap = round(sum(Gap < 0, na.rm = T)/sum(Rank.Future.Num > 0,na.rm = T) *100),
      Mean_Future = round(mean(Rank.Future.Num, na.rm = T),2))
}

makeDataPlot_count <-  function(mytable,grouping,filter0Now=FALSE, filter0Future=FALSE){
  
  # if needed filter out 0    
  if (filter0Now) filter(mytable, Rank.Future.Num>0)
  if (filter0Future) filter(mytable, Rank.Future.Future>0)
  
  # aggregate    
  summarySt <- mytable %>%
    group_by(.dots=grouping) %>%
    mutate(nDim = n()) %>% 
    ungroup()
  
  summarySt <- summarySt %>%          
    group_by(.dots = c(   grouping, "nDim")) %>% 
    dplyr::summarise(
      Total = sum(Rank.Now.Num > 0, na.rm = T),
      Average_Score = round(mean(Rank.Now.Num, na.rm = T),2))
}


########################## This scrips contains functions to help the analysis----------------
# Authors: JA

#################################################################################

# function to help load attendees


colors <- c("#E41A1C99",
            "#377EB899",
            "#4DAF4A99",
            "#984EA399",
            "#FF7F0099")

colors <- c("#CF395C",
            "#003366",
            "#009973",
            "#984EA399",
            "#d6d6c2")

cbPalette <- colorRampPalette(colors)(20)


# Charts
theme_skillsMatrix <- function(base_size = 16)
{
  theme_classic( base_size = base_size) +
    theme(
      axis.line = element_blank(),
      axis.title.x = element_text(
        angle = 0,
        size = base_size,
        colour = "black"
      ),
      axis.text.x = element_text(
        size = base_size *  1,
        hjust = 0,
        colour = "grey50"
      ),
      axis.text.y = element_text(
        size = base_size *  1,
        hjust = 0,
        colour = "grey50"
      ),
      axis.title.y = element_text(
        angle = 90,
        size = base_size,
        colour = "black"
      ),
      axis.title = element_text(
        size = base_size,
        colour = "black"
      ),
      legend.title = element_blank(), legend.text = element_text(size = base_size),
      panel.grid.major.y = ggplot2::element_line(colour = "grey"),
      axis.line.x = ggplot2::element_line(colour = "grey"),
      axis.line.y = ggplot2::element_line(colour = "grey"),
      axis.ticks.x = ggplot2::element_line(colour = "grey"),
      axis.ticks.y = ggplot2::element_line(colour = "grey"),
      plot.caption = ggplot2::element_text(colour = "grey50", hjust = 1),
      panel.background = element_rect(fill = "white", colour = "grey50"),
      panel.grid.minor.y = element_blank(), legend.position = "bottom", 
      panel.grid.major = element_line(colour = "grey50"), 
      panel.grid.minor = element_line(colour = "grey50", size = 0.25), 
      plot.title = ggplot2::element_text(size = base_size, hjust = 0, colour = "#002A42"), 
      plot.subtitle = ggplot2::element_text(size = base_size*0.9, colour = "grey"))
}




# this function provides the conditions for the sweetalert
dataAlert <- function(text, title, type) {
  sendSweetAlert(
  session = session,
   #messageId = messageId,
    title = title,
    text = text,
    type = type
  )
}



# this function gives the info to the users
makeInfo <- function(text = "") {
  dropdownButton(
    size = "xs",
    tags$h5(text),
    circle = TRUE,
    status = "danger",
    width = "50px",
    icon = icon("question"),
    right = F
  )
}

Cols_AllMissing <- function(df){ # helper function
  as.vector(which(colSums(is.na(df)) == nrow(df)))
}


# this function gives the info to the users
makeNotice <- function(text = "") {
  dropdownButton(
    label=" Privacy notice",
    size = "xs",
   # style="minimal",
    tags$h5(text),
    circle = F,
    status = "danger",
    width = "50px",
    icon = icon("bars"),
    right = F
  )
}




textDataModule <- p(
  h5(
    "
    This App will allow you to gain insights to the skills and business needs of the division/business area, such as;
    "
  ),
  h5(tags$ul(
    tags$li("How many people have Tableau skills, and at what level? "),
    tags$li("What the biggest skill gaps across DSD, by scale or by business area? "),
    tags$li("How have skill levels, needs and expectations changed on a temporal basis? ")
  )),
  
  h5("
     To start please follow the instructions below."),
  h5(tags$b("Loading the skills matrix from a .csv file")),
  h5(
    tags$ol(
      tags$li(
        "Firstly, ensure you have a .csv file stored locally, that has been cleansed and is appropriate for use within this app." ,
        "Before uploading the CSV please ensure that any private information that you do not want analysed has been removed from the data;"
      ),
      tags$li(
        "To upload your Skills Data, click the 'Load Skills Matrix data' button and select the CSV file you want to upload to the tool;
        "
      ),
      tags$li("Ensure that the notification pops up, stating that the data has been loading successfully.
              "))),

  h5(tags$b("To Load data from source file")),
  h5(tags$b("WARNING: DSD only")),
  
  h5(tags$ol(
      tags$li("To upload the Skills Data from a processed data file, click the 'Load processed data file' button and wait for the data to load;
              "),
      tags$li(
          "'Ensure that the notification pops up, stating that the data has been loading successfully
          "
        )
      )
     ),
  
  h5("The two tables at the bottom show the respondent population statistics, broken down by team and scale for the reporting periods in the sample.")
  
)


textStockModule <- p(
  h5("This tab allows you to explore the skill distribution of the business area, primarily looking at the count of individuals and their current/future state skill level. Please follow the instructions below:"),
  h5(tags$ol(
    tags$li("Select the Required inputs on the left-hand side. The options available are high level skill category (upper level of skills hierarchy, e.g. soft or analytical skills), of which you can choose multiple categories if desired. You will also need to select a year, the latest year is selected as default. 
 "),
    tags$li("Select the Optional inputs on the right-hand side. The options available are scale and team, choices will feed into the table/chart below. Multiple teams, scales or combination of the two can be selected;
 "),
    tags$li("The chart/table shows the count of individuals, by the grouping selected in the required/optional inputs
 "),
    tags$li("The data can be exported (copy to clipboard or to Excel) using the buttons provided in the table")
    
  )),
  
  h5("The now vs future table shows a quick cross comparison of counts for current/expected skill, by the user defined grouping"),
  h5(tags$ol(
    tags$li("Select the Required inputs on the left-hand side. The options available are high level skill category (upper level of skills hierarchy, e.g. soft or analytical skills), of which you can choose multiple categories if desired."),
    tags$li("Select the Optional inputs on the right-hand side. The options available are scale and tam choices will feed into the table/chart below. Multiple teams, scales or combination of the two can be selected;"),
    tags$li("The chart/table shows the count of individuals, by the grouping selected in the required/optional inputs"),
    tags$li("The data can be exported (copy to clipboard or to Excel) using the buttons provided in the table")
  ))
      )

textGapModule <- p(
  h5(
    "This tab shows the results from the skills gap analysis, which calculated the difference between the current and future scores for a given user defined grouping.
     The metrics in the tables can be interpreted as follows:
    "
  ),
  h5(tags$ul(
    tags$li("Count - number of people in the population selected"),
    tags$li("Mean gap - is the average difference between the current and future scores across the selected population, a negative gap means that the current score is less than the future score"),
    tags$li("Abs gap (%) - is the % of people with a gap, of the population shown in the 'Count' column?"),
    tags$li("Mean future - is the average future score, indicating the level of the skill catagory")
    
  )),
  
  
  h5("This tab allows you to explore the skill gaps of the business area by looking at the difference in current/future state skill level. Please follow the instructions below:"),
  h5(tags$ol(
    tags$li("Select the Required inputs on the left-hand side. The options available are high level skill category (upper level of skills hierarchy, e.g. soft or analytical skills), of which you can choose multiple categories if desired. You will also need to select a year, the latest year is selected as default. 
            "),
    tags$li("Select the Optional inputs on the right-hand side. The options available are scale and team, choices will feed into the table/chart below. Multiple teams, scales or combination of the two can be selected;
            "),
    tags$li("The chart/table shows the count of individuals, by the grouping selected in the required/optional inputs
            "),
    tags$li("The data can be exported (copy to clipboard or to Excel) using the buttons provided in the table")
    
    ))
)



textTemporalModule <- p(
  h5(
    "
    This tab shows a temporal summary of the results collected over the sample years and aims to answer the following questions;
    "
  ),
  h5(tags$ul(
    tags$li("How have the average current state scores evolved?"),
    tags$li("How has the future state changed over the sample periods?"),
    tags$li("What skill gaps have narrowed, and which have widened?")
  )),
  
  h5("This tab allows you to explore the skill evolution over time, including current and future state scores. Please follow the instructions below;"),
  h5(tags$ol(
    tags$li("Select the Required inputs on the left-hand side. The options available are high level skill category (upper level of skills hierarchy, e.g. soft or analytical skills), of which you can choose multiple categories if desired.
            "),
    tags$li("Select the Optional inputs on the right-hand side. The options available are scale and team, choices will feed into the table/chart below. Multiple teams, scales or combination of the two can be selected;
            "),
    tags$li("The chart/table shows the count of individuals, by the grouping selected in the required/optional inputs
            "),
    tags$li("The data can be exported (copy to clipboard or to Excel) using the buttons provided in the table")
    
  ))
  

      
  )
  

textDrilldownModule <- p(
  h5(
    "
    This tab allows for a more detailed analysis of skills Please follow the instructions below:"),
 
  h5(tags$b("Skill Level Analysis")),
  
  h5(tags$ol(
    tags$li("Select the Required inputs on the left-hand side. The options available are skill (lower level of skills hierarchy, e.g. Excel), you will also need to select a year. Note that the latest year is selected as default. 
            ")
  )
  ),
  
  
  h5(tags$ul(
    tags$li("The table shows the summary metrics for the chosen skill, such as the average current and future scores"),
    tags$li("The density curve shows the distribution of current and future state score for the chosen skill/year"),
    tags$li("The bar charts show the reported counts of the chosen skill at each level")
  )),
  
  h5(tags$b("Data Science")),
  
  h5(tags$ol(
    tags$li("Select the Required inputs on the left-hand side. The options available are year and the latest year is selected as default. 
            ")
    )
  ),
  
  
  h5(tags$ul(
    tags$li("The table shows the summary metrics for the chosen skill, such as the average current scores, future scores and skill gaps"),
    tags$li("The diverging lollipop chart shows the reported skill gaps")
  )),
  
  h5(tags$b("Network Analysis")),
  
  h5(tags$ol(
    tags$li("Select the Required inputs on the left-hand side. The options available are skill category (higher level of skills hierarchy, e.g. Systems) 
            ")
  )
  ),
  
  
  h5(tags$ul(
    tags$li("The network diagram below shows the strength of relationship between reported skills, by catagory. The thicker to edge, the stronger the relationship.")  ))

  )



textPrivacyNortice <- p(
  h5("This app uses two sources of data:"),
  h5(tags$ol(
    tags$li("The skills matrix, as completed by members of the business area/division;") ,
    tags$li("The list of required/desirable skills, as defined by the relevant area"))),

  h5("Note the following points regarding using and storage of data:"),
  
  h5(tags$ul(
    tags$li("The skills matrix is only  displayed for the duration of this session. After you log-off  from the Shiny Server no data regarding the skills is kept or stored."),
    tags$li("The data used by the app contains data since collection (2018 for DSD) up to the current year."))),
    
    h5("To read the privacy notice in full please click", a(" here.",     href="http://intranet/Banknav/IML.asp?svr=BOE-DMS&db=Analytical&id=8030975&v=0") )
  
  )

textSettingsModule<-p(
  h5(
    "
    The Skills Matrix: A way of understanding the skills composition of the business and managing future needs.
    "
  ),

  hr(),

  h5(tags$b("Background")),
  
  h5(
    "The Data and Statistics division (DSD) embarked  on a divisional effort to redesign the business area, ranging from our culture, our business processes to management skills. 
     The Capabilities group focussed their work in three areas:  skills, recruitment and retention (career paths).  The app that you see today is the result of this work stream, supporting our management of capability needs across the division. 
     We will develop our skills by combining breadth of knowledge with deep expertise in one or more areas to enable our adaptability and agility. Our learning culture, and focus on realising our potential, will also enable us to grow, personally and organisationally, fuelled by ideas and innovation, and learning and experimentation. 
") ,

  h5("We had a few things going for us that made this possible: "),

  h5(tags$ol(
    tags$li("We have moved to a flexible project pool approach in DSD, which allows people to work on ideas they think might be interesting;"),
    tags$li("we started a partnership with the Delivery team in Technology that aimed to explore the ways we work and we have been learning together;"),
    tags$li("The Data Programme has delivered a Shiny Server that makes it possible to build and share apps easily in a self-service way. We have also RADaR, which is
            a single tool for easier discovery of analytical reports and dashboard from across the Bank;"),
    tags$li("We had vision 2020 encouraging us to do this type of reflection on better decision making and to experiment under the 'fast fail' prerogative.")
  )),
  h5("Please note that the purpose of this app is to be used by business areas for to explore their skills data in an accessible and intuitive manor, and it does not come with the sort of tech warranty or support you might expect from something Technology have built. Hopefully you will find it useful and help us to expand it with your ideas and feedback.
"),
  hr(),
  
  h5(tags$b("Useful links and contacts")),
  h5(tags$ul(
    tags$li("For general ideas and feedback, please email Josh or Jenny;"),
    tags$li("To find out more about Shiny, have a look at this introduction from", a(" RStudio.",     href="https://shiny.rstudio.com/")  ,"For more information on how to publish your apps to the Shiny server so that you can share them with your colleagues, see the", a("Shiny collaborate page;",     href="http://collaborate/workspaces/EPiC/AnalyticalToolsandVisualisationZone/default.aspx")),
    tags$li("To explore the RADaR reports and dashboards go to ", a(" the RADAR website;",     href="https://radar.boe.bankofengland.co.uk/")), 
    tags$li("To find out more about the Data Programme, see a recent ", a("presentation to EDCo",     href="http://intranet/Banknav/IML.asp?svr=BOE-DMS&db=Analytical&id=7735516&v=0"),", or contact your local data board reps.")

  ))
  )


#--------------------------------------------------------------------------------------------------------------

makeTemporalDT <-  function(mytable,grouping,filter0Now=FALSE, filter0Future=FALSE){
  
  # if needed filter out 0    
  if (filter0Now) filter(mytable, Rank.Future.Num>0)
  if (filter0Future) filter(mytable, Rank.Future.Future>0)
  
  # aggregate    
  summarySt <- mytable %>%
    group_by(.dots=grouping) %>%
    mutate(nPeople = n_distinct(ID)) %>% 
    ungroup()
  
  summarySt <- summarySt %>%       
    group_by(.dots = c(grouping)) %>% 
    dplyr::summarise(
      meanGap = round(mean(Gap, na.rm = T), 2),
      Mean_Future = round(mean(Rank.Future.Num, na.rm = T),2), 
      Mean_Now = round(mean(Rank.Now.Num, na.rm = T),2))
  
  Years <- unique(summarySt$Year)
  
  if(length(Years) > 1){
    
    return(summarySt)}
  
  else{
    summarySt<- NULL
    
    return(summarySt)
    
  }
  
}

geom_Dumbbell <- function (mapping = NULL, data = NULL, ..., colour_x = NULL, 
                           size_x = NULL, colour_xend = NULL, size_xend = NULL, dot_guide = FALSE, 
                           dot_guide_size = NULL, dot_guide_colour = NULL, na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = "identity", 
        geom = GeomDumbbell, position = "identity", show.legend = show.legend, 
        inherit.aes = inherit.aes)
}

GeomDumbbell <- ggproto("GeomDumbbell", Geom,
                        required_aes = c("x", "xend", "y"),
                        non_missing_aes = c("size", "shape",
                                            "point.colour.l", "point.size.l",
                                            "point.colour.r", "point.size.r"),
                        default_aes = aes(
                          shape = 19, colour = "black", size = 0.5, fill = NA,
                          alpha = NA, stroke = 0.5
                        ),
                        
                        setup_data = function(data, params) {
                          transform(data, yend = y)
                        },
                        
                        draw_group = function(data, panel_scales, coord,
                                              point.colour.l = NULL, point.size.l = NULL,
                                              point.colour.r = NULL, point.size.r = NULL) {
                          
                          points.l <- data
                          points.l$colour <- point.colour.l %||% data$colour
                          points.l$size <- point.size.l %||% (data$size * 2.5)
                          
                          points.r <- data
                          points.r$x <- points.r$xend
                          points.r$colour <- point.colour.r %||% data$colour
                          points.r$size <- point.size.r %||% (data$size * 2.5)
                          
                          grid::gList(
                            ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
                            ggplot2::GeomPoint$draw_panel(points.l, panel_scales, coord),
                            ggplot2::GeomPoint$draw_panel(points.r, panel_scales, coord)
                          )
                          
                        },
                        
                        draw_key = draw_key_point
)

makeDumbellPlot <- function(DT, df2, year1, year2, grouping){
  
  year1<- rlang:: sym(year1)
  year2<- rlang:: sym(year2)
  grouping<- rlang:: sym(grouping)
  
  DT <- DT %>% tidyr:: drop_na()
  
  p<- ggplot(DT, aes(y = !! grouping)) + 
    geom_point(data = df2, aes(x = value, color = group), size = 10) +
    geom_Dumbbell(aes(x = !! year2, xend = !! year1), size=3.5, 
                  color= unname(boe_cols[3]), 
                  colour_x=unname(boe_cols[1]), 
                  colour_xend = unname(boe_cols[2]),
                  dot_guide=TRUE, 
                  dot_guide_size=0.25, 
                  size_x= 10,
                  size_xend = 10) +
  labs(x="Average Score", y= grouping, 
       title="Dumbbell Chart", 
       subtitle=paste("Average score:",year2, " vs ",year1)) +
    theme_skillsMatrix(base_size = 18) + 
    theme(plot.title = element_text(hjust=0.5, face="bold"),
          plot.background=element_rect(fill="#f7f7f7"),
          panel.background=element_rect(fill="#f7f7f7"),
          panel.grid.minor=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.major.x=element_line(),
          axis.ticks=element_blank(),
          legend.position="top",
          panel.border=element_blank()) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_color_manual(name = "", values = c(unname(boe_cols[1]), unname(boe_cols[2])))#+ 
  
  
  return(p)
  
}

makeSlopeChart <- function(DT, year1, year2, group)
{
  
  left_label <- paste(as.matrix(dplyr:: select(DT, !! group)), as.matrix(dplyr:: select(DT, !! year2)),sep=", ")
  right_label <- paste(as.matrix(dplyr:: select(DT, !! group)), as.matrix(dplyr:: select(DT, !! year1)),sep=", ")
  
  # class <- rlang:: sym(class)
  group <- rlang:: sym(group) 
  year1 <- rlang:: sym(year1)
  year2 <- rlang:: sym(year2)
  DT <- dplyr:: mutate(DT, class = ifelse((!! year1 - !! year2) < 0, "green", "red")) %>% dplyr:: ungroup() %>%  tidyr:: drop_na()

  
  #color = ifelse(as.matrix(dplyr:: select(DT, class)) == "green", unname(boe_cols["green"]), unname(boe_cols["red"]))) +
  p<-  
    ggplot(DT) + 
    
    geom_segment(data = DT, aes(x=0.25, xend=1.75 , y= !! year2, yend= !! year1, col = class), size=0.75, show.legend=F) +
    geom_vline(xintercept=0.25, linetype="dashed", size=.1) + 
    geom_vline(xintercept=1.75, linetype="dashed", size=.1) +
    scale_color_manual(labels = c("red", "green"), 
                       values = c("red"="#A51140", "green"="#006663")) +  # color of lines
    labs(x="", y="Average temporal GAP", title = "Slope Chart", subtitle = paste("Average score GAP change: ",year2," vs ",year1)) +  # Axis labels
    xlim(0, 2) + 
    ylim(c(min(as.vector(dplyr:: select(DT, !! year2)), as.vector(dplyr:: select(DT, !! year1)))/1.2)
         ,c(1.1*max(as.vector(dplyr:: select(DT, !! year2)), as.vector(dplyr:: select(DT, !! year1))))) +   # X and Y axis limits
    # Add texts #
    geom_text(label=left_label, y=as.matrix(dplyr:: select(DT, !! year2)), x=rep(0.125, NROW(DT)), size=3.5)+
    geom_text(label=right_label, y=as.matrix(dplyr:: select(DT, !! year1)), x=rep(1.875, NROW(DT)), size=3.5)+
    # #geom_text(label="2018", x=0.1, y=1.1*(max(DT$`2018`, DT$`2019`)), size=5)+  # title
    # #geom_text(label="2019", x=1.9, y=1.1*(max(DT$`2018`, DT$`2019`)), size=5)+ # title
    
    # Minify theme
    theme_skillsMatrix(18) + 
    theme(plot.title = element_text(hjust=0.5, face="bold"),
          plot.background=element_rect(fill="#f7f7f7"),
          panel.background=element_rect(fill="#f7f7f7"),
          panel.grid.minor=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.major.x=element_blank(),
          axis.ticks=element_blank(),
          legend.position="top",
          axis.text.x = element_blank(),
          panel.border=element_blank(),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  
  return(p)
}



makeSlopePlot<- function(DT, xaxis, yaxis, group, Year1, Year2, base_size = 4.5){
  
  
  xaxis <- rlang:: sym(xaxis)
  yaxis <- rlang:: sym(yaxis)
  group <- rlang:: sym(group)
  Year1 <- rlang:: sym(Year1)
  Year2 <- rlang:: sym(Year2)
  
  
  P<- 
    ggplot(data = DT, aes(x = !! xaxis, y = !! yaxis, group = !! group)) +
    geom_line(aes(color = class), size = 1.5) +
    geom_point(aes(color = class), size = 6) +
    geom_text_repel(data = DT %>% filter(Year == Year1), 
                    aes(label = paste0(!! group, " : ", Position)) , 
                    hjust = "left", 
                    fontface = "bold", 
                    size = base_size, 
                    nudge_x = .3, 
                    direction = "y") +
    geom_text_repel(data = DT %>% filter(Year == Year2), 
                    aes(label = paste0(!! group, " : ", Position)) , 
                    hjust = "right", 
                    fontface = "bold", 
                    size = base_size, 
                    nudge_x = -.25, 
                    direction = "y") +
    geom_vline(xintercept=1, linetype="dashed", size=.1) + 
    geom_vline(xintercept=2, linetype="dashed", size=.1) +
    scale_color_manual(labels = c("Down", "Up"), 
                       values = c("red"="#A51140", "green"="#006663")) +
    labs(
      title = "Temporal GAP",
      subtitle = "Average gap by grouping", x = "", y = "Average GAP") + 
    theme_skillsMatrix(base_size = 16) + 
    theme(axis.line.y = ggplot2::element_line(colour = "grey70"),
          axis.line.x = ggplot2::element_line(colour = "grey"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 
  
  
  return(P)
  
  
}

# Year1 <- rlang:: sym("2019")
# Year2 <- rlang:: sym("2018")
# 
# DT <- makeTemporalDT(mytable = ProcessedData,grouping=c("Specific.Skill", "Scale", "High.Level.Cat", "Year")) %>%
#   dplyr:: filter(Scale %in% c("Scale G"), High.Level.Cat %in% c("Systems")) %>%
#   tidyr:: drop_na() %>%
#   dplyr:: ungroup() %>%
#   dplyr:: select(-meanGap, -Mean_Future, -Scale, -High.Level.Cat) %>%
#   tidyr:: spread(key = Year, value = Mean_Now) %>%
#   dplyr:: mutate(class = ifelse((!! Year1 - !! Year2) < 0, "green", "red")) %>%
#   tidyr:: gather(key = Year, value = Position, - class, -Specific.Skill)
# 
# makeSlopePlot(DT = DT, xaxis = "Year", yaxis = "Position", group = "Specific.Skill", Year1 = "2019", Year2 = "2018" )


#--------------------------------------------------------------------------------------------------------------
#function for the skills gap analysis 

makeGapDT <-  function(mytable,grouping,filter0Now=FALSE, filter0Future=TRUE){
  
  # if needed filter out 0    
  if (filter0Now){mytable <- dplyr:: filter(mytable, Rank.Future.Num>0)}
  if (filter0Future){mytable <-  dplyr:: filter(mytable, Rank.Future.Num>0)}
  
  # aggregate    
  # summarySt <- mytable %>%
  #   group_by(.dots=grouping) %>%
  #   mutate(nPeople = n_distinct(ID)) %>% 
  #   ungroup()
  
  summarySt <- mytable %>%       
    group_by(.dots = c(grouping)) %>% 
    #tally(name="Count") %>%
    dplyr::summarise(
      Count = sum(Rank.Future.Num > 0,na.rm = T), 
      meanGap = round(mean(Gap, na.rm = T), 2),
      `AbsGap %` = round(sum(Gap < 0, na.rm = T)/sum(Rank.Future.Num > 0,na.rm = T) *100),
      Mean_Now = round(mean(Rank.Now.Num, na.rm = T),2),
      Mean_Future = round(mean(Rank.Future.Num, na.rm = T),2))
  
  return(summarySt)
}

makeHeatMap <- function(DT, xaxis, yaxis){
  
  xaxis<- rlang:: sym(xaxis)
  yaxis<- rlang:: sym(yaxis)
  
  
  p <- 
    ggplot(DT, aes(!! xaxis, !! yaxis)) +
    geom_tile(aes(fill = meanGap), colour = "white") + 
    scale_y_discrete(expand = c(0, 0)) + 
    scale_fill_gradient(low = "darkred", high = "white") +
    theme_skillsMatrix(base_size = 10)  +
    theme(axis.line.y = ggplot2::element_line(colour = "grey70"),
          axis.line.x = ggplot2::element_line(colour = "grey"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(p)
  
}




makeBoxPlot <- function(DT, xaxis, yaxis){
  
  xaxis<- rlang:: sym(xaxis)
  yaxis<- rlang:: sym(yaxis)
  
  
  p <- 
    ggplot(DT, aes(!! xaxis, !! yaxis)) +
    geom_boxplot(fill= boe_cols[4]) +
    theme_skillsMatrix(base_size = 12) +
    theme(axis.line.y = ggplot2::element_line(colour = "grey70"),
          axis.line.x = ggplot2::element_line(colour = "grey"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_line(colour = "grey")) +
    #scale_y_continuous(breaks = seq(min(DT$Gap), max(DT$Gap), by = 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) +
    coord_flip()  + 
    geom_vline(xintercept = 0, linetype="dotted", 
               color = boe_cols[1], size=1.5) 
  
  return(p)
  
  
}

#function for stock data analysis

makeStockDT <-  function(mytable, grouping, SelectYear, SelectHighCat = FALSE, SelectScale=FALSE, SelectTeam=FALSE, filter0Now=FALSE, filter0Future=FALSE){
  
  # if needed filter out 0    
  if(filter0Now == TRUE){mytable <- dplyr:: filter(mytable, Rank.Now.Num != 0)}
  if(filter0Future == TRUE){mytable <- dplyr:: filter(mytable, Rank.Future.Num != 0)}
  if(SelectScale[1] !=FALSE){mytable <- dplyr:: filter(mytable, Scale %in% SelectScale)}
  if(SelectTeam[1] != FALSE){mytable <- dplyr:: filter(mytable, Team %in% SelectTeam)}
  if(SelectHighCat[1] != FALSE){mytable <- dplyr:: filter(mytable, High.Level.Cat %in% SelectHighCat)}
  
  mytable <- mytable %>%
    dplyr:: filter(Year %in% c(SelectYear)) 
  
  if(nrow(mytable) > 1){
  
  inputKey <- rlang:: sym(dplyr::last(grouping))
  
  Total <- mytable %>%
    dplyr:: group_by(.dots=grouping[-length(grouping)]) %>%
    dplyr:: tally() %>%
    dplyr:: rename("Total" = "n")
  
  inputKey <- rlang:: sym(dplyr::last(grouping))
  # aggregate  
  summarySt <- mytable %>%  
    dplyr:: filter(Year %in% c(SelectYear)) %>%
    group_by(.dots = c(grouping)) %>% 
    dplyr:: tally() %>%
    dplyr:: rename("Count" = "n") %>%
    tidyr:: spread(key = !! inputKey, value = Count) %>%
    dplyr:: ungroup() %>%
    dplyr:: mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>%
    dplyr:: inner_join(Total, by = grouping[-length(grouping)])

  
  
  cols <- colnames(summarySt)[which(!colnames(summarySt) %in% c("No idea but want to learn (0)", "Basic (1)",  "Good (2)", "High (3)", "Expert (4)", "Total"))]
  
  summarySt <- summarySt %>%
    dplyr:: select(cols, contains( "No idea but want to learn (0)"), contains( "Basic (1)"), contains("Good (2)"), contains("High (3)"), contains("Expert (4)" ), contains("Total"))

  
  } else {
    
    summarySt = NULL

  }
  
  
  return(summarySt)
  
}

makeStockBar<-function(df, xlabel, ylabel){
  
  xlabel<- rlang:: sym(xlabel)
  ylabel<- rlang:: sym(ylabel)
  
  df <- df %>% dplyr:: mutate(Number = as.numeric(Number)) %>% dplyr:: mutate(rank = rank(Number, ties.method = 'first'))
  
  c<-ggplot(df, aes(reorder(!!xlabel, -!! ylabel), y= !! ylabel), group = rank) +
    geom_bar(position="dodge", stat="identity", width=0.5, fill="#A51140") +
    labs(title="", x = "Count", y = "Skill") +
    coord_flip() +
    theme_skillsMatrix()
  
  return(c)
  
}

makeStockBarLevel<-function(df, xlabel = "Specific.Skill", ylabel = "Rank.Now"){
  
  xlabel<- rlang:: sym(xlabel)
  ylabel<- rlang:: sym(ylabel)
  
  df <- df %>% 
    dplyr:: mutate(Score = !! ylabel) %>% 
    dplyr:: filter(Score != "Not applicable (0)") %>%
    dplyr:: mutate(rank = rank(Score, ties.method = 'first')) %>%
    arrange(rank) %>% 
    mutate(Score = factor(Score, levels = sort(unique(Score), decreasing = TRUE))) 
  
  labels = c("No idea but want to learn (0)", "Basic (1)", "Good (2)", "High (3)","Expert (4)")
  
  
  p<- 
    ggplot(df, aes(x = !!xlabel), group = rank) +
    geom_bar(aes(fill= Score), width = 0.5) +
    labs(title="", x = "Skill", y = "Count") +
    theme_skillsMatrix() + 
    #ggplot2:: scale_fill_discrete(name = "Score", labels = c("Not applicable (0)", "Basic (1)", "Good (2)", "High (3)","Expert (4)")) +
    ggplot2::scale_fill_manual(values = unname(boe_cols, length(unique(df$Score)))) + 
    coord_flip() 
  
  
  return(p)
  
}

makeStockBarWithLevel<-function(df, xlabel = "Specific.Skill"){
  
  xlabel<- rlang:: sym(xlabel)
  #ylabel<- rlang:: sym(ylabel)
  #inputKey <- rlang:: sym(dplyr::last(grouping[length(grouping)]))
  
  labels = c("No idea but want to learn (0)", "Basic (1)", "Good (2)", "High (3)","Expert (4)")
  
  p<- 
    ggplot(df, aes(x = reorder(!!xlabel, Position), y= Position), group = rank) +
    geom_bar(aes(fill= Score ), width = 0.5, stat = "identity") +
    labs(title="", x = "Skill", y = "Count") +
    theme_skillsMatrix(base_size = 12) + 
    #ggplot2:: scale_fill_discrete(name = "Score", labels = c("Not applicable (0)", "Basic (1)", "Good (2)", "High (3)","Expert (4)")) +
    ggplot2::scale_fill_manual(values = unname(boe_cols, length(unique(df$Score)))) + 
    theme(legend.title = element_blank()) +
    coord_flip() 
  
  return(p)
  
}

makeBreakdownDT <-  function(df, grouping, SelectHighCat = FALSE, SelectScale=FALSE, SelectTeam=FALSE, filter0Now=FALSE, filter0Future=FALSE){
  # if needed filter out 0    
  if(filter0Now == TRUE){df <- dplyr:: filter(df, Rank.Now.Num != 0)}
  if(filter0Future == TRUE){df <- dplyr:: filter(df, Rank.Future.Num != 0)}
  if(SelectScale[1] !=FALSE){df <- dplyr:: filter(df, Scale %in% SelectScale)}
  if(SelectTeam[1] != FALSE){df <- dplyr:: filter(df, Team %in% SelectTeam)}
  if(SelectHighCat[1] != FALSE){df <- dplyr:: filter(df, High.Level.Cat %in% SelectHighCat)}

  if(nrow(df) > 1){
    
    inputKey <- rlang:: sym(dplyr::last(grouping))
    
    df <- df %>%
      group_by(.dots=c(grouping[-length(grouping)]))%>%
      dplyr:: tally() %>%
      dplyr:: rename("Count" = "n") %>%
      dplyr:: arrange(desc(Count))
  }
  
  else{
    
    df = NULL
    
  }
  
  return(df)
}

makeBreakdownChart<-function(df, xlabel,ylabel){
  
  xlabel<- rlang:: sym(xlabel)
  ylabel<- rlang:: sym(ylabel)
  
  
  
  df <- df %>%
    dplyr:: mutate(rank = rank(Count, ties.method = 'first')) %>% dplyr:: ungroup()
  
  p<- 
    ggplot(df, aes(x = reorder(!!xlabel, !! ylabel), y = Count), group = Year) +
    geom_bar(aes(fill= factor(Year), group = Year), width = 0.5, stat = "identity", position = "dodge") +
    labs(title="Quantity of people with this skill per year") +
    ggplot2::scale_fill_manual(values = unname(boe_cols, length(unique(df$Score)))) + 
    coord_flip() +
    theme_skillsMatrix(base_size = 12) + 
    theme(panel.grid.major=element_blank()) 
    
  
  return(p)
  
  }

# aggregate  
# summarySt <- ProcessedData %>%  
#   dplyr:: filter(Rank.Now.Num > 0, Year %in% c("2019"), High.Level.Cat %in% c("Analytical tools"), Scale %in% c("Scale G")) 
# makeStockBarLevel(df = summarySt, xlabel = "Specific.Skill", ylabel = "Rank.Now.Num")

makeStockDiffDT <-  function(mytable, grouping, SelectHighCat = FALSE, SelectScale=FALSE, SelectTeam=FALSE, filter0Now=FALSE, filter0Future=FALSE){
  
  # if needed filter out 0    
  if(filter0Now == FALSE){mytable <- dplyr:: filter(mytable, Rank.Now.Num != 0)}
  if(filter0Future == FALSE){mytable <- dplyr:: filter(mytable, Rank.Future.Num != 0)}
  if(SelectScale[1] !=FALSE){mytable <- dplyr:: filter(mytable, Scale %in% SelectScale)}
  if(SelectTeam[1] != FALSE){mytable <- dplyr:: filter(mytable, Team %in% SelectTeam)}
  if(SelectHighCat[1] != FALSE){mytable <- dplyr:: filter(mytable, High.Level.Cat %in% SelectHighCat)}
  
  if(nrow(mytable) > 1 & length(unique(mytable$Year)) > 1 ){
  
  DT1 <- mytable %>% 
    dplyr:: mutate(Latest = (Year==max(Year)),  Previous = (Year==max(Year[!Latest]))) %>%
    dplyr:: filter(Latest == TRUE)
  
  RP1 <- rlang:: sym(as.character(unique(DT1$Year)))
  
  DT2 <- mytable %>% 
    dplyr:: mutate(Latest = (Year==max(Year)), Previous = (Year==max(Year[!Latest]))) %>%
    dplyr:: filter(Previous == TRUE)
  
  RP2 <- rlang:: sym(as.character(unique(DT2$Year)))
  
  
  # aggregate  
  summarySt1 <- DT1 %>%  
    dplyr:: filter(Rank.Now.Num > 0) %>%
    group_by(.dots = c(grouping)) %>% 
    #tally(name=paste(as.character(RP1))) 
    dplyr:: tally() 
  
  colnames(summarySt1)[which(colnames(summarySt1) == "n")] <- paste(as.character(RP1))
  
  summarySt2 <- DT2 %>%  
    dplyr:: filter(Rank.Now.Num > 0) %>%
    group_by(.dots = c(grouping)) %>% 
    dplyr:: tally()
  
  colnames(summarySt2)[which(colnames(summarySt2) == "n")] <- paste(as.character(RP2))
  
  master <- dplyr:: inner_join(summarySt2, summarySt1, by = grouping) %>%
    dplyr:: mutate(Change = !! RP1 - !! RP2, 
                   `% Change` = round((!! RP1 - !! RP2)/!! RP1*100, 0)) 
  
  
  return(master)
  
  } else {
    
    master = NULL
    
    return(master)
    
  }
  
}


generate_network_plot <- function(mytable, SelectYear, SelectHighCat = FALSE, SelectScale=FALSE, 
                                  SelectTeam=FALSE, filter0Now=FALSE, filter0Future=FALSE){
  
  
  if(filter0Now == TRUE){mytable <- dplyr:: filter(mytable, Rank.Now.Num != 0)}
  if(filter0Future == TRUE){mytable <- dplyr:: filter(mytable, Rank.Future.Num != 0)}
  if(SelectHighCat[1] != FALSE){mytable <- dplyr:: filter(mytable, High.Level.Cat %in% SelectHighCat)}
  

    
    # frequently repoted skills 
    frequentSkills = mytable %>%
      dplyr:: group_by(Specific.Skill) %>%
      filter(n() >= 5)
    
    #frequentSkills %>% count(Specific.Skill)
    if(nrow(frequentSkills) > 2){
    
    word_cors <- frequentSkills %>%
      widyr:: pairwise_cor(Specific.Skill, ID, sort = TRUE)
    
    ######################################
    
    nodes = frequentSkills %>% plyr:: count("Specific.Skill")
    
    edges = word_cors
    
    nodesWithConnections = nodes %>% 
      dplyr:: filter(Specific.Skill %in% union(edges$item1,edges$item2))
    
    g = graph_from_data_frame(edges,directed = T,vertices = nodesWithConnections)
    v = toVisNetworkData(g)
    
    # take square root so size distribution isn't totally dominated by Ninjas!
    v$edges$value = v$edges$correlation * 3
    
    p<- visNetwork(v$nodes,v$edges,height = "800px", width = "800px") %>% 
      visIgraphLayout(layout = "layout_with_fr",grid="nogrid") %>% 
      visPhysics(stabilization = F) %>% 
      visEdges(smooth = F, dashes = T, hoverWidth = 0.3, color = "#A51140") %>% 
      visNodes(scaling = list(min=20,max=70),
               font = list(size = 12), color = "#1E1E1E" ) %>% 
      visOptions(nodesIdSelection = TRUE,collapse = TRUE, highlightNearest = T) %>% 
      visInteraction(navigationButtons = TRUE) %>%
      visInteraction(tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;white-space: wrap;
                     font-family: helvetica;font-size:14px;font-color:black;background-color: white;')
    
  } else{
    
    text = paste("No rows in DF")
  p <-ggplot() + 
      annotate("text",size=8, label = text) + 
      theme_bw() +   theme(panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank())

    
  }
  
  
  return(p)
  
  
}

