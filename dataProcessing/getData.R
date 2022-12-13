library(tidyverse)

# this function gets data from processed folder or process data
getData<-function(
  folderRawData="C:/Users/328576/source/repos/SkillsMatrixPRA/Data",
  folderProcessedData="C:/Users/328576/source/repos/SkillsMatrixPRA/Data/ProcessedData/",
  loadProcessedData=FALSE,
  processedDataDate="2019-10-31", 
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
    # df.list <-
    #   mapply(`[<-`, df.list, 'TeamFolder', value = names(df.list), SIMPLIFY = FALSE)
    
    
    ## add ramdom identifier
    names(df.list) <- 1:length(file.list)
    rawData <- dplyr:: bind_rows(df.list, .id = "id")[,1:9]
    
    
    ## clean names
    colnames(rawData) = c("ID","Name", "Scale","Team", "Role","High.Level.Cat","Specific.Skill","Rank.Now","Rank.Future")
    
    
    ## clean scales
    rawData<- dplyr:: filter(rawData, !is.na(Scale))
    
    ## make the values numeric
    labels<-c("Not applicable"= NA, "No idea but want to learn (0)"=0 , "Basic (1)"=1, "Good / Demonstrating (2)"=2 , "High / Proficient (3)"=3, "Expert (4)" = 4)    
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
    load(paste0("C:/Users/328576/source/repos/SkillsMatrixPRA/ProcessedData/ProcessedData_",processedDataDate,".Rda"))
  }  
  
  processedData
  
}

#this function is used to cleanse the data 
CleanSkillsData <- function(DF){
  
  cols <- c("ID","Name", "Scale","Team", "Role","High.Level.Cat","Specific.Skill","Rank.Now","Rank.Future", "Rank.Now.Num","Rank.Future.Num","Gap","Year")
  
  ColumnTest <- all(cols %in% colnames(DF))
  
  if(ColumnTest == TRUE){
    
    CleanDT <- DF %>% 
      dplyr:: mutate(Scale = recode(Scale, "Industrial placement" = "Scale IP/JP/J/K")) %>%
      dplyr:: mutate(Scale = recode(Scale, "Scale J/JP/K" = "Scale IP/JP/J/K")) %>%
      dplyr:: mutate(Scale = recode(Scale, "Scale J" = "Scale IP/JP/J/K")) %>%
      dplyr:: mutate(Scale = recode(Scale, "Scale IP/JP/K" = "Scale IP/JP/J/K")) %>%
      dplyr:: select(cols) %>%
      dplyr:: mutate(Year = as.character(Year), 
                     ID = as.character(ID)) %>% 
      dplyr:: mutate(Rank.Now = recode(Rank.Now,"Good / Demonstrating (2)" =  "Good (2)" )) %>%
      dplyr:: mutate(Rank.Now = recode(Rank.Now, "High / Proficient (3)" =   "High (3)" )) %>%
      dplyr:: mutate(Rank.Future = recode(Rank.Future, "Good / Demonstrating (2)" =  "Good (2)" )) %>%
      dplyr:: mutate(Rank.Future = recode(Rank.Future, "High / Proficient (3)" =  "High (3)")) 
    
  } else{
    
    CleanDT = NULL
  }
  
  
  return( CleanDT)
  
  
}
#get 2018 YE data 
#processedData2018 <- read.csv("N:/DATA/Cross Divisional Work/SkillsDashboard/Data/2018/processedData2018.csv", stringsAsFactors = FALSE)

#get 2019 YE data from submission files and clean 
processedData2019 <- getData()
#check number of respondants
nTotalMat=length(unique(processedData2019$ID))
nTotalMat
#clean processed data - tidy DF with correct column headings and recoding of data items to ensure consistency 
#processedData2018Clean <- CleanSkillsData(DF = processedData2018)
processedData2019Clean <- CleanSkillsData(DF = processedData2019)

#write to secure location
write.csv(processedData2019Clean, "C:/Users/328576/source/repos/SkillsMatrixPRA/Data/ProcessedData/processedData2019.csv", row.names = F)
