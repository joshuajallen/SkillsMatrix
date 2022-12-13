### this scripts reads all calendars and adds them to the R data
### LP, CN, RL , OLB, DB, FH 
### 05/09/2017

rm(list = ls())
library("stringr")
library("lubridate")
library("hms")
library("dplyr")
library("readr")
library("zoo")
library("tidyr")
library("tictoc")


### Set-up -----


inputsPath<-"N:/DATA/Cross Divisional Work/R/Cross_Div_Projects/NetworkAnalysis/inputs"
refDataPath = paste0(inputsPath,"/ReferenceData")   ### This stays as Reference Data because it has to draw eg historic reporting lines from here
calendarsFolder = paste0(inputsPath,"/dataDSD/")    ### This defines where the calendar data comes from
processedFolder = paste0(inputsPath,"/ProcessedData/")    ### This defines where the calendar data comes from
#roomFolder= "N:/DATA/Cross Divisional Work/R/Cross_Div_Projects/RoomUsage/inputs" ### This defines where the meeting room data comes from


alternativePay<-"alternativePay.csv"
UKbankholidays<-"UKbankholidays.csv"

maildata<-"Copy_of_mailboxes_SS_13-03-18.csv"
staffdata<-"HistoricReportingLinesJulyForwardtoMarch.csv"
#maildata<-"Copy_of_mailboxes_SS_23-09-18.csv"
#staffdata<-"HistoricReportingLinesJulyForwardtoAugust2.csv"



#roomsData<-"Bank of England Meeting Room Sense Data.xlsx"

## Functions -----
source("./Scripts/helperFunsShiny.R")


### Get differencial data  -----

# Get UKbankHolidays
UKbankholidays <- read_csv(file.path(refDataPath,"UKbankholidays.csv"),col_names = F)
UKbankholidays<-mutate(UKbankholidays, dates=dmy(X1) ) %>% select(-X1)

# Get meeting room data

# mapRooms<-read_excel(file.path(roomFolder,roomsData))
# colnames(mapRooms) <- tolower(make.names(colnames(mapRooms), unique=TRUE))
# mapRooms<-mapRooms %>% select( location, floor, department, category,room.name,room.size)%>% 
#   mutate(room.size=as.numeric(room.size))
# colnames(mapRooms)[1:4]<-paste0("room.",colnames(mapRooms)[1:4])

# Get the stafflist & make sure to remove numbers not there yet
liststaff = cleanHistoricStaffListShiny(refDataPath, alternativePay, maildata,staffdata)
staff=liststaff$staff %>% select(-directorate.join.date, -length.of.service...directorate,-length.of.service...division, -fte.at.end,-headcount.at.end )

#if email lsist is not recent
staff<-staff[!is.na(staff$emailname),]
save(UKbankholidays,staff, file=paste0(processedFolder,"ReferenceData_",Sys.Date(),".Rda"))
