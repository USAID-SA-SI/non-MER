# Title: Level Two process
# Author: C. Trapence
# Purpose: Automating the process of Reporting AGYW_PREV for Inter-agency
# Date:2023-07-04
# Updated:2023:05:04
#Load Required libraries
# Red text symbolizes comments

#######################################################################################################################
#  sources files used in the code include:                                                                            #
#              1) Data Export from CBMIS                                                                              #
#              2) Host Country Results DREAMS (USG) from DATIM Support                                                #
#              3) Data Exchange Organisation Units from DATIM Support                                                 #
#              4) Mechanisms from DATIM support                                                                       #
#######################################################################################################################

setwd("C:/Users/ctrapence/Documents")

library(tidyverse)
library(here)
library(readxl)
library(lubridate)
library(readr)
library(excel.link)
library(openxlsx)
library(data.table)
library(sqldf)
library(stringr)
library(lubridate)
library(tidyr)
library(anytime)
library(googledrive)
library(googlesheets4)
library(glamr)
library(gargle)



##Step 2: Global variables

#after this step copy and paste into the R profile script and save
#now stored so don't have to authenticate

Date=(floor_date(Sys.Date() - months(2), "month"))

reporting_period=floor_date(today()-months(1),"month")

drive_auth()

columns<-c("mech_code",'7/31/2021','8/31/2021','9/30/2021' ,'10/31/2021','11/30/2021','12/31/2021','1/31/2022','2/28/2022','3/31/2022','4/30/2022','5/31/2022','6/30/2022','7/31/2022','8/31/2022','9/30/2022','10/31/2022','11/30/2022','12/31/2022', '1/31/2023','2/28/2023','3/31/2023','4/30/2023' ,'5/31/2023','6/30/2023' ,'7/31/2023','8/31/2023','9/30/2023' )
columns_b<-c('7/31/2021','8/31/2021','9/30/2021' ,'10/31/2021','11/30/2021','12/31/2021','1/31/2022','2/28/2022','3/31/2022','4/30/2022','5/31/2022','6/30/2022','7/31/2022','8/31/2022','9/30/2022' )

#'[Load Data from Individual Partners google sheets for Level one review 
MatCH<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Dfjvf-K7O0q6i6vHQkzNCFfoSkbtMtB7NvT703GvCSY/edit#gid=1044282265k'), sheet = "4. Reporting tab") 


RTC<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Dfjvf-K7O0q6i6vHQkzNCFfoSkbtMtB7NvT703GvCSY/edit#gid=1044282265k'), sheet = "4. Reporting tab") 

BRCH<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1vjfCZ2QIjKS2ASfsWq4jHGNot38W_FAiaslOp4YR0sE/edit#gid=1044282265'), sheet = "4. Reporting tab") 


ANOVA<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1hJUAaSludW5SbXeng51oc2tbf6pCYHyp2QvxLUq5tYQ/edit#gid=1044282265'), sheet = "4. Reporting tab") 



####FLAGGING MISSING DATRA FOR CORRECTION


Missing_data<-OutputTableau %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period,indicator_status=="Active",mech_code!=80002)

#LEVEL 2 Data checks

DQRT_level1<-OutputTableau


DQRT_level2<-DQRT_level1  %>% filter(year(period)>2022 & age=="<18" &  indicator_status =="Active") %>% dplyr::group_by(primepartner,mech_code,psnu,community,age,period,indicator ) %>% dplyr::summarise(value=sum(value))
  #Level Two checks flags

level2<-dcast(setDT(DQRT_level2),... ~ indicator,value.var = "value")%>% mutate(OVC_HIVSTAT=(OVC_HIVSTAT_Negative  +`OVC_HIVSTAT_Positive_Not Receiving ART`+`OVC_HIVSTAT_Positive_Receiving ART`+
                         `OVC_HIVSTAT_Test Not Required`+`OVC_HIVSTAT_Unknown_No HIV Status` )) %>% select(
   primepartner,mech_code,psnu,community,period,age,OVC_HIVSTAT_Negative :OVC_HIVSTAT ) %>% mutate(period=anydate(period)) %>% filter(period>=Date & period<=reporting_period)
  #Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
  check1<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE)# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18

level2<-dcast(setDT(DQRT_level2),... ~ indicator,value.var = "value") %>% mutate(OVC_HIVSTAT=
                       OVC_HIVSTAT_Negative  +`OVC_HIVSTAT_Positive_Not Receiving ART`+`OVC_HIVSTAT_Positive_Receiving ART`+
                         `OVC_HIVSTAT_Test Not Required`+`OVC_HIVSTAT_Unknown_No HIV Status`)%>% select( primepartner,mech_code,psnu,community,period,age,OVC_HIVSTAT_Negative :OVC_HIVSTAT ) %>% mutate(period=anydate(period)) %>% filter(period>=Date & period<=reporting_period)

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check2<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE)# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18
#Check 3:OVC VLS>OVC_VLR
check3<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE)

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check4<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% 
  
mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")




#'[Final import output below]

#write_csv(AGYW_DREAMS,"AGYW_PREV_Final.csv")

write.xlsx(check1,"C:/Users/ctrapence/Documents/Dataout/DQRT_Feedback.xlsx",  sheetName="Check1",append=TRUE)

wb<-loadWorkbook("C:/Users/ctrapence/Documents/Dataout/DQRT_Feedback.xlsx")

addWorksheet(wb,"check2")
writeData(wb,sheet="check2",x=check2)

addWorksheet(wb,"check3")
addWorksheet(wb,"check4")
writeData(wb,sheet="check3",x=check3)
addWorksheet(wb,"check3")
writeData(wb,sheet="check4",x=check4)


saveWorkbook(wb,"C:/Users/ctrapence/Documents/Dataout/DQRT_Feedback.xlsx",overwrite = T)

#saveWorkbook(wb,"DQRT_Feedback.xlsx",overwrite = T)


write.xlsx(check2,"C:/Users/ctrapence/Documents/Dataout/DQRT_Feedback.xlsx",  sheetName="Check2",append=FALSE)



filename<-paste(Sys.Date(), "Missing_data", ".xlsx")

openxlsx::write.xlsx(Missing_data, file.path(here("Dataout"),filename))
