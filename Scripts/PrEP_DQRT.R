# Title: Level Two process
# Author: C. Trapence
# Purpose: Automating the process of Reporting AGYW_PREV for Inter-agency
# Date:2023-07-11
# Updated:2023:07:12
# Load Required libraries
# Red text symbolizes comments

#######################################################################################################################
#  source files used in the code include:                                                                            #
#              1) Google Sheets                                                                                      #

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
library(tcltk2)
library(svDialogs)


##Step 2: Global variables

#after this step copy and paste into the R profile script and save
#now stored so don't have to authenticate

Date=(floor_date(Sys.Date() - months(2), "month"))

reporting_period=floor_date(today()-months(1),"month")

#'[Load Data from Individual Partners google sheets for Level one review 
#'
MatCH<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Dfjvf-K7O0q6i6vHQkzNCFfoSkbtMtB7NvT703GvCSY/edit#gid=1044282265k'), sheet = "4. Reporting tab") %>% mutate(kptype="")%>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,'10/31/2021':'12/31/2023')


RTC<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Hc99weOc2CjGCKs3pruJ5_YetSwn02zZp6AhCjj6N1g/edit#gid=2092521029'), sheet = "4. Reporting tab") %>% mutate(kptype="")%>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,'10/31/2021':'12/31/2023')

BRCH<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1vjfCZ2QIjKS2ASfsWq4jHGNot38W_FAiaslOp4YR0sE/edit#gid=1044282265'), sheet = "4. Reporting tab") %>% mutate(kptype="")%>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,'10/31/2021':'12/31/2023')


ANOVA<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1hJUAaSludW5SbXeng51oc2tbf6pCYHyp2QvxLUq5tYQ/edit#gid=1044282265'), sheet = "4. Reporting tab") %>% mutate(kptype="")%>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,'10/31/2021':'12/31/2023')


WRHI_70306<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1CxN09HcB711uHgRAaO8HfPOgX86329f4RRKiKMiavr8/edit#gid=1044282265'), sheet = "4. Reporting tab") %>% rename(`5/31/2023`=`05/31/2023`)

WRHI_70301<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1xLat8u9GZLgtpwRJWpe4WwAZ1eoulXXUtgxm7r3dsqo/edit#gid=1044282265'), sheet = "4. Reporting tab")  %>% mutate(kptype="")%>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,'10/31/2021':'12/31/2023')

WRHI_80007<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1oOVQ2swAuXUPmBrI0UOo8mjaTEB87IYC2QSKZ36hPDA/edit#gid=1044282265'), sheet = "4. Reporting tab") %>% mutate(kptype="") %>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,'10/31/2021':'12/31/2023')


FHI360<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1H1hv4EK8RYTrA1OOn38AZtLeJ0ovKbC_mNSAGgJiJ0c/edit#gid=1044282265'), sheet = "4. Reporting tab") 



All_PrEP<-bind_rows(WRHI_70301,WRHI_70306,WRHI_80007,FHI360,ANOVA,BRCH,RTC,MatCH)
####FLAGGING MISSING DATRA FOR CORRECTION

Append1<-gather(All_PrEP,period,value,`10/31/2021`:`12/31/2023`) %>% mutate(period=mdy(period)) %>% mutate(last_refreshed=today(),End_Date=period,Start_Date=period,period_type="Monthly Cummulative within Quarter") %>% mutate(value=abs(value),missing=if_else(  is.na(value),"Yes","No"))%>% 
  group_by( indicator ,partner, mechanismid,country, snu1,snu1id,psnu,psnuuid , kptype,age,sex,period ) %>% summarise(value=sum(value)) %>% mutate(missing=if_else(is.na(value),"Yes","No")) %>%  mutate(flag =(is.na(value)& lag(value))>0)


#'[WRHI_70301 Feedback Tracker]

Missing_data_WRHI_70301<-Append1 %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period) %>% filter(mechanismid==70301)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")


#'[WRHI_70306 Feedback Tracker]

Missing_data_WRHI_70306<-Append1 %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period) %>% filter(mechanismid==70306)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")


#'[WRHI_80007 Feedback Tracker]

Missing_data_WRHI_80007<-Append1 %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period) %>% filter(mechanismid==80007)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

#'[RTC Feedback Tracker]

Missing_data_RTC<-Append1 %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period) %>% filter(mechanismid==70290)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

#'[BRCH Feedback Tracker]

Missing_data_BRCH<-Append1 %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period) %>% filter(mechanismid==70287)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")


#'[MaTCH Feedback Tracker]
Missing_data_MaTCH<-Append1 %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period) %>% filter(mechanismid==81902)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

#'[FHI360 Feedback Tracker]
Missing_data_FHI360<-Append1 %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period) %>% filter(mechanismid==82199)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")


#'[ANOVA Feedback Tracker]

Missing_data_ANOVA<-Append1 %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period) %>% filter(mechanismid==70310) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")
######Checking for Zero reproting
flag_zeros<-Append1 %>%  filter(period>=Date & period<=reporting_period) %>% filter(mechanismid==70310) %>% mutate(zero_reporting=case_when(age=="15-19" & value==0~"Yes",
                                                                                                                                            age=="20-24"& value==0~"Yes",
                                                                                                                                            age=="25-29"& value==0~"Yes",
                                                                                                                                            age=="30-34" &value==0~"Yes",
                                                                                                                                            age=="35-39" & value==0~"Yes",
                                                                                                                                            age=="40-44" & value==0~"Yes",
                                                                                                                                            age=="50+" &  value==0~"Yes",TRUE~"No"))

flag<-flag_zeros$zero_reporting== "Yes"

if (flag==TRUE) {    
  
  dlg_message(c("Please check for zero reporting before clearing the data"))
  
   ""
}






#write_csv(AGYW_DREAMS,"AGYW_PREV_Final.csv")
wb<-write.xlsx(Missing_data_ANOVA,"Dataout/PrEP_DQRT_Feedback_ANOVA.xlsx",sheetName="Missing Data")
wb<-write.xlsx(Missing_data_FHI360,"Dataout/PrEP_DQRT_Feedback_FHI360.xlsx",sheetName="Missing Data")
wb<-write.xlsx(Missing_data_MaTCH,"Dataout/PrEP_DQRT_Feedback_MaTCH.xlsx",sheetName="Missing Data")
wb<-write.xlsx(Missing_data_BRCH,"Dataout/PrEP_DQRT_Feedback_BRCH.xlsx",sheetName="Missing Data")
wb<-write.xlsx(Missing_data_RTC,"Dataout/PrEP_DQRT_Feedback_RTC.xlsx",sheetName="Missing Data")
wb<-write.xlsx(Missing_data_WRHI_80007,"Dataout/PrEP_DQRT_Feedback_WRHI_80007.xlsx",sheetName="Missing Data")
wb<-write.xlsx(Missing_data_WRHI_70306,"Dataout/PrEP_DQRT_Feedback_WRHI_70306.xlsx",sheetName="Missing Data")
wb<-write.xlsx(Missing_data_WRHI_70301,"Dataout/PrEP_DQRT_Feedback_WRHI_70301.xlsx",sheetName="Missing Data")


#'[End ANOVA Feedback Tracker]

RawData<-Append1 %>% select(indicator,	partner,	mechanismid	,country	,snu1	,snu1id,	psnu,	psnuuid,	age,	sex	,period,	kptype,	value)

filename<-paste(Sys.Date(), "RawData", ".xlsx")

openxlsx::write.xlsx(RawData, file.path(here("Dataout"),filename),sheetName="RawData")

#Gen CIRG custom list

CIGR_Lst<-RawData %>% mutate(reportingperiod=perio,	orgunit	orgunituid	mech_code	partner	operatingunit	psnu	indicator	sex	age	otherdisaggregate	population	numdenom	value)



                             