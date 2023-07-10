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

Date=(floor_date(Sys.Date() - months(1), "month"))

reporting_period=floor_date(today()-months(0),"month")

#'[Load Data from Individual Partners google sheets for Level one review 
#'
MatCH<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Dfjvf-K7O0q6i6vHQkzNCFfoSkbtMtB7NvT703GvCSY/edit#gid=1044282265k'), sheet = "4. Reporting tab") %>% mutate(kptype="")%>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,'10/31/2021':'12/31/2023')


RTC<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Dfjvf-K7O0q6i6vHQkzNCFfoSkbtMtB7NvT703GvCSY/edit#gid=1044282265k'), sheet = "4. Reporting tab") %>% mutate(kptype="")%>% select(
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

Missing_data<-Append1 %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period)



#'[Final import output below]

#write_csv(AGYW_DREAMS,"AGYW_PREV_Final.csv")
wb<-createWorkbook("C:/Users/ctrapence/Documents/Dataout/PrEP_DQRT_Feedback.xlsx")

saveWorkbook(wb,"C:/Users/ctrapence/Documents/Dataout/PrEP_DQRT_Feedback.xlsx",overwrite = T)


wb<-loadWorkbook("C:/Users/ctrapence/Documents/Dataout/PrEP_DQRT_Feedback.xlsx")

addWorksheet(wb,"level1")
writeData(wb,sheet="level1",x=Missing_data)

saveWorkbook(wb,"C:/Users/ctrapence/Documents/Dataout/PrEP_DQRT_Feedback.xlsx",overwrite = T)

#saveWorkbook(wb,"DQRT_Feedback.xlsx",overwrite = T)

filename<-paste(Sys.Date(), "Missing_data", ".xlsx")

openxlsx::write.xlsx(Missing_data, file.path(here("Dataout"),filename))
