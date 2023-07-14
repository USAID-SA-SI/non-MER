# Title: Level Two process
# Author: C. Trapence
# Purpose: Automating the process of Reporting AGYW_PREV for Inter-agency
# Date:2023-07-04
# Updated:2023:05:04
#Load Required libraries
# Red text symbolizes comments

#######################################################################################################################
# #  sources files used in the code include:                                                                            #
# #              1)Partners Google sheets                                                                             #
# #                                                                                     #
# #######################################################################################################################
# 
# #Make sure you have the following packages installed 
# install.packages("devtools")
# install.packages("googledrive")
# install.packages("gargle")
# install.packages("googlesheets4")
# devtools::install_github("USAID-OHA-SI/glamr", build_vignettes = TRUE)
# vignette("credential-management", package = "glamr")
# install.packages("validate")
# install.packages("plyr")
# install.packages("rmarkdown")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("readxl")
# install.packages("glamr")
# install.packages("purrr")
# install.packages("data.table")
# install.packages("gophr")
# install.packages("keyring")
# install.packages("fs")
# install.packages("readxl")
# install.packages("lubridate")
# install.packages("glue")
# install.packages("validate")
# install.packages("plyr")
# install.packages("here")
# install.packages("janitor")
# install.packages("anytime")

setwd("C:/Users/ctrapence/Documents")

library(tidyverse)
library(here)
library(readxl)
library(lubridate)
library(readr)
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
library(janitor)

load_secrets()
##Step 2: Global variables

#after this step copy and paste into the R profile script and save
#now stored so don"t have to authenticate

Date=(floor_date(Sys.Date() - months(1), "month"))

reporting_period=floor_date(today()-months(0),"month")

columns<-c("mech_code","7/31/2021","8/31/2021","9/30/2021" ,"10/31/2021","11/30/2021","12/31/2021","1/31/2022","2/28/2022","3/31/2022","4/30/2022","5/31/2022","6/30/2022","7/31/2022","8/31/2022","9/30/2022","10/31/2022","11/30/2022","12/31/2022", "1/31/2023","2/28/2023","3/31/2023","4/30/2023" ,"5/31/2023","6/30/2023" ,"7/31/2023","8/31/2023","9/30/2023" )
columns_b<-c("7/31/2021","8/31/2021","9/30/2021" ,"10/31/2021","11/30/2021","12/31/2021","1/31/2022","2/28/2022","3/31/2022","4/30/2022","5/31/2022","6/30/2022","7/31/2022","8/31/2022","9/30/2022" )

#'[Load Data from Individual Partners google sheets for Level one review 
HIVSA<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/u/1/d/1-EDzs3Wxf-T6r6SJ_AepsqDbln-LAM8fwy9IPhLvd8g/edit?usp=drive_open&ouid=109338274002987002395#gid=329992892"), sheet = "OVC Indicators") %>% janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
"12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
"1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` ) %>% mutate_if(is.list,as.character)
HIVSA[columns]<-sapply(HIVSA[columns],as.integer)

FHI360<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1KLFnNV5szfw0Ns1Q4kjV5J2x7qp4biYTeDwU4YMdJlM/edit?usp=drive_link"), sheet = "OVC Indicators") %>% janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
"12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
"1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` )  %>% mutate_if(is.list,as.character)
FHI360[columns]<-sapply(FHI360[columns],as.integer)

PACT<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1G8xIspdRzFswVk59WvYjVK22k7_Pi9EqrSp2uvdA-HA/edit#gid=0"), sheet = "OVC Indicators") %>% janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
"12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
"1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` )  %>% mutate_if(is.list,as.character)
PACT[columns]<-sapply(PACT[columns],as.integer)

M2M<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1BLl3QcQyZJulAkxDWONJRXOcvMDEOPtmHbbtHLGCMXA/edit#gid=329992892
"), sheet = "OVC Indicators") %>% janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
"12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
"1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` ) %>% mutate_if(is.list,as.character)
M2M[columns]<-sapply(M2M[columns],as.integer)

CINDI<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/11v2uMvKG2WSsOeKy_KFQEssE5dznkT3-TYzk88gvpuw/edit#gid=0"), sheet = "OVC Indicators") %>% janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
"12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
"1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` )  %>% mutate_if(is.list,as.character)
CINDI[columns]<-sapply(CINDI[columns],as.integer)

  G2G<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/18sQPukk1KQhyGNogJDbjvcGJRBOJQYzAN7pdwmbfixU/edit#gid=329992892"), sheet = "OVC Indicators") %>% janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
"12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
"1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` )  %>% mutate_if(is.list,as.character) 
  G2G[columns]<-sapply(G2G[columns],as.integer)
  
  
NACOSA_A<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1ePof8YITyIUiZn7FfKZeaUje5PR57MbLig95rSMeK-M/edit#gid=329992892"), sheet = "OVC Indicators") %>% janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
"12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000` ) %>% select( -(`1667174400`:`1696032000`))   %>% mutate(mech_code=unlist(mech_code)) %>% mutate_if(is.list,as.character)
NACOSA_A[columns_b]<-sapply(NACOSA_A[columns_b],as.integer)

NACOSA_B<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1pbH6R54ioOjf-9rz12TNLJLqpx_9j-6CDQaSi3VDpIo/edit#gid=329992892"), sheet = "OVC Indicators") %>% janitor::row_to_names(1) %>% rename("10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
"1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` ) %>% mutate(mech_code=unlist(mech_code))  %>% mutate_if(is.list,as.integer)

NACOSA_All<-bind_rows(NACOSA_A,NACOSA_B)%>%  mutate_if(is.list, as.character) 
  

NACOSA_All[columns]<-sapply(NACOSA_All[columns],as.integer)

AllData<-bind_rows(NACOSA_All,FHI360,CINDI,G2G,M2M,PACT,HIVSA) 

#Level Checks:Looking for missing data across indicator points

OutputTableau<-gather(AllData,period,value,`7/31/2021`:`9/30/2023`) %>% mutate(period=mdy(period)) %>% mutate(last_refreshed=today(),End_Date=period,Start_Date=period,period_type="Monthly Cummulative within Quarter") %>% mutate(value=abs(value),missing=if_else(  is.na(value),"Yes","No"))%>% 
  group_by( mech_code ,primepartner, psnu,community,indicator ,last_refreshed,disaggregate , age,otherdisaggregate , community_status,indicator_status, Start_Date,period,period_type  ,missing ) %>% summarise(value=sum(value)) %>% select(mech_code ,primepartner, psnu,community,indicator,value ,Start_Date,period,last_refreshed,missing,period_type,age,disaggregate ,
     otherdisaggregate , community_status,indicator_status) 
###FLAGGING MISSING DATRA FOR CORRECTION



#LEVEL 2 Data checks

DQRT_level1<-OutputTableau

DQRT_level2<-DQRT_level1  %>% filter(year(period)>2022 & age=="<18" &  indicator_status =="Active") %>% dplyr::group_by(primepartner,mech_code,psnu,community,age,period,indicator ) %>% dplyr::summarise(value=sum(value))
  #Level Two checks flags

level2<-dcast(setDT(DQRT_level2),... ~ indicator,value.var = "value")%>% mutate(OVC_HIVSTAT=(OVC_HIVSTAT_Negative  +`OVC_HIVSTAT_Positive_Not Receiving ART`+`OVC_HIVSTAT_Positive_Receiving ART`+
                         `OVC_HIVSTAT_Test Not Required`+`OVC_HIVSTAT_Unknown_No HIV Status` )) %>% select(
   primepartner,mech_code,psnu,community,period,age,OVC_HIVSTAT_Negative :OVC_HIVSTAT ) %>% mutate(period=anydate(period)) %>% filter(period>=Date & period<=reporting_period)


#'[HIVSA Partner feedback]
#Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
check1_HIVSA<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>% 
  mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check2_HIVSA<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>% 
  filter(mech_code=="70307")
#Check 3:OVC VLS>OVC_VLR
check3_HIVSA<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check4_HIVSA<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")

#'[Final import output below]


Missing_data_HIVSA<-OutputTableau %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period,indicator_status=="Active",mech_code!=80002) %>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>% 
  filter(mech_code=="70307")

write.xlsx(check1_HIVSA,"Dataout/OVC_DQRT_Feedback_HIVSA.xlsx",  sheetName="Check1",append=TRUE)


wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_HIVSA.xlsx")

addWorksheet(wb,"check2")
writeData(wb,sheet="check2",x=check2_HIVSA)

addWorksheet(wb,"check3")
writeData(wb,sheet="check3",x=check3_HIVSA)

addWorksheet(wb,"check4")
writeData(wb,sheet="check4",x=check4_HIVSA)

addWorksheet(wb,sheetName = "Missing_Data")
writeData(wb,sheet = "Missing_Data",x=Missing_data_HIVSA)

saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_HIVSA.xlsx",overwrite = T)
#'[HIVSA END]

#'[FHI360 Partner feedback]
Missing_data_FHI360<-OutputTableau %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period,indicator_status=="Active",mech_code!=80002) %>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>% 
  filter(mech_code=="14295")
  #Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
  check1_FHI360<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>% 
    mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="14295")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18

level2<-dcast(setDT(DQRT_level2),... ~ indicator,value.var = "value") %>% mutate(OVC_HIVSTAT=
                       OVC_HIVSTAT_Negative  +`OVC_HIVSTAT_Positive_Not Receiving ART`+`OVC_HIVSTAT_Positive_Receiving ART`+
                         `OVC_HIVSTAT_Test Not Required`+`OVC_HIVSTAT_Unknown_No HIV Status`)%>% select( primepartner,mech_code,psnu,community,period,age,OVC_HIVSTAT_Negative :OVC_HIVSTAT ) %>% mutate(period=anydate(period)) %>% filter(period>=Date & period<=reporting_period) %>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check2_FHI360<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>% 
  filter(mech_code=="14295")
#Check 3:OVC VLS>OVC_VLR
check3_FHI360<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="14295")

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check4_FHI360<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="14295")

#'[Final import output below]


write.xlsx(check1_FHI360,"Dataout/OVC_DQRT_Feedback_FHI360.xlsx",  sheetName="Check1",append=TRUE)


wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_FHI360.xlsx")

addWorksheet(wb,"check2")
writeData(wb,sheet="check2",x=check2_FHI360)

addWorksheet(wb,"check3")
writeData(wb,sheet="check3",x=check3_FHI360)

addWorksheet(wb,"check4")
writeData(wb,sheet="check4",x=check4_FHI360)

addWorksheet(wb,sheetName = "Missing_Data")
writeData(wb,sheet = "Missing_Data",x=Missing_data_FHI360)

saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_FHI360.xlsx",overwrite = T)
#'[FHI360 END]

#'[PACT Partner feedback]

Missing_data_PACT<-OutputTableau %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period,indicator_status=="Active",mech_code!=80002) %>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>% 
  filter(mech_code=="14631")

#Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
check1_PACT<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>% 
  mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="14631")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check2_PACT<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>% 
  filter(mech_code=="14631")
#Check 3:OVC VLS>OVC_VLR
check3_PACT<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="14631")

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check4_PACT<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="14631")

write.xlsx(check1_PACT,"Dataout/OVC_DQRT_Feedback_PACT.xlsx",  sheetName="Check1",append=TRUE)

wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_PACT.xlsx")

addWorksheet(wb,"check2")
writeData(wb,sheet="check2",x=check2_PACT)

addWorksheet(wb,"check3")
writeData(wb,sheet="check3",x=check3_PACT)

addWorksheet(wb,"check4")
writeData(wb,sheet="check4",x=check4_PACT)

addWorksheet(wb,sheetName = "Missing_Data")
writeData(wb,sheet = "Missing_Data",x=Missing_data_PACT)

saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_PACT.xlsx",overwrite = T)
#'[PACT END]

#'[G2G Partner feedback]

Missing_data_G2G<-OutputTableau %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period,indicator_status=="Active",mech_code!=80002) %>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>% 
  filter(mech_code=="81904")

#Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
check1_G2G<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>% 
  mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="81904")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check2_G2G<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>% 
  filter(mech_code=="81904")
#Check 3:OVC VLS>OVC_VLR
check3_G2G<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="81904")

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check4_G2G<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="81904")

write.xlsx(check1_G2G,"Dataout/OVC_DQRT_Feedback_G2G.xlsx",  sheetName="Check1",append=TRUE)

wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_G2G.xlsx")

addWorksheet(wb,"check2")
writeData(wb,sheet="check2",x=check2_G2G)

addWorksheet(wb,"check3")
writeData(wb,sheet="check3",x=check3_G2G)

addWorksheet(wb,"check4")
writeData(wb,sheet="check4",x=check4_G2G)

addWorksheet(wb,sheetName = "Missing_Data")
writeData(wb,sheet = "Missing_Data",x=Missing_data_G2G)

saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_G2G.xlsx",overwrite = T)
#'[G2G END]

#'[M2M Partner feedback]

Missing_data_M2M<-OutputTableau %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period,indicator_status=="Active",mech_code!=80002) %>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>% 
  filter(mech_code=="80004")

#Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
check1_M2M<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>% 
  mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="80004")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check2_M2M<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>% 
  filter(mech_code=="80004")
#Check 3:OVC VLS>OVC_VLR
check3_M2M<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80004")

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check4_M2M<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80004")

write.xlsx(check1_M2M,"Dataout/OVC_DQRT_Feedback_G2G.xlsx",  sheetName="Check1",append=TRUE)

wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_G2G.xlsx")

addWorksheet(wb,"check2")
writeData(wb,sheet="check2",x=check2_M2M)

addWorksheet(wb,"check3")
writeData(wb,sheet="check3",x=check3_M2M)

addWorksheet(wb,"check4")
writeData(wb,sheet="check4",x=check4_M2M)

addWorksheet(wb,sheetName = "Missing_Data")
writeData(wb,sheet = "Missing_Data",x=Missing_data_M2M)

saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_M2M.xlsx",overwrite = T)
#'[M2M END]


#'[CINDI Partner feedback]

Missing_data_CINDI<-OutputTableau %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period,indicator_status=="Active",mech_code!=80002) %>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>% 
  filter(mech_code=="70311")

#Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
check1_CINDI<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>% 
  mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="70311")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check2_CINDI<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>% 
  filter(mech_code=="70311")
#Check 3:OVC VLS>OVC_VLR
check3_CINDI<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70311")

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check4_CINDI<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70311")

write.xlsx(check1_CINDI,"Dataout/OVC_DQRT_Feedback_CINDI.xlsx",  sheetName="Check1",append=TRUE)

wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_CINDI.xlsx")

addWorksheet(wb,"check2")
writeData(wb,sheet="check2",x=check2_CINDI)

addWorksheet(wb,"check3")
writeData(wb,sheet="check3",x=check3_CINDI)

addWorksheet(wb,"check4")
writeData(wb,sheet="check4",x=check4_CINDI)

addWorksheet(wb,sheetName = "Missing_Data")
writeData(wb,sheet = "Missing_Data",x=Missing_data_CINDI)

saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_CINDI.xlsx",overwrite = T)
#'[CINDI END]


#'[NACOSA Partner feedback]

Missing_data_NACOSA<-OutputTableau %>% filter(missing=="Yes") %>% filter(period>=Date & period<=reporting_period,indicator_status=="Active",mech_code!=80002) %>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>% 
  filter(mech_code=="80008")

#Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
check1_NACOSA<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>% 
  mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="80008")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check2_NACOSA<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>% 
  filter(mech_code=="80008")
#Check 3:OVC VLS>OVC_VLR
check3_NACOSA<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80008")

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check4_NACOSA<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80008")

write.xlsx(check1_NACOSA,"Dataout/OVC_DQRT_Feedback_NACOSA.xlsx",  sheetName="Check1",append=TRUE)

wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_NACOSA.xlsx")

addWorksheet(wb,"check2")
writeData(wb,sheet="check2",x=check2_NACOSA)

addWorksheet(wb,"check3")
writeData(wb,sheet="check3",x=check3_NACOSA)

addWorksheet(wb,"check4")
writeData(wb,sheet="check4",x=check4_NACOSA)

addWorksheet(wb,sheetName = "Missing_Data")
writeData(wb,sheet = "Missing_Data",x=Missing_data_NACOSA)
#'[NACOSA END]

saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_NACOSA.xlsx",overwrite = T)


#'[OVC Dashboard output file
write.xlsx(OutputTableau,"Dataout/OVC_OutputTableau.xlsx",sheetName="1OutputTableau",apppend=T)

