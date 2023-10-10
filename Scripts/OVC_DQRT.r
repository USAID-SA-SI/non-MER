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
library(readr)

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
     otherdisaggregate , community_status,indicator_status) %>% mutate(datasource="GoogleDrive",valuetype="Results") %>% mutate(partnershort=if_else(mech_code == 80008  | mech_code== 80002,"NACOSA",primepartner)) %>% filter(indicator_status=="Active")



###FLAGGING MISSING DATRA FOR CORRECTION


#LEVEL 2 Data checks

DQRT_level1<-OutputTableau

DQRT_level2<-DQRT_level1  %>% filter(year(period)>2022 & age=="<18" &  indicator_status =="Active") %>% dplyr::group_by(primepartner,mech_code,psnu,community,age,period,indicator ) %>% dplyr::summarise(value=sum(value))
  #Level Two checks flags


level2<-dcast(setDT(DQRT_level2),... ~ indicator,value.var = "value") %>% mutate(OVC_HIVSTAT=
                                                                                   OVC_HIVSTAT_Negative  +`OVC_HIVSTAT_Positive_Not Receiving ART`+`OVC_HIVSTAT_Positive_Receiving ART`+
                                                                                   `OVC_HIVSTAT_Test Not Required`+`OVC_HIVSTAT_Unknown_No HIV Status`)%>% select( primepartner,mech_code,psnu,community,period,age,OVC_HIVSTAT_Negative :OVC_HIVSTAT ) %>% mutate(period=anydate(period)) %>% filter(period>=Date & period<=reporting_period) %>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

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
writeData(wb,sheet="check2",check2_HIVSA)

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



#' #Skipping Tableau Prep Script file
#' 
#' 
#' 
#' OutputTableau2<- OutputTableau%>%   mutate(standardizedDisaggregate=if_else (indicator == "OVC_HIVSTAT_Positive_Receiving ART" | indicator == "OVC_HIVSTAT_Test Not Required" | indicator== "OVC_HIVSTAT_Positive_Not Receiving ART" | indicator == "OVC_HIVSTAT_Negative" | indicator == "OVC_HIVSTAT_Unknown_No HIV Status",  "ReportedStatus",
#'                                                                            if_else(indicator == "OVC_SERV_ Preventive" ,"Age/Sex/Preventive",if_else(indicator == "OVC_SERV_DREAMS" , "Age/Sex/DREAMS",if_else(indicator =="OVC_SERV_Comprehensive" & disaggregate == "Secondary Education" ,  "Age/Sex/ProgramStatus",if_else(indicator== "OVC_SERV_Comprehensive" & age == "<18" ,  "Age/Sex/ProgramStatus",
#'                                                                                                                                                                                                                                                                                                                                  if_else(indicator== "OVC_SERV_Comprehensive" & disaggregate == "Caregivers" , "Age/Sex/ProgramStatusCaregiver",if_else(indicator== "OVC_SERV_EWG_Service Lapse" , "ProgramStatus", 
#'                                                                                                                                                                                                                                                                                                                                                                                                                                                         if_else(indicator== "OVC_SERV_Potentially Active" , "ProgramStatus",""))) ))))))
#' OutputTableau3<-OutputTableau2 %>%  mutate(statushiv=indicator,disaggregate=standardizedDisaggregate) %>% mutate(yearquarter=if_else(period=="2021-07-31"  , "FY2021Q4",if_else(
#'   period=="2021-08-31"  , "FY2021Q4",if_else(  period=="2021-09-30"  ,"FY2021Q4",if_else( period=="2021-10-31"  , "FY2022Q1",if_else( period=="2021-11-30"  , "FY2022Q1",if_else(
#'     period=="2021-12-31"  , "FY2022Q1",if_else(  period=="2022-01-31"  ,"FY2022Q2",if_else(period=="2022-02-28"  , "FY2022Q2",if_else(  period=="2022-03-31"  , "FY2022Q2",if_else(
#'       period=="2022-04-30"  , "FY2022Q3",if_else(  period=="2022-05-31"  , "FY2022Q3",if_else(  period=="2022-06-30"  , "FY2022Q3",if_else(  period=="2022-07-31"  , "FY2022Q4",if_else(
#'         period=="2022-08-31"  , "FY2022Q4",if_else(  period=="2022-09-30"  , "FY2022Q4",if_else(  period=="2022-10-31"  , "FY2023Q1",if_else(  period=="2022-11-30"  , "FY2023Q1",if_else(
#'           period=="2022-12-31"  , "FY2023Q1",if_else(  period=="2023-01-31"  , "FY2023Q2",if_else(  period=="2023-02-28"  , "FY2023Q2",if_else(  period=="2023-03-31"  , "FY2023Q2",if_else(
#'             period=="2023-04-30"  , "FY2023Q3",if_else(  period=="2023-05-31"  , "FY2023Q3",if_else(  period=="2023-06-30"  , "FY2023Q3",if_else(  period=="2023-07-31"  , "FY2023Q4",if_else(
#'               period=="2023-08-31"  , "FY2023Q4",if_else(  period=="2023-09-30"  , "FY2023Q4"," "))))))))))))))))))))))))))))
#' 
#' 
#' OutputTableau4<-OutputTableau3%>% mutate(fiscal_year=case_when(yearquarter=="FY2020Q4" ~ "FY2020",
#'                                                                yearquarter=="FY2021Q1" ~ "FY2021",
#'                                                                yearquarter=="FY2021Q2" ~ "FY2021",
#'                                                                yearquarter=="FY2021Q3" ~ "FY2021",
#'                                                                yearquarter=="FY2021Q4" ~ "FY2021",
#'                                                                yearquarter=="FY2022Q1" ~ "FY2022",
#'                                                                yearquarter=="FY2022Q2" ~ "FY2022",
#'                                                                yearquarter=="FY2022Q3" ~ "FY2022",
#'                                                                yearquarter=="FY2022Q4" ~ "FY2022",
#'                                                                yearquarter=="FY2023Q1" ~ "FY2023",
#'                                                                yearquarter=="FY2023Q2" ~ "FY2023",
#'                                                                yearquarter=="FY2023Q3" ~ "FY2023",
#'                                                                yearquarter=="FY2023Q4" ~ "FY2023",FALSE~""))
#' 
#' OutputTableau5<-OutputTableau4 %>% mutate(otherDisaggregate=if_else(indicator =="OVC_HIVSTAT_Negative" , "",if_else(indicator == "OVC_HIVSTAT_Positive_Not Receiving ART" , "Not Receiving ART",if_else(
#'   indicator== "OVC_HIVSTAT_Positive_Receiving ART" , "Receiving ART",if_else(indicator== "OVC_HIVSTAT_Test Not Required" , "Test Not Required",if_else(indicator== "OVC_HIVSTAT_Unknown_No HIV Status" , "No HIV Status",
#'                                                                                                                                                        if_else(indicator== "OVC_SERV_EWG_Service Lapse" , "Exited without Graduation",if_else(indicator== "OVC_SERV_Potentially Active" , "Potentially Active","")))))))) %>% 
#'   mutate(age=if_else(age == "<18" , "<18",if_else(age== "18+" , "18+" ,if_else(age == "18-20" , "18-20", if_else( indicator== "OVC_SERV_ Preventive" , "<18",if_else (indicator== "OVC_SERV_DREAMS" , "<18","")))))) %>% mutate(funding_agency="USAID") %>% rename(prime_partner_name=primepartner) %>%
#'   mutate(indicator=if_else(indicator=="OVC_SERV_Potentially Active","OVC_SERV",indicator)) %>% group_by_all() %>% summarise(value=sum(value)) %>% mutate(operatingunituid="cDGPF739ZZr",country="South Africa") %>% mutate(orgunituid=		
#' case_when(community=="ec Amahlathi Local Municipality" ~	"XzKgahqXvhA"	,
#' community=="ec Buffalo City Local Municipality" ~	"QPEuCIzKBZb"	,
#' community=="ec Emalahleni Local Municipality" ~	"NlhPVQEKfmH"	,
#' community=="ec Engcobo Health sub-District" ~	"diGGc8zzqNf"	,
#' community=="ec Enoch Mgijima Local Municipality" ~	"uwZ6kIFBeRN"	,
#' community=="ec Ingquza Hill Local Municipality" ~	"Ui5a3DF84Vw"	,
#' community=="ec Intsika Yethu Local Municipality" ~	"LzI1Aqz2zE0"	,
#' community=="ec King Sabata Dalindyebo Local Municipality" ~	"m9Q4H1NiXPQ"	,
#' community=="ec Matatiele Local Municipality" ~	"CslOGPFjCGC"	,
#' community=="ec Mbhashe Local Municipality" ~	"BmB1vGlbHa0"	,
#' community=="ec Mbizana Local Municipality" ~	"Mqgc8hLJZmu"	,
#' community=="ec Mnquma Local Municipality" ~	"Lb4ba8PPgPS"	,
#' community=="ec Ntabankulu Local Municipality" ~	"h1Y6W3XNp4R"	,
#' community=="ec Nyandeni Health sub-District" ~	"VDhJfm06Jkv"	,
#' community=="ec Port St Johns Local Municipality" ~	"eH8Yqx05VGU"	,
#' community=="ec Raymond Mhlaba Local Municipality" ~	"XeyQABePuXa"	,
#' community=="ec Umzimvubu Health sub-District" ~	"w589pQ7mkgb"	,
#' community=="fs Dihlabeng Local Municipality" ~	"b0AuEG9wuo7"	,
#' community=="fs Maluti-a-Phofung Local Municipality"~	"FXwsp2b12ba"	,
#' community=="fs Mantsopa Local Municipality" ~	"x1Ls7DEDROq"	,
#' community=="fs Masilonyana Local Municipality" ~	"Zffl7AthEEm"	,community=="fs Matjhabeng Local Municipality" ~	"C8bcDMSOBNS"	,
#' community=="fs Nala Local Municipality" ~	"uG0iBmY9x7S"	,
#' community=="fs Nketoana Local Municipality" ~	"Nu19DAcL0eE"	,
#' community=="fs Phumelela Local Municipality" ~	"Oo1o6LUGLtI"	,
#' community=="fs Setsoto Local Municipality" ~	"J2pT6LrlERj"	,
#' community=="gp Ekurhuleni East 1 Local Municipality" ~	"c8P5lXZo5U1"	,
#' community=="gp Ekurhuleni East 2 Local Municipality" ~	"xMTTHM2fftG"	,
#' community=="gp Ekurhuleni North 1 Health sub-District" ~	"LxMoklHIayz"	,
#' community=="gp Ekurhuleni North 1 Local Municipality" ~	"LxMoklHIayz"	,
#' community=="gp Ekurhuleni North 2 Health sub-District" ~	"K9GdUGILK1S"	,
#' community=="gp Ekurhuleni South 1 Local Municipality" ~	"OoScyXDPGtM"	,
#' community=="gp Ekurhuleni South 2 Local Municipality" ~	"hGT2SBC4dH3"	,
#' community=="gp Emfuleni Local Municipality" ~	"hmFJXC3ZOPj"	,
#' community=="gp Johannesburg A Health sub-District" ~	"mr2p5Mqbony"	,
#' community=="gp Johannesburg B Health sub-District" ~	"O6HPLoa2WbQ"	,
#' community=="gp Johannesburg C Health sub-District" ~	"o0CzjRVpRJ6"	,
#' community=="gp Johannesburg D Health sub-District" ~	"zf2BeMdf62h"	,
#' community=="gp Johannesburg E Health sub-District" ~	"a5UyMYoJiBB"	,
#' community=="gp Johannesburg F Health sub-District" ~	"oI1rnMlEJz1"	,
#' community=="gp Johannesburg G Health sub-District" ~	"DPnhkLN88vc"	,community=="gp Tshwane 1 Local Municipality" ~	"qLQnLlFj8TL"	,
#' community=="gp Tshwane 2 Local Municipality" ~	"Jgd0zBKuHbm"	,
#' community=="gp Tshwane 3 Local Municipality" ~	"vQ3MumZt4Ie"	,
#' community=="gp Tshwane 4 Local Municipality" ~	"f3ZdI0ihEQE"	,
#' community=="gp Tshwane 5 Local Municipality" ~	"PSDpC7k4tvo"	,
#' community=="gp Tshwane 6 Local Municipality" ~	"UmwGr8Tv38c"	,
#' community=="gp Tshwane 7 Local Municipality" ~	"PHv1qPlJD91"	,
#' community=="kz Abaqulusi Local Municipality" ~	"VQ81fQp6JGu"	,
#' community=="kz Alfred Duma Local Municipality"~	"cROE42XaZhr"	,
#' community=="kz City of uMhlathuze Local Municipality" ~	"QAP8Lt9EWhI"	,
#' community=="kz Edumbe Local Municipality" ~	"e75DWL3OSX3"	,
#' community=="kz Emnambithi Local Municipality" ~	"Vwj3TmjnRd0"	,
#' community=="kz eThekwini Metropolitan Municipality Sub" ~	"NlX1mBFnTfd"	,
#' community=="kz Impendle Local Municipality" ~	"pKpJB0to5Hc"	,
#' community=="kz Inkosi Langalibalele Local Municipality" ~	"DOs1dhcqU78"	,
#' community=="kz Msunduzi Local Municipality" ~	"Hadi7Geu8w2"	,
#' community=="kz Mthonjaneni Local Municipality" ~	"cfqLGWHRA06"	,
#' community=="kz Nkandla Local Municipality" ~	"u5tDFzWbCwA"	,
#' community=="kz Nongoma Local Municipality" ~	"MmbSZqsjiPt"	,
#' community=="kz Okhahlamba Local Municipality" ~	"dvHSbaM5yjN"	,
#' community=="kz Ray Nkonyeni Local Municipality" ~	"XWzywhO0xak"	,
#' community=="kz Richmond Local Municipality" ~"Fdr6Wsz9ior"	,
#' community=="kz Ubuhlebezwe Local Municipality" ~	"GqopJ97KaAW"	,
#' community=="kz Ulundi Local Municipality" ~	"eQLaDwaqBbT"	,
#' community=="kz Umdoni Local Municipality" ~	"q9hs4q9W9FE"	,
#' community=="kz uMfolozi Local Municipality" ~	"oiukKexEqh4"	,
#' community=="kz uMlalazi Local Municipality" ~	"pWdHN41uiRP"	,
#' community=="kz Umtshezi Local Municipality" ~	"qM89xYLRdqK"	,
#' community=="kz uMuziwabantu Local Municipality" ~	"DdUJ8eMODYD"	,
#' community=="kz Umzimkhulu Local Municipality" ~	"ZGpw4FGxMqO"	,
#' community=="kz Umzumbe Local Municipality" ~	"Rt8UXcd34h5"	,
#' community=="kz uPhongolo Local Municipality" ~	"oE1zSYtAqqW"	,
#' community=="lp Ba-Phalaborwa Local Municipality" ~	"z1tNxqzlzwn"	,
#' community=="lp Blouberg Local Municipality" ~	"k6WaAJ5EcLH"	,
#' community=="lp Greater Giyani Local Municipality" ~	"gvb9vE05rpo"	,
#' community=="lp Greater Letaba Local Municipality" ~	"j3cmhkFeTDD"	,
#' community=="lp Greater Tzaneen Local Municipality" ~	"igxBmx3z9SL"	,
#' community=="lp Lepelle-Nkumpi Local Municipality" ~	"o5VGD8sDmFr"	,
#' community=="lp Maruleng Local Municipality" ~	"Ek0jh44N5so"	,
#' community=="lp Molemole Local Municipality" ~	"tRNrjkKndew"	,
#' community=="lp Polokwane Local Municipality" ~	"skpQVcIzxkE"	,
#' community=="mp Bushbuckridge Local Municipality" ~	"pQcaBXpjH4u"	,
#' community=="mp Chief Albert Luthuli Local Municipality" ~	"Bb6SoYxKBW9"	,
#' community=="mp City of Mbombela Local Municipality" ~	"Ea0tiOAhfQE"	,
#' community=="mp Dipaleseng Local Municipality" ~	"CmgIA6ZasEa"	,
#' community=="mp Dr JS Moroka Local Municipality" ~	"o8O6rw75u3H"	,
#' community=="mp Dr Pixley Ka Isaka Seme Local Municipality" ~	"hobSH0yvAJF"	,
#' community=="mp Emakhazeni Local Municipality" ~"OH11IZTTb7Z"	,
#' community=="mp Emalahleni Local Municipality" ~	"wb9AlnMQ7Ly"	,
#' community=="mp Govan Mbeki Local Municipality" ~	"rWsVQAMbvld"	,
#' community=="mp Lekwa Local Municipality" ~	"PrHEQZZRBZy"	,
#' community=="mp Mkhondo Local Municipality" ~	"B7fgWZECPeK"	,
#' community=="mp Msukaligwa Local Municipality" ~	"tKDzivBHmnD"	,
#' community=="mp Nkomazi Local Municipality" ~	"gUzBGRaXFU4"	,
#' community=="mp Steve Tshwete Local Municipality" ~	"OOLXsiOmElr"	,
#' community=="mp Thaba Chweu Local Municipality" ~	"eFtwFu6uCTv"	,
#' community=="mp Thembisile Hani Local Municipality" ~	"M7k2FwX2pQy"	,
#' community=="mp Victor Khanye Local Municipality" ~	"Jgob3l0wKys"	,
#' community=="nw City of Matlosana Local Municipality" ~	"pJ86GOgGlui"	,
#' community=="nw Ditsobotla Local Municipality" ~	"NVWLiqrB9Px"	,
#' community=="nw JB Marks Local Municipality" ~	"o6waNef1lli"	,
#' community=="nw Kgetlengrivier Local Municipality" ~	"kBV9TLdRSjf"	,
#' community=="nw Madibeng Local Municipality" ~	"rBDCWqb7seg"	,
#' community=="nw Mahikeng Local Municipality" ~	"BmhJTZsfLqG"	,
#' community=="nw Moretele Local Municipality" ~	"wmJR8FCgTft"	,
#' community=="nw Moses Kotane Local Municipality" ~	"NSwQM3Xjw3K"	,
#' community=="nw Ramotshere Moiloa Local Municipality" ~	"Pph7m8P4iYR"	,
#' community=="nw Rustenburg Local Municipality" ~	"MzXMAlDmVYp"	,
#' community=="nw Tswaing Local Municipality" ~	"u12pB1p4FXC"	,
#' community=="wc Cape Town Eastern Health sub-District" ~	"P7uLfoPWa2j"	,
#' community=="wc Cape Town Northern Health sub-District" ~	"Tpye0N2eFz6"	,
#' community=="wc Cape Town Southern Health sub-District" ~	"mzomxZiNvUH"	,
#' community=="wc Cape Town Western Health sub-District" ~	"gnyeVm1kO0r"	,
#' community=="wc Khayelitsha Health sub-District"~	"v9XCremlqAX"	,
#' community=="wc Mitchells Plain Health sub-District" ~"awPTFCVyuH5"	,
#' community=="wc Tygerberg Health sub-District" ~"Qbw8XUMuPDe"	,TRUE~""))
#' 
#' Genie<-list.files(here("Data"),pattern="Genie-SITE")
#' 
#' Genie_FY23<-read_delim(here::here("Data",Genie)) %>% filter(indicator=="OVC_HIVSTAT" | indicator=="OVC_HIVSTAT_NEG" | indicator=="OVC_HIVSTAT_POS" | indicator=="OVC_SERV" | indicator=="OVC_SERV_ACTIVE"| indicator=="OVC_SERV_GRADUATED" |
#' indicator=="OVC_SERV_OVER_18" | indicator=="OVC_SERV_UNDER_18") %>%  mutate(psnushort=psnu,datasource="Genie") %>% mutate(prime_partner_name=case_when(prime_partner_name=="TBD" &funding_agency=="USAID"~"G2G",TRUE~prime_partner_name))
#' 
#' Genie_FY23v1<-Genie_FY23 %>% mutate(partnershort=if_else(mech_code=="80008" |mech_code=="80002","NACOSA" ,prime_partner_name)) %>% rename(age=ageasentered) %>% mutate(period="DATIM QUARTERS") %>% mutate(partnershort=
#' if_else(partnershort=="HIVS","HIVSA",partnershort)) %>%pivot_longer(cols=targets:cumulative,names_to = "valuetype_original",values_to = "value") %>% mutate(value_type=if_else(valuetype_original=="qtr1"| valuetype_original=="qtr2" | valuetype_original=="qtr3" |
#' valuetype_original=="qtr4" | valuetype_original=="cumulative", "Results"    ,"Targets"  )) %>% mutate(yearquarter=case_when(fiscal_year == 2020 & valuetype_original == "qtr1" ~ "FY2020Q1" ,
#' 
#'                                                                                                                             fiscal_year == 2020 & valuetype_original == "qtr2"~"FY2020Q2",
#'                                                                                                                             fiscal_year == 2020 & valuetype_original== "qtr3" ~ "FY2020Q3",
#'                                                                                                                             fiscal_year == 2020 & valuetype_original == "qtr4" ~ "FY2020Q4",
#'                                                                                                                             fiscal_year == 2021 & valuetype_original == "qtr1" ~ "FY2021Q1",
#'                                                                                                                             fiscal_year == 2021 & valuetype_original== "qtr2" ~ "FY2021Q2",
#'                                                                                                                             fiscal_year == 2021 & valuetype_original == "qtr3" ~ "FY2021Q3",
#'                                                                                                                             fiscal_year == 2021 & valuetype_original == "qtr4" ~ "FY2021Q4",
#'                                                                                                                             fiscal_year == 2022 & valuetype_original == "qtr1" ~ "FY2022Q1",
#'                                                                                                                             fiscal_year == 2022 & valuetype_original == "qtr2" ~ "FY2022Q2",
#'                                                                                                                             fiscal_year == 2022 & valuetype_original == "qtr3" ~ "FY2022Q3",
#'                                                                                                                             fiscal_year == 2022 & valuetype_original == "qtr4" ~ "FY2022Q4",
#'                                                                                                                             fiscal_year == 2020 & valuetype_original == "cumulative" ~ "FY2020Annual",
#'                                                                                                                             fiscal_year == 2021 & valuetype_original == "cumulative" ~ "FY2021Annual",
#'                                                                                                                             fiscal_year == 2022 & valuetype_original == "cumulative" ~ "FY2022Annual",
#'                                                                                                                             fiscal_year == 2020 & valuetype_original == "targets" ~ "FY2020Target",
#'                                                                                                                             fiscal_year == 2021 & valuetype_original == "targets" ~ "FY2021Target",
#'                                                                                                                             fiscal_year == 2022 & valuetype_original == "targets" ~ "FY2022Target",
#'                                                                                                                             fiscal_year == 2023 & valuetype_original == "qtr1" ~ "FY2023Q1",
#'                                                                                                                             fiscal_year == 2023 & valuetype_original == "qtr2" ~ "FY2023Q2",
#'                                                                                                                             fiscal_year == 2023 & valuetype_original == "qtr3" ~ "FY2023Q3",
#'                                                                                                                             fiscal_year == 2023 & valuetype_original == "qtr4" ~ "FY2023Q4",
#'                                                                                                                             fiscal_year == 2023 & valuetype_original == "cumulative" ~ "FY2023Annual",
#'                                                                                                                             fiscal_year == 2023 & valuetype_original == "targets" ~ "FY2023Target")) %>%
#'   mutate(result_source=if_else(valuetype_original=="Results" , "Genie","Null")) %>% mutate(mech_code=as.integer(mech_code)) %>% mutate(period=as.character(period)) %>% mutate(fiscal_year=as.character(fiscal_year))
#' 
#' 
#'                                                                                                       
#'                                                                                                       
#'                                                                                                     
#'   final_datset<-bind_rows(OutputTableau5,Genie_FY23v1 )                                                                                               
#'                                                                                                       
#'                                                                                                       
#' #'[OVC Dashboard output file
#' write.xlsx(OutputTableau,"Dataout/OVC_OutputTableau.xlsx",sheetName="1OutputTableau",apppend=T)                                                                                            
#'                                                                                                       
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
