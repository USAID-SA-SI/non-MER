# Title: Level Two process
# Author: C. Trapence
# Purpose: Automating the process of Reporting AGYW_PREV for Inter-agency
# Date:2023-07-04
# Updated:2024:01:09
# Updated by Rosaline
#Load Required libraries
# Red text symbolizes comments

#######################################################################################################################
# #  sources files used in the code include:                                                                            #
# #              1)Partners Google sheets                                                                             #
# #                                                                                     #
# #######################################################################################################################
#


if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, here, gargle,glamr,anytime,patchwork,maditr, googledrive,googlesheets4,openxlsx,lubridate,janitor,readr,stringr,sqldf)

load_secrets()
##Step 2: Global variables

#after this step copy and paste into the R profile script and save
#now stored so don"t have to authenticate

Date=(floor_date(Sys.Date() - months(0), "month"))

reporting_period=floor_date(today()-months(2),"month")


columns_FY21<-c("7/31/2021","8/31/2021","9/30/2021" ,"10/31/2021","11/30/2021","12/31/2021","1/31/2022","2/28/2022","3/31/2022","4/30/2022","5/31/2022","6/30/2022","7/31/2022","8/31/2022","9/30/2022","10/31/2022","11/30/2022","12/31/2022", "1/31/2023","2/28/2023","3/31/2023","4/30/2023" ,"5/31/2023","6/30/2023" ,"7/31/2023","8/31/2023","9/30/2023")

columns<-c("7/31/2021","8/31/2021","9/30/2021" ,"10/31/2021","11/30/2021","12/31/2021","1/31/2022","2/28/2022","3/31/2022","4/30/2022","5/31/2022","6/30/2022","7/31/2022","8/31/2022","9/30/2022","10/31/2022","11/30/2022","12/31/2022", "1/31/2023","2/28/2023","3/31/2023","4/30/2023" ,"5/31/2023","6/30/2023" ,"7/31/2023","8/31/2023","9/30/2023","10/31/2023", "11/30/2023", "12/31/2023","1/31/2024","2/29/2024", "3/31/2024","4/30/2024", "5/31/2024","6/30/2024","7/31/2024", "8/31/2024", "9/30/2024")
columns_FY22<-c("7/31/2021","8/31/2021","9/30/2021" ,"10/31/2021","11/30/2021","12/31/2021","1/31/2022","2/28/2022","3/31/2022","4/30/2022","5/31/2022","6/30/2022","7/31/2022","8/31/2022","9/30/2022" )
columns_FY24<- c("10/31/2023","11/30/2023" ,"12/31/2023","1/31/2024" ,"2/29/2024" , "3/31/2024" ,"4/30/2024","5/31/2024","6/30/2024","7/31/2024",
                "8/31/2024","9/30/2024","10/31/2024","11/30/2024","12/31/2024","1/31/2025","2/28/2025","3/31/2025","4/30/2025","5/31/2025", "6/30/2025","7/31/2025","8/31/2025",
                "9/30/2025","10/31/2025" , "11/30/2025", "12/31/2025")

columns_NC<- c("11/30/2023" ,"12/31/2023","1/31/2024" ,"2/29/2024" , "3/31/2024" ,"4/30/2024","5/31/2024","6/30/2024","7/31/2024",
                 "8/31/2024","9/30/2024","10/31/2024","11/30/2024","12/31/2024","1/31/2025","2/28/2025","3/31/2025","4/30/2025","5/31/2025", "6/30/2025","7/31/2025","8/31/2025",
                 "9/30/2025","10/31/2025" , "11/30/2025", "12/31/2025")

#'[Load Data from Individual Partners Google sheets for Level one review]

### HIVSA

## Reading historical data
HIVSA_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1-EDzs3Wxf-T6r6SJ_AepsqDbln-LAM8fwy9IPhLvd8g/edit#gid=147779424"), sheet = "OVC Indicators") %>%
janitor::row_to_names(1) %>% mutate(mech_code = as.character(mech_code)) %>%
rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,

"12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,
"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
"1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,
"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` )  %>%
mutate_if(is.list,as.character)%>% mutate(mech_code = as.character(mech_code))%>% mutate(timer=1)

HIVSA_FY23[columns_FY21]<-sapply(HIVSA_FY23[columns_FY21],as.integer)

## Reading data for current reporting period FY_2024
HIVSA_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1-EDzs3Wxf-T6r6SJ_AepsqDbln-LAM8fwy9IPhLvd8g/edit#gid=147779424"), sheet = "FY24_Reporting_Tool") %>% 
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=2)

## Consolidating HIVSA's historical and FY2024 data
HIVSA<-bind_rows(HIVSA_FY24,HIVSA_FY23)

### FHI 360

## Reading historical data. FHI 360 last reported in September 2023. No  reporting for FY_2024

FHI360<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1KLFnNV5szfw0Ns1Q4kjV5J2x7qp4biYTeDwU4YMdJlM/edit?usp=drive_link"), sheet = "OVC Indicators") %>%
            janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
            "12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
            "1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` )  %>% mutate_if(is.list,as.character)%>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=1)


FHI360[columns_FY21]<-sapply(FHI360[columns_FY21],as.integer)

### PACT Inc

## Reading historical data.

PACT_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1G8xIspdRzFswVk59WvYjVK22k7_Pi9EqrSp2uvdA-HA/edit#gid=0"), sheet = "OVC Indicators") %>%
           janitor::row_to_names(1) %>%  rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
            "12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
            "1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` )  %>%
  mutate_if(is.list,as.character)%>% mutate(timer=1)

PACT_FY23[columns_FY21]<-sapply(PACT_FY23[columns_FY21],as.integer)

## Reading data for current reporting period FY2024. PACT indicated on email they will not be reporting on OVC indicators for Oct 2023

PACT_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1ae6N27LozI1q2iv4oQVLIoGzBmE72r7foVvHjQ0KjFQ/edit#gid=2041408209"),sheet = "FY24_Reporting_Tool")%>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=2)


PACT_FY24[columns_FY24]<-sapply(PACT_FY24[columns_FY24],as.integer)

## Consolidating PACT's historical and FY2024 data
PACT<-bind_rows(PACT_FY23,PACT_FY24)


### Mothers to Mothers(M2M)

## Reading historical data
M2M_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1BLl3QcQyZJulAkxDWONJRXOcvMDEOPtmHbbtHLGCMXA/edit#gid=329992892"), sheet = "OVC Indicators") %>%
          janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
          "12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
          "1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` ) %>%
  mutate_if(is.list,as.character)%>% mutate(timer=1)

M2M_FY23[columns_FY21]<-sapply(M2M_FY23[columns_FY21],as.integer)

## Reading data for current reporting period FY_2024Q1
M2M_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1BLl3QcQyZJulAkxDWONJRXOcvMDEOPtmHbbtHLGCMXA/edit#gid=329992892"), sheet = "FY24_Reporting_Tool")%>% 
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=2)


M2M_FY24[columns_FY24]<-sapply(M2M_FY24[columns_FY24],as.integer)
## Consolidating M2M's historical and FY2024 data
M2M<-bind_rows(M2M_FY23,M2M_FY24)

### CINDI

## Reading historical data.

CINDI_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/11v2uMvKG2WSsOeKy_KFQEssE5dznkT3-TYzk88gvpuw/edit#gid=0"), sheet = "OVC Indicators") %>%
           janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
          "12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
         "1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` )  %>% mutate_if(is.list,as.character) %>% mutate(timer=1)

CINDI_FY23[columns_FY21]<-sapply(CINDI_FY23[columns_FY21],as.integer) 

## Reading data for current reporting period FY_2024

CINDI_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/11v2uMvKG2WSsOeKy_KFQEssE5dznkT3-TYzk88gvpuw/edit#gid=0"), sheet = "FY24_Reporting_Tool")%>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=2)


CINDI_FY24[columns_FY24]<-sapply(CINDI_FY24[columns_FY24],as.integer)
## Consolidating CINDI's historical and FY2024 data
CINDI<-bind_rows(CINDI_FY23,CINDI_FY24)

### G2G

## Reading historical data.
G2G_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/18sQPukk1KQhyGNogJDbjvcGJRBOJQYzAN7pdwmbfixU/edit#gid=329992892"), sheet = "OVC Indicators") %>%
          janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
          "12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000`,"10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
          "1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` )  %>%
  mutate_if(is.list,as.character)%>% mutate(timer=1)

G2G_FY23[columns_FY21]<-sapply(G2G_FY23[columns_FY21],as.integer)
## Reading data for current reporting period FY_2024
G2G_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/18sQPukk1KQhyGNogJDbjvcGJRBOJQYzAN7pdwmbfixU/edit#gid=351803566"), sheet = "FY24_Reporting_Tool")%>%
  mutate(mech_code = as.character(mech_code)) %>% mutate(timer=2)

G2G_FY24[columns_FY24]<-sapply(G2G_FY24[columns_FY24],as.integer)
## Consolidating G2G's historical and data and FY2024 data
G2G<-bind_rows(G2G_FY23,G2G_FY24)


### NACOSA

## Reading historical data.
NACOSA_FY22<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1ePof8YITyIUiZn7FfKZeaUje5PR57MbLig95rSMeK-M/edit#gid=329992892"), sheet = "OVC Indicators") %>%
          janitor::row_to_names(1) %>% rename("7/31/2021"= `1627689600`,"8/31/2021"= `1630368000`,"9/30/2021"= `1632960000` ,"10/31/2021"= `1635638400`,"11/30/2021"= `1638230400`,
         "12/31/2021"= `1640908800`,"1/31/2022"= `1643587200`,"2/28/2022"= `1646006400`,"3/31/2022"= `1648684800`,"4/30/2022"= `1651276800`,"5/31/2022"= `1653955200`,"6/30/2022"= `1656547200`,"7/31/2022"= `1659225600`,"8/31/2022"= `1661904000`,"9/30/2022"= `1664496000` ) %>%
         select( -(`1667174400`:`1696032000`))   %>% mutate(mech_code=unlist(mech_code)) %>% mutate_if(is.list,as.character) %>% 
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=0)

NACOSA_FY22[columns_FY22]<-sapply(NACOSA_FY22[columns_FY22],as.integer)

NACOSA_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1pbH6R54ioOjf-9rz12TNLJLqpx_9j-6CDQaSi3VDpIo/edit#gid=329992892"), sheet = "OVC Indicators") %>%
          janitor::row_to_names(1) %>% rename("10/31/2022"= `1667174400`,"11/30/2022"= `1669766400`,"12/31/2022"= `1672444800`,
           "1/31/2023"= `1675123200`,"2/28/2023"= `1677542400`,"3/31/2023"= `1680220800`,"4/30/2023"= `1682812800` ,"5/31/2023"= `1685491200`,"6/30/2023"= `1688083200` ,"7/31/2023"= `1690761600`,"8/31/2023"= `1693440000`,"9/30/2023"= `1696032000` ) %>% mutate(mech_code=unlist(mech_code))  %>% mutate_if(is.list,as.integer) %>%
          select(mech_code:indicator_status,`10/31/2022`:`9/30/2023`) %>%  
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=1)

NACOSA_FY22[columns_FY22]<-sapply(NACOSA_FY22[columns_FY22],as.integer)

## Reading data for current reporting period FY2024
NACOSA_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1pbH6R54ioOjf-9rz12TNLJLqpx_9j-6CDQaSi3VDpIo/edit#gid=329992892"), sheet = "FY24_Reporting_Tool") %>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=2)


NACOSA_FY24[columns_FY24]<-sapply(NACOSA_FY24[columns_FY24],as.integer)

## Consolidating NACOSA's historical and FY2024 data
NACOSA<-bind_rows(NACOSA_FY22,NACOSA_FY23,NACOSA_FY24)

####################Consolidating data from all partners (historical and FY_2024Q1 data)#########

#AllData<-bind_rows(NACOSA,CINDI,HIVSA,FHI360,G2G,M2M,PACT) %>% select(-(`11/30/2024`:`12/31/2025`)) %>% select(-(`7/31/2021`:`9/30/2022`))

#---------- FY24 Q1 data only
AllData <- bind_rows(HIVSA_FY24,PACT_FY24,M2M_FY24, CINDI_FY24, G2G_FY24,NACOSA_FY24) %>% select(-(`1/31/2024`:`12/31/2025`))

AllDatav1<-AllData  %>% select(-(timer) ) %>% 
  pivot_longer(cols= 10:11, values_to ="Value" ,names_to = "period") %>%
  #filter(!is.na(psnu),indicator_status=="Active") %>%
  filter(!is.na(psnu)) %>%
  group_by_if(is_character) %>% summarise(value=sum(Value))

OutputTableau <-AllDatav1 %>%  mutate(period=mdy(period))%>%
  mutate(community_status="",last_refreshed=today(),End_Date=period,Start_Date=period,period_type="Monthly Cummulative within Quarter") %>%
  group_by( mech_code ,primepartner, psnu,community,indicator ,last_refreshed,disaggregate ,age,otherdisaggregate , community_status, Start_Date,period,period_type ) %>%
  summarise(value=sum(value))%>% mutate(missing=if_else( is.na(value),"Yes","No"))%>%
  select(mech_code ,primepartner, psnu,community,indicator,value ,Start_Date,period,last_refreshed,missing,period_type,age,disaggregate ,otherdisaggregate , community_status) %>%
  mutate(datasource="GoogleDrive",valuetype="Results")  %>%
  mutate(partnershort=if_else(mech_code == "80008"  | mech_code== "80002","NACOSA",primepartner)) 

# Preparing data for DQRT Level 1 and Level 2 checks
DQRT_temp1<-OutputTableau %>%
  group_by( mech_code ,primepartner, psnu,community,indicator ,last_refreshed,disaggregate ,age,otherdisaggregate , community_status, Start_Date,period,period_type  ,missing ) %>%
  summarise(value=sum(value))

DQRT_temp2<-DQRT_temp1  %>% filter(age=="<18" ) %>% dplyr::group_by(primepartner,mech_code,psnu,community,age,period,indicator) %>%
  dplyr::summarise(value=sum(value))

#Level Two checks flags

level2<-maditr::dcast(setDT(DQRT_temp2),... ~ indicator,value.var = "value") %>%
  mutate(OVC_HIVSTAT=OVC_HIVSTAT_Negative  +`OVC_HIVSTAT_Positive_Not Receiving ART`+`OVC_HIVSTAT_Positive_Receiving ART`+
           `OVC_HIVSTAT_Test Not Required`+`OVC_HIVSTAT_Unknown_No HIV Status`)%>%
  select( primepartner,mech_code,psnu,community,period,age,OVC_HIVSTAT_Negative :OVC_HIVSTAT )%>%
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

#'[HIVSA Partner feedback]

#Missing data
Missing_data_HIVSA<-OutputTableau %>%  filter(missing=="Yes") %>% filter(period<Date & period>= reporting_period ) %>%
  mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
  filter(mech_code=="70307")

#Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
check1_HIVSA<-level2 %>% mutate(check1 = OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>%
  select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>%
  mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")

# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check2_HIVSA<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>%
  mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%
  filter(mech_code=="70307")
#Check 3:OVC VLS>OVC_VLR
check3_HIVSA<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>%
  select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>%
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")

#Check 4:HIVSTAT More that OVC COMPREHENSIVE
check4_HIVSA<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>%
  mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")

#[Final import output below]


#Creating workbook with separate worksheets to document the different L1 and L2 checks
wb <- createWorkbook()
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
rm(HIVSA,HIVSA_FY23,HIVSA_FY24,check1_HIVSA,check2_HIVSA,check3_HIVSA,check4_HIVSA,Missing_data_HIVSA)

#'[PACT Partner feedback]

# Checking for missing data
Missing_data_PACT<-OutputTableau %>% filter(missing=="Yes"  ) %>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
  filter(mech_code=="86130")


#Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
check1_PACT<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>%
  mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="86130")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18

#Check 2:HIVSTAT More that OVC COMPREHENSIVE
check2_PACT<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>%
  mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%
  filter(mech_code=="86130")
#Check 3:OVC VLS>OVC_VLR
check3_PACT<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>%
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="86130")

#Check 4:HIVSTAT More that OVC COMPREHENSIVE
check4_PACT<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="86130")

#Creating workbook with separate worksheets to document the different L1 and L2 checks
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
rm(check1_PACT,check2_PACT,check3_PACT,check4_PACT,Missing_data_PACT,PACT,PACT_FY23,PACT_FY24)
#'[PACT END]


#'[G2G Partner feedback]

# Checking for missing data
Missing_data_G2G<-OutputTableau %>% filter(missing=="Yes")%>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
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

#Check 4:HIVSTAT More that OVC COMPREHENSIVE
check4_G2G<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="81904")

#Creating workbook with separate worksheets to document the different L1 and L2 checks
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
rm(G2G,G2G_FY23,G2G_FY24,check1_G2G,check2_G2G,check3_G2G,check4_G2G,Missing_data_G2G)
#'[G2G END]


#'[M2M Partner feedback]

# Checking for missing data
Missing_data_M2M <-OutputTableau %>% filter(missing=="Yes") %>%    mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
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

#Check 4:HIVSTAT More that OVC COMPREHENSIVE
check4_M2M<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80004")

#Creating workbook with separate worksheets to document the different L1 and L2 checks
write.xlsx(check1_M2M,"Dataout/OVC_DQRT_Feedback_M2M.xlsx",  sheetName="Check1",append=TRUE)

wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_M2M.xlsx")

addWorksheet(wb,"check2")
writeData(wb,sheet="check2",x=check2_M2M)

addWorksheet(wb,"check3")
writeData(wb,sheet="check3",x=check3_M2M)

addWorksheet(wb,"check4")
writeData(wb,sheet="check4",x=check4_M2M)

addWorksheet(wb,sheetName = "Missing_Data")
writeData(wb,sheet = "Missing_Data",x=Missing_data_M2M)

saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_M2M.xlsx",overwrite = T)

rm(M2M,M2M_FY23,M2M_FY24, check1_M2M,check2_M2M,check3_M2M,check4_M2M,Missing_data_M2M)
#'[M2M END]


#'[CINDI Partner feedback]

#Checking for missing data
Missing_data_CINDI<-OutputTableau %>%filter(missing=="Yes")%>% mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
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

#Check 4:HIVSTAT More that OVC COMPREHENSIVE
check4_CINDI<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70311")

#Creating workbook with separate worksheets to document the different L1 and L2 checks
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

rm(CINDI,CINDI_FY23, CINDI_FY24,check1_CINDI,check2_CINDI,check3_CINDI,check4_CINDI, Missing_data_CINDI)
#'[CINDI END]


#'[NACOSA Partner feedback]
# Checking for missing data
Missing_data_NACOSA<-OutputTableau %>% filter(missing=="Yes") %>% mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
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

#Check 4:HIVSTAT More that OVC COMPREHENSIVE
check4_NACOSA<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80008")

#Creating workbook with separate worksheets to document the different L1 and L2 checks
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
rm(NACOSA,NACOSA_FY22,NACOSA_FY23,NACOSA_FY24, check1_NACOSA,check2_NACOSA, check3_NACOSA, check4_NACOSA, Missing_data_NACOSA)

#'[OVC Dashboard output file]
#'
print(distinct(AllDatav1,primepartner,mech_code ))


OutputTableau <- OutputTableau %>% mutate( mech_code=case_when(mech_code=="80002" | mech_code=="80008 "~"80008",
                                                               mech_code=="14631"   |mech_code=="86130"~"86130",.default =  mech_code    ))


write.xlsx(OutputTableau,"Dataout/OVC_OutputTableauv3.xlsx",sheetName="1OutputTableau",apppend=T)

