# Title: Dispatch Tracker
# Author: C. Trapence
# Date:2023-07-14
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

#'[Load Data from Individual Partners google sheets for Level one review 
RTC_Dispatch<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1Q737L6h3fLBOYpOekbm3pAaIE2AH8Num55JHa1o34KE/edit#gid=2074257105"), sheet = "DispatchTracker") %>%janitor::row_to_names(1) %>% mutate(`Jun2023_Facility Dispatch creation date`=as.numeric(as.character(`Jun2023_Facility Dispatch creation date`)),`Jun2023_Dispatch load date`=as.numeric(as.character(`Jun2023_Dispatch load date`)),
                                                                                                                                                                                                                    `Jun2023_Facility last backup creation date`=as.numeric(as.character(`Jun2023_Facility last backup creation date`))) %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Jun2023_Facility Dispatch creation date`,`Jun2023_Dispatch load date`,`Jun2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Jun2023_Dispatch load date`) | is.na(`Jun2023_Dispatch load date`)|is.na(`Jun2023_Facility last backup creation date`),"Yes","No")) 
RTC_missing<-RTC_Dispatch %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Jun2023_Facility Dispatch creation date`,`Jun2023_Dispatch load date`,`Jun2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" & `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(RTC_missing,"Dataout/RTC_dispatch_tracker.xlsx",sheetName="Dispatch",append=T)



MaTCH_Dispatch<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/18NCnoOpuxYt6TrPnXfmx1HgYn87qKGzYNrARiNoNYD0/edit#gid=454279539"), sheet = "DispatchTracker") %>% janitor::row_to_names(1) %>% mutate(`Jun2023_Facility Dispatch creation date`=as.numeric(as.character(`Jun2023_Facility Dispatch creation date`)),`Jun2023_Dispatch load date`=as.numeric(as.character(`Jun2023_Dispatch load date`)),
                                                                                                                                                                                                                     `Jun2023_Facility last backup creation date`=as.numeric(as.character(`Jun2023_Facility last backup creation date`))) %>% 
select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Jun2023_Facility Dispatch creation date`,`Jun2023_Dispatch load date`,`Jun2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Jun2023_Dispatch load date`) | is.na(`Jun2023_Dispatch load date`)|is.na(`Jun2023_Facility last backup creation date`),"Yes","No")) 

MaTCH_missing<-MaTCH_Dispatch %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Jun2023_Facility Dispatch creation date`,`Jun2023_Dispatch load date`,`Jun2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" & `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(MaTCH_missing,"Dataout/MaTCH_dispatch_tracker.xlsx",sheetName="Dispatch",append=T)


BRCH_Dispatch<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1eKjFp2RiK25qXTalJN53F_Mcv59ehiOzgffJ9O68RI8/edit#gid=664161671"), sheet = "DispatchTracker") %>% janitor::row_to_names(1) %>% mutate(`Jun2023_Facility Dispatch creation date`=as.numeric(as.character(`Jun2023_Facility Dispatch creation date`)),`Jun2023_Dispatch load date`=as.numeric(as.character(`Jun2023_Dispatch load date`)),
                                                                                                                                                                                                                      `Jun2023_Facility last backup creation date`=as.numeric(as.character(`Jun2023_Facility last backup creation date`))) %>% 
select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Jun2023_Facility Dispatch creation date`,`Jun2023_Dispatch load date`,`Jun2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Jun2023_Dispatch load date`) | is.na(`Jun2023_Dispatch load date`)|is.na(`Jun2023_Facility last backup creation date`),"Yes","No")) 

BRCH_missing<-BRCH_Dispatch %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Jun2023_Facility Dispatch creation date`,`Jun2023_Dispatch load date`,`Jun2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" & `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(BRCH_missing,"Dataout/BRCH_dispatch_tracker.xlsx",sheetName="Dispatch",append=T)


ANOVA_Dispatch<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1cC9D-U0CRtYXbQX142s75U4cqAVd1l9OndIVq3fZ7aQ/edit#gid=664161671"), sheet = "DispatchTracker") %>% janitor::row_to_names(1) %>% mutate(`Jun2023_Facility Dispatch creation date`=as.numeric(as.character(`Jun2023_Facility Dispatch creation date`)),`Jun2023_Dispatch load date`=as.numeric(as.character(`Jun2023_Dispatch load date`)),
                                                                                                                                                                                                                      `Jun2023_Facility last backup creation date`=as.numeric(as.character(`Jun2023_Facility last backup creation date`))) %>% 
select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Jun2023_Facility Dispatch creation date`,`Jun2023_Dispatch load date`,`Jun2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Jun2023_Dispatch load date`) | is.na(`Jun2023_Dispatch load date`)|is.na(`Jun2023_Facility last backup creation date`),"Yes","No")) 

ANOVA_missing<-ANOVA_Dispatch %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Jun2023_Facility Dispatch creation date`,`Jun2023_Dispatch load date`,`Jun2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" & `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(ANOVA_missing,"Dataout/ANOVA_dispatch_tracker.xlsx",sheetName="Dispatch",append=T)


WRHI_Dispatch<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/10UwrxhAEB_z1r2spIjoSe8H5gkCZnER5RUhGysp4Rjo/edit#gid=664161671"), sheet = "DispatchTracker") %>% janitor::row_to_names(1) %>% mutate(`Jun2023_Facility Dispatch creation date`=as.numeric(as.character(`Jun2023_Facility Dispatch creation date`)),`Jun2023_Dispatch load date`=as.numeric(as.character(`Jun2023_Dispatch load date`)),
                                                                                                                                                                                                                      `Jun2023_Facility last backup creation date`=as.numeric(as.character(`Jun2023_Facility last backup creation date`))) %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Jun2023_Facility Dispatch creation date`,`Jun2023_Dispatch load date`,`Jun2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Jun2023_Dispatch load date`) | is.na(`Jun2023_Dispatch load date`)|is.na(`Jun2023_Facility last backup creation date`),"Yes","No")) 

WRHI_missing<-WRHI_Dispatch %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Jun2023_Facility Dispatch creation date`,`Jun2023_Dispatch load date`,`Jun2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" & `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(WRHI_missing,"Dataout/WRHI_dispatch_tracker.xlsx",sheetName="Dispatch",append=T)

