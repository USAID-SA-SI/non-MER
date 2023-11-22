# Title: Dispatch Tracker
# Author: C. Trapence
# Purpose: Minimize Microsoft power Query issues.
# Date:2023-07-14
# Updated: 2023-11-16
# Updated by Rosaline and Clement
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

#setwd("C:/Users/ctrapence/Documents")
setwd("C:/Users/rpineteh/Documents")


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


## Change period here 
#period<-"Oct2023"

#after this step copy and paste into the R profile script and save
#now stored so don"t have to authenticate

#'[Load Data from Individual Partners google sheets for Level one review 

################### RTC (70290)
RTC_Dispatch<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1Q737L6h3fLBOYpOekbm3pAaIE2AH8Num55JHa1o34KE/edit#gid=2074257105"), sheet = "DispatchTracker") %>%janitor::row_to_names(1)  %>%
  mutate(`Oct2023_Facility Dispatch creation date`=as.numeric(as.character(`Oct2023_Facility Dispatch creation date`)),`Oct2023_Dispatch load date`=as.numeric(as.character(`Oct2023_Dispatch load date`)),
 `Oct2023_Facility last backup creation date`=as.numeric(as.character(`Oct2023_Facility last backup creation date`)))  %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ) %>% 
  mutate(missing=if_else(is.na(`Oct2023_Dispatch load date`) | is.na(`Oct2023_Facility Dispatch creation date`)|is.na(`Oct2023_Facility last backup creation date`),"Yes","No"))

RTC_missing<-RTC_Dispatch %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(RTC_missing,"Dataout/RTC_dispatch_tracker.xlsx",sheetName="Dispatch",append=T)

##################  MATCH (81902)
MaTCH_Dispatch<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/18NCnoOpuxYt6TrPnXfmx1HgYn87qKGzYNrARiNoNYD0/edit#gid=2074257105"), sheet = "DispatchTracker") %>% janitor::row_to_names(1) %>% mutate(`Oct2023_Facility Dispatch creation date`=as.numeric(as.character(`Oct2023_Facility Dispatch creation date`)),
        `Oct2023_Dispatch load date`=as.numeric(as.character(`Oct2023_Dispatch load date`)), `Oct2023_Facility last backup creation date`=as.numeric(as.character(`Oct2023_Facility last backup creation date`))) %>% 
         select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ) %>%
         mutate(missing=if_else(is.na(`Oct2023_Dispatch load date`) | is.na(`Oct2023_Facility Dispatch creation date`)|is.na(`Oct2023_Facility last backup creation date`),"Yes","No")) 

MaTCH_missing<-MaTCH_Dispatch %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(MaTCH_missing,"Dataout/MaTCH_dispatch_tracker.xlsx",sheetName="Dispatch",append=T)

##################  BRAODREACH (70287)
                                        
BRCH_Dispatch<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1eKjFp2RiK25qXTalJN53F_Mcv59ehiOzgffJ9O68RI8/edit#gid=2074257105"), sheet = "DispatchTracker") %>% janitor::row_to_names(1) %>% 
  mutate(`Oct2023_Facility Dispatch creation date`=as.numeric(as.character(`Oct2023_Facility Dispatch creation date`)),`Oct2023_Dispatch load date`=as.numeric(as.character(`Oct2023_Dispatch load date`)),
     `Oct2023_Facility last backup creation date`=as.numeric(as.character(`Oct2023_Facility last backup creation date`))) %>% 
select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Oct2023_Dispatch load date`) | is.na(`Oct2023_Facility Dispatch creation date`)|is.na(`Oct2023_Facility last backup creation date`),"Yes","No")) 

BRCH_missing<-BRCH_Dispatch %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(BRCH_missing,"Dataout/BRCH_dispatch_tracker.xlsx",sheetName="Dispatch",append=T)

##################  ANOVA_70310
                                         
# ANOVA_Dispatch_70310<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1cC9D-U0CRtYXbQX142s75U4cqAVd1l9OndIVq3fZ7aQ/edit#gid=2074257105"), sheet = "DispatchTracker")
# %>% 
#   mutate(`Oct2023_Facility Dispatch creation date`=as.numeric(as.character(`Oct2023_Facility Dispatch creation date`)),`Oct2023_Dispatch load date`=as.numeric(as.character(`Oct2023_Dispatch load date`)),
#    `Oct2023_Facility last backup creation date`=as.numeric(as.character(`Oct2023_Facility last backup creation date`))) %>% 
# select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Oct2023_Dispatch load date`) | is.na(`Oct2023_Dispatch load date`)|is.na(`Oct2023_Facility last backup creation date`),"Yes","No")) 

ANOVA_Dispatch_70310<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1cC9D-U0CRtYXbQX142s75U4cqAVd1l9OndIVq3fZ7aQ/edit#gid=2074257105"), sheet = "DispatchTracker") %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Oct2023_Dispatch load date`) | is.na(`Oct2023_Facility Dispatch creation date`)|is.na(`Oct2023_Facility last backup creation date`),"Yes","No")) 

        #filtered out LP from missing as it is now under new mech 87577
ANOVA_missing_70310<-ANOVA_Dispatch_70310 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ,missing) %>% filter(snu == "gp Gauteng Province", missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(ANOVA_missing_70310,"Dataout/ANOVA_dispatch_tracker_70310.xlsx",sheetName="Dispatch",append=T)
#janitor::row_to_names(1)

###################  ANOVA_87577

ANOVA_Dispatch_87577<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1QrFHYuGlToOoRNZYuJkTWtegWytLfpUSwy4a-kwmRc0/edit#gid=2074257105"), sheet = "DispatchTracker") %>% 
select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Oct2023_Dispatch load date`) | is.na(`Oct2023_Facility Dispatch creation date`)|is.na(`Oct2023_Facility last backup creation date`),"Yes","No")) 

ANOVA_missing_87577<-ANOVA_Dispatch_87577 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(ANOVA_missing_87577,"Dataout/ANOVA_dispatch_tracker_87577.xlsx",sheetName="Dispatch",append=T)

###################  WHC_70310
WHC_Dispatch<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/10UwrxhAEB_z1r2spIjoSe8H5gkCZnER5RUhGysp4Rjo/edit#gid=2074257105"), sheet = "DispatchTracker") %>%janitor::row_to_names(1)  %>%
  mutate(`Oct2023_Facility Dispatch creation date`=as.numeric(as.character(`Oct2023_Facility Dispatch creation date`)),`Oct2023_Dispatch load date`=as.numeric(as.character(`Oct2023_Dispatch load date`)),
  `Oct2023_Facility last backup creation date`=as.numeric(as.character(`Oct2023_Facility last backup creation date`)))  %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ) %>% 
  mutate(missing=if_else(is.na(`Oct2023_Dispatch load date`) | is.na(`Oct2023_Facility Dispatch creation date`)|is.na(`Oct2023_Facility last backup creation date`),"Yes","No"))

WHC_missing<-RTC_Dispatch %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(WHC_missing,"Dataout/WHC_dispatch_tracker.xlsx",sheetName="Dispatch",append=T)

