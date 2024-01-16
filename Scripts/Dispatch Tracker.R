# Title: Dispatch Tracker
# Author: C. Trapence
# Purpose: Minimize Microsoft power Query issues.
# Date:2023-07-14
# Updated: 2024-01-09
# Updated by Rosaline 
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


if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, here, gargle,glamr,anytime,patchwork,maditr,tidyr, googledrive,googlesheets4,openxlsx,lubridate,readr,readxl, data.table, esquisse, flextable,stringr,sqldf)

load_secrets()
##Step 2: Global variables

#after this step copy and paste into the R profile script and save
#now stored so don"t have to authenticate

#'[Load Data from Individual Partners google sheets for Level one review 

################### RTC (70290)
#'[November 2023 data]
RTC_Dispatch_Nov23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1Q737L6h3fLBOYpOekbm3pAaIE2AH8Num55JHa1o34KE/edit#gid=2074257105"), sheet = "DispatchTracker") %>%janitor::row_to_names(1)  %>%
  mutate(`Nov2023_Facility Dispatch creation date`=as.numeric(as.character(`Nov2023_Facility Dispatch creation date`)),`Nov2023_Dispatch load date`=as.numeric(as.character(`Nov2023_Dispatch load date`)),
 `Nov2023_Facility last backup creation date`=as.numeric(as.character(`Nov2023_Facility last backup creation date`)))  %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ) %>% 
  mutate(missing=if_else(is.na(`Nov2023_Dispatch load date`) | is.na(`Nov2023_Facility Dispatch creation date`)|is.na(`Nov2023_Facility last backup creation date`),"Yes","No"))

RTC_missing_Nov23<-RTC_Dispatch_Nov23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 
write.xlsx(RTC_missing_Nov23,"Dataout/RTC_dispatch_tracker_Nov23.xlsx",sheetName="Dispatch",append=T)

#'[December 2023 data]
RTC_Dispatch_Dec23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1Q737L6h3fLBOYpOekbm3pAaIE2AH8Num55JHa1o34KE/edit#gid=2074257105"), sheet = "DispatchTracker") %>%janitor::row_to_names(1)  %>%
  mutate(`Dec2023_Facility Dispatch creation date`=as.numeric(as.character(`Dec2023_Facility Dispatch creation date`)),`Dec2023_Dispatch load date`=as.numeric(as.character(`Dec2023_Dispatch load date`)),
         `Dec2023_Facility last backup creation date`=as.numeric(as.character(`Dec2023_Facility last backup creation date`)))  %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ) %>% 
  mutate(missing=if_else(is.na(`Dec2023_Dispatch load date`) | is.na(`Dec2023_Facility Dispatch creation date`)|is.na(`Dec2023_Facility last backup creation date`),"Yes","No"))

RTC_missing_Dec23<-RTC_Dispatch_Dec23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(RTC_missing_Dec23,"Dataout/RTC_dispatch_tracker_Dec23.xlsx",sheetName="Dispatch",append=T)


##################  MATCH (81902)
#'[November 2023 data]
MaTCH_Dispatch_Nov23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/18NCnoOpuxYt6TrPnXfmx1HgYn87qKGzYNrARiNoNYD0/edit#gid=2074257105"), sheet = "DispatchTracker") %>% janitor::row_to_names(1) %>% mutate(`Nov2023_Facility Dispatch creation date`=as.numeric(as.character(`Nov2023_Facility Dispatch creation date`)),
        `Nov2023_Dispatch load date`=as.numeric(as.character(`Nov2023_Dispatch load date`)), `Nov2023_Facility last backup creation date`=as.numeric(as.character(`Nov2023_Facility last backup creation date`))) %>% 
         select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ) %>%
         mutate(missing=if_else(is.na(`Nov2023_Dispatch load date`) | is.na(`Nov2023_Facility Dispatch creation date`)|is.na(`Nov2023_Facility last backup creation date`),"Yes","No")) 

MaTCH_missing_Nov23<-MaTCH_Dispatch_Nov23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(MaTCH_missing_Nov23,"Dataout/MaTCH_dispatch_tracker_Nov23.xlsx",sheetName="Dispatch",append=T)

#'[December 2023 data]
MaTCH_Dispatch_Dec23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/18NCnoOpuxYt6TrPnXfmx1HgYn87qKGzYNrARiNoNYD0/edit#gid=2074257105"), sheet = "DispatchTracker") %>% janitor::row_to_names(1) %>% mutate(`Dec2023_Facility Dispatch creation date`=as.numeric(as.character(`Dec2023_Facility Dispatch creation date`)),
                                                                                                                                                                                                                       `Dec2023_Dispatch load date`=as.numeric(as.character(`Dec2023_Dispatch load date`)), `Dec2023_Facility last backup creation date`=as.numeric(as.character(`Dec2023_Facility last backup creation date`))) %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ) %>%
  mutate(missing=if_else(is.na(`Dec2023_Dispatch load date`) | is.na(`Dec2023_Facility Dispatch creation date`)|is.na(`Dec2023_Facility last backup creation date`),"Yes","No")) 

MaTCH_missing_Dec23<-MaTCH_Dispatch_Dec23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(MaTCH_missing_Dec23,"Dataout/MaTCH_dispatch_tracker_Dec23.xlsx",sheetName="Dispatch",append=T)


##################  BRAODREACH (70287)
#'[November 2023 data]

BRCH_Dispatch_Nov23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1eKjFp2RiK25qXTalJN53F_Mcv59ehiOzgffJ9O68RI8/edit#gid=2074257105"), sheet = "DispatchTracker") %>% janitor::row_to_names(1) %>% 
  mutate(`Nov2023_Facility Dispatch creation date`=as.numeric(as.character(`Nov2023_Facility Dispatch creation date`)),`Nov2023_Dispatch load date`=as.numeric(as.character(`Nov2023_Dispatch load date`)),
     `Nov2023_Facility last backup creation date`=as.numeric(as.character(`Nov2023_Facility last backup creation date`))) %>% 
select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Nov2023_Dispatch load date`) | is.na(`Nov2023_Facility Dispatch creation date`)|is.na(`Nov2023_Facility last backup creation date`),"Yes","No")) 

BRCH_missing_Nov23<-BRCH_Dispatch_Nov23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(BRCH_missing_Nov23,"Dataout/BRCH_dispatch_tracker_Nov23.xlsx",sheetName="Dispatch",append=T)

#'[December 2023 data]

BRCH_Dispatch_Dec23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1eKjFp2RiK25qXTalJN53F_Mcv59ehiOzgffJ9O68RI8/edit#gid=2074257105"), sheet = "DispatchTracker") %>% janitor::row_to_names(1) %>% 
  mutate(`Dec2023_Facility Dispatch creation date`=as.numeric(as.character(`Dec2023_Facility Dispatch creation date`)),`Dec2023_Dispatch load date`=as.numeric(as.character(`Dec2023_Dispatch load date`)),
         `Dec2023_Facility last backup creation date`=as.numeric(as.character(`Dec2023_Facility last backup creation date`))) %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Dec2023_Dispatch load date`) | is.na(`Dec2023_Facility Dispatch creation date`)|is.na(`Dec2023_Facility last backup creation date`),"Yes","No")) 

BRCH_missing_Dec23<-BRCH_Dispatch_Dec23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(BRCH_missing_Dec23,"Dataout/BRCH_dispatch_tracker_Dec23.xlsx",sheetName="Dispatch",append=T)

##################  ANOVA_70310
                                         
# ANOVA_Dispatch_70310<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1cC9D-U0CRtYXbQX142s75U4cqAVd1l9OndIVq3fZ7aQ/edit#gid=2074257105"), sheet = "DispatchTracker")
# %>% 
#   mutate(`Oct2023_Facility Dispatch creation date`=as.numeric(as.character(`Oct2023_Facility Dispatch creation date`)),`Oct2023_Dispatch load date`=as.numeric(as.character(`Oct2023_Dispatch load date`)),
#    `Oct2023_Facility last backup creation date`=as.numeric(as.character(`Oct2023_Facility last backup creation date`))) %>% 
# select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Oct2023_Facility Dispatch creation date`,`Oct2023_Dispatch load date`,`Oct2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Oct2023_Dispatch load date`) | is.na(`Oct2023_Dispatch load date`)|is.na(`Oct2023_Facility last backup creation date`),"Yes","No")) 

#'[November 2023 data]
ANOVA_Dispatch_70310_Nov23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1cC9D-U0CRtYXbQX142s75U4cqAVd1l9OndIVq3fZ7aQ/edit#gid=2074257105"), sheet = "DispatchTracker") %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Nov2023_Dispatch load date`) | is.na(`Nov2023_Facility Dispatch creation date`)|is.na(`Nov2023_Facility last backup creation date`),"Yes","No")) 

        #filtered out LP from missing as it is now under new mech 87577
ANOVA_missing_70310_Nov23<-ANOVA_Dispatch_70310_Nov23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ,missing) %>% filter(snu == "gp Gauteng Province", missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(ANOVA_missing_70310_Nov23,"Dataout/ANOVA_dispatch_tracker_70310_Nov23.xlsx",sheetName="Dispatch",append=T)
#janitor::row_to_names(1)

#'[December 2023 data]
ANOVA_Dispatch_70310_Dec23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1cC9D-U0CRtYXbQX142s75U4cqAVd1l9OndIVq3fZ7aQ/edit#gid=2074257105"), sheet = "DispatchTracker") %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Dec2023_Dispatch load date`) | is.na(`Dec2023_Facility Dispatch creation date`)|is.na(`Dec2023_Facility last backup creation date`),"Yes","No")) 

#filtered out LP from missing as it is now under new mech 87577
ANOVA_missing_70310_Dec23<-ANOVA_Dispatch_70310_Dec23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ,missing) %>% filter(snu == "gp Gauteng Province", missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(ANOVA_missing_70310_Dec23,"Dataout/ANOVA_dispatch_tracker_70310_Dec23.xlsx",sheetName="Dispatch",append=T)


###################  ANOVA_87577
#'[November 2023 data]
ANOVA_Dispatch_87577_Nov23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1QrFHYuGlToOoRNZYuJkTWtegWytLfpUSwy4a-kwmRc0/edit#gid=2074257105"), sheet = "DispatchTracker") %>% 
select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Nov2023_Dispatch load date`) | is.na(`Nov2023_Facility Dispatch creation date`)|is.na(`Nov2023_Facility last backup creation date`),"Yes","No")) 

ANOVA_missing_87577_Nov23<-ANOVA_Dispatch_87577_Nov23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(ANOVA_missing_87577_Nov23,"Dataout/ANOVA_dispatch_tracker_87577_Nov23.xlsx",sheetName="Dispatch",append=T)

#'[December 2023 data]
ANOVA_Dispatch_87577_Dec23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1QrFHYuGlToOoRNZYuJkTWtegWytLfpUSwy4a-kwmRc0/edit#gid=2074257105"), sheet = "DispatchTracker") %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ) %>% mutate(missing=if_else(is.na(`Dec2023_Dispatch load date`) | is.na(`Dec2023_Facility Dispatch creation date`)|is.na(`Dec2023_Facility last backup creation date`),"Yes","No")) 

ANOVA_missing_87577_Dec23<-ANOVA_Dispatch_87577_Dec23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(ANOVA_missing_87577_Dec23,"Dataout/ANOVA_dispatch_tracker_87577_Dec23.xlsx",sheetName="Dispatch",append=T)

###################  WHC_70310
#'[November 2023 data]

WHC_Dispatch_Nov23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/10UwrxhAEB_z1r2spIjoSe8H5gkCZnER5RUhGysp4Rjo/edit#gid=2074257105"), sheet = "DispatchTracker") %>%janitor::row_to_names(1)  %>%
  mutate(`Nov2023_Facility Dispatch creation date`=as.numeric(as.character(`Nov2023_Facility Dispatch creation date`)),`Nov2023_Dispatch load date`=as.numeric(as.character(`Nov2023_Dispatch load date`)),
  `Nov2023_Facility last backup creation date`=as.numeric(as.character(`Nov2023_Facility last backup creation date`)))  %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ) %>% 
  mutate(missing=if_else(is.na(`Nov2023_Dispatch load date`) | is.na(`Nov2023_Facility Dispatch creation date`)|is.na(`Nov2023_Facility last backup creation date`),"Yes","No"))

WHC_missing_Nov23<-WHC_Dispatch_Nov23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Nov2023_Facility Dispatch creation date`,`Nov2023_Dispatch load date`,`Nov2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(WHC_missing_Nov23,"Dataout/WHC_dispatch_tracker_Nov23.xlsx",sheetName="Dispatch",append=T)

#'[December 2023 data]

WHC_Dispatch_Dec23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/10UwrxhAEB_z1r2spIjoSe8H5gkCZnER5RUhGysp4Rjo/edit#gid=2074257105"), sheet = "DispatchTracker") %>%janitor::row_to_names(1)  %>%
  mutate(`Dec2023_Facility Dispatch creation date`=as.numeric(as.character(`Dec2023_Facility Dispatch creation date`)),`Dec2023_Dispatch load date`=as.numeric(as.character(`Dec2023_Dispatch load date`)),
         `Dec2023_Facility last backup creation date`=as.numeric(as.character(`Dec2023_Facility last backup creation date`)))  %>% 
  select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ) %>% 
  mutate(missing=if_else(is.na(`Dec2023_Dispatch load date`) | is.na(`Dec2023_Facility Dispatch creation date`)|is.na(`Dec2023_Facility last backup creation date`),"Yes","No"))

WHC_missing_Dec23<-WHC_Dispatch_Dec23 %>% select(operatingunit,fundingagency,partner,mech_code,snu,psnu,`sub-district` ,orgunit,`orgUnit Operation Status`,`Date Closed (If facility is Closed)`,`Dec2023_Facility Dispatch creation date`,`Dec2023_Dispatch load date`,`Dec2023_Facility last backup creation date` ,missing) %>% filter(missing=="Yes",`orgUnit Operation Status`!="Active - Do not submit to NDOH" , `orgUnit Operation Status`!="Closed" ) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 

write.xlsx(WHC_missing_Dec23,"Dataout/WHC_dispatch_tracker_Dec23.xlsx",sheetName="Dispatch",append=T)


