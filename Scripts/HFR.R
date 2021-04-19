library(tidyverse)
library(readxl)
library(ICPIutilities)
library(here)
library(glamr)



#GLOBALS -----------------------------------------------------------------------
hfr<-here("non-MER/Data/hfr")
user<-"gsarfaty@usaid.gov"
folder_save<-(here("non-MER/Dataout/hfr"))


WRHI<-"1LCoPZygn1NWRJuDYsaZ2wj2j2zZ5KorT"
Anova<-"1vPT_rSZrIf4ChPbBBtkrk4OPfJQyPLnw"
RTC<-"1fdp8nM60mVK83vlfKbUduXpJmL2rz8za"
BR<-"1GfJMIOhRpGq9t96l34GSGErG1-0Gn2xo"
Match<-"1AJhemB4Vsiq6eFK3OyxQIaHSVYE70P87"
  

#Get data from google sheets ---------------------------------------------------

access_files(user,WRHI,folder_save)
access_files(user,Anova,folder_save)
access_files(user,RTC,folder_save)
access_files(user,BR,folder_save)
access_files(user,Match,folder_save)

#process -----------------------------------------------------------------------

# hfr_files<-list.files(hfr,pattern="High")

Anova_f<-list.files(hfr,pattern = "ANOVA")

Anova_df<-here("non-MER/Data/hfr",Anova_f) %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = here("non-MER/Data/hfr",Anova_f))