library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(lubridate)
library(glamr)
library(Wavelength)




#read in files --------------------------------------------------------
monthly_misc<-here("Data/misc")


#NDD
ndd_file<-list.files(monthly_misc,pattern="OU5")
ndd_path<-here("Data/misc",ndd_file)

ndd_df<-read_csv(ndd_path) %>% 
  select(OU5uid,OU5name,OU5code)

#NHLS DATIM X-WALK
nhls_file<-list.files(monthly_misc,pattern="facility_mapping")
nhls_path<-here("Data/misc",nhls_file)

nhls_df<-read_csv(nhls_path)


#NDD+NHLS+DATIM
final_df<-nhls_df %>% 
  left_join(ndd_df,by=c("DATIM Facility Name"="OU5name"))


write_csv(final_df,here("Dataout/misc","nhls_datim_ndd_facility_mapping_2022-04-20.csv"),na="")

