library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(ICPIutilities)
library(lubridate)




#read in weekly files  ---------------------------------------------------------
weekly_extracts<-here("Data/weekly")

hfr_file<-list.files(weekly_extracts,pattern="HFR") 
hfr<-read_excel(here("Data/weekly",hfr_file),
                     sheet="ForDataViz",
                skip=14)

workforce_file<-list.files(weekly_extracts,pattern="Workforce") 
workforce<-read_excel(here("Data/weekly",workforce_file),
                sheet="forDSPviz")


siyenza_file<-list.files(weekly_extracts,pattern="Siyenza") 
siyenza<-read_excel(here("Data/weekly",siyenza_file),
                      sheet="Raw_Data")


#transform ---------------------------------------------------------------------

hfr<-hfr %>% 
  clean_names() %>% 
  rename(age=agecoarse,
         disaggregate=otherdisaggregate,
         community=sub_district,
         value=val) %>% 
  mutate(table="hfr",
         date=case_when(
           indicator=="HTS_TST" ~ floor_date(date,"week",week_start = 5),
           TRUE ~ date
         )) %>% 
  filter(operatingunit=="South Africa",
         date > "2020-12-28")



workforce<-workforce %>% 
  clean_names() %>% 
  rename(psnu=district,
         indicator=data_element,
         value=sum_of_value,
         baseline=sum_of_baseline_normal_operation) %>% 
  mutate(table="workforce",
         mech_code=case_when(
    partner=="Anova Health Institute" ~ "70310",
    partner=="Broadreach" ~ "70287",
    partner=="Maternal, Adolscent and Child Health (MatCH)" ~ "81902",
    partner=="Right To Care, South Africa" ~ "70290",
    partner=="Wits Reproductive Health& HIV Institute" ~ "70301")) %>% 
  filter(date > "2020-12-28")


siyenza<-siyenza %>% 
  clean_names() %>% 
  distinct(orgunituid,siyenza_start_date,siyenza_end_date,siyenzasite) %>% 
  filter(siyenzasite=="Yes")
  

hfr_syzatt<-hfr %>% 
  left_join(siyenza,by="orgunituid") 


#combine -----------------------------------------------------------------------
final_df<-bind_rows(hfr_syzatt,workforce) %>% 
  filter(!is.na(value)) %>% 
  mutate(indicator2=indicator,
         value2=value) %>% 
  spread(indicator2,value2) %>% 
  rename_official() %>% 
  select(-c(partner,mech_name))



write_tsv(final_df,here("Dataout/weekly","2021-07-02_v1.0_weekly_nonmer_data_combined.txt"),na="")

