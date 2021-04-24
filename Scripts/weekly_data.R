library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(ICPIutilities)




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
                      sheet="Raw data")


#transform ---------------------------------------------------------------------

hfr<-hfr %>% 
  clean_names() %>% 
  rename(age=agecoarse,
         disaggregate=otherdisaggregate,
         community=sub_district,
         value=val) %>% 
  mutate(table="hfr") %>% 
  filter(operatingunit=="South Africa")


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
    partner=="Wits Reproductive Health& HIV Institute" ~ "70301"))


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

  

write_tsv(final_df,here("Dataout/weekly","weekly_nonmer_data_combined_2021-04-09_v1.2.txt"),na="")

