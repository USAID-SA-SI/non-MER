library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(lubridate)
library(ICPIutilities)



#read in dsp viz tables --------------------------------------------------------
monthly_extracts<-here("Data/monthly")


hfr_file<-list.files(monthly_extracts,pattern="HFR") 
hfr<-read_excel(here("Data/monthly",hfr_file),
                     sheet="ForDataViz",
                skip=14)

# workforce_file<-list.files(monthly_extracts,pattern="Workforce") # NEEEEEED UPDATE ########
# workforce<-read_excel(here("Data/monthly",workforce_file),
#                 sheet="forDSPviz")


siyenza_file<-list.files(monthly_extracts,pattern="Siyenza") 
siyenza<-read_excel(here("Data/monthly",siyenza_file),
                      sheet="Raw data")


core_file<-list.files(monthly_extracts,pattern="Core") 
core<-read_excel(here("Data/monthly",core_file),
                  sheet="Full_Data")


covid19_file<-list.files(monthly_extracts,pattern="COVID") 
covid<-read_excel(here("Data/monthly",covid19_file),
                      sheet="ForDataViz")



decanting_file<-list.files(monthly_extracts,pattern="Decanting") 
decanting<-read_excel(here("Data/monthly",decanting_file),
                      sheet="ForDataViz")


hivss_file<-list.files(monthly_extracts,pattern="HIVSS") 
hivss<-read_excel(here("Data/monthly",hivss_file),
                  sheet="ForDataViz")
                  
                  
index_file<-list.files(monthly_extracts,pattern="Index") 
index<-read_excel(here("Data/monthly",index_file),
                sheet="ForDataViz")

tld_file<-list.files(monthly_extracts,pattern="TLD") 
tld<-read_excel(here("Data/monthly",tld_file),
                    sheet="ForDataViz")


ipc_file<-list.files(monthly_extracts,pattern="IPC") 
ipc<-read_excel(here("Data/monthly",ipc_file),
                sheet="ForDataViz")

#transform weekly to monthly----------------------------------------------------

hfr_snapshot<-hfr %>% 
  clean_names() %>% 
  filter(indicator %in% c("cLTFU","Headcount","LATEMISSED","EARLYMISSED","TX_CURR_28",
                          "TX_CURR_90","uLTFU")) %>% 
  rename(age=agecoarse,
         disaggregate=otherdisaggregate,
         snu1=snu,
         community=sub_district,
         value=val) %>% 
  mutate(table="hfr",
         mon_yr= format(date, "%Y-%m")) %>% 
  group_by(mon_yr,orgunit,mech_code,indicator) %>% 
  filter(date==max(date)) %>% 
  ungroup() 


hfr_cumulative<-hfr %>% 
  clean_names() %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW")) %>% 
  rename(age=agecoarse,
         disaggregate=otherdisaggregate,
         snu1=snu,
         community=sub_district,
         value=val) %>% 
  mutate(table="hfr",
         mon_yr= format(date, "%Y-%m")) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  ungroup()


hfr_combined<-bind_rows(hfr_cumulative,hfr_snapshot) %>% 
  select(-date)

rm(hfr,hfr_cumulative,hfr_snapshot)

siyenza<-siyenza %>% 
  clean_names() %>% 
  distinct(orgunituid,siyenza_start_date,siyenza_end_date,siyenzasite) %>% 
  filter(siyenzasite=="Yes")
  

hfr_syzatt<-hfr_combined%>% 
  left_join(siyenza,by="orgunituid") 


sitelist<-hfr_syzatt %>% 
  select(orgunit,orgunituid) %>% 
  distinct(orgunit,orgunituid)


index<-index%>% 
  clean_names() %>% 
  rename(snu1=province,
         psnu=district,
         mech_code=mechanismid) %>% 
  mutate(table="index",
         mon_yr= format(date, "%Y-%m")) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  ungroup()


# workforce<-workforce %>% #CHANGE TO AVG # IN THE MONTH!!!########
#   clean_names() %>% 
#   rename(psnu=district,
#          indicator=data_element,
#          value=sum_of_value,
#          baseline=sum_of_baseline_normal_operation) %>% 
#   mutate(table="workforce")

# transform monthly -------------------------------------------------------------

# core<-core %>% ### NEEEEEED TO COME BACK HERE #####################
#   clean_names()


covid<-covid%>% 
  clean_names() %>% 
  rename(psnu=district,
         indicator=data_element) %>% 
  mutate(table="covid-19",
         mech_code=case_when(
           partner=="ANOVA" ~ "70310",
           partner=="Broadreach" ~ "70287",
           partner=="Maternal, Adolscent and Child Health (MatCH)" ~ "81902",
           partner=="RIGHT TO CARE" ~ "70290",
           partner=="WITS HEALTH CONSORTIUM (PTY) LTD" ~ "70301"),
         snu1=case_when(
           str_starts(psnu, "gp") ~ "gp Gauteng Province",
           str_starts(psnu, "lp") ~ "lp Limpopo Province",
           str_starts(psnu, "kz") ~ "kz KwaZulu-Natal Province",
           str_starts(psnu, "mp") ~ "mp Mpumalanga Province",
           str_starts(psnu, "ec") ~ "ec Eastern Cape Province",
           str_starts(psnu, "fs") ~ "fs Free State Province",
           str_starts(psnu, "wc") ~ "wc Westerm Cape Province")
         )


decanting<-decanting%>% 
  clean_names() %>% 
  rename(snu1=province,
         psnu=district,
         community=subdistrict,
         orgunit=facility,
         orgunituid=facilityuid,
         mech_code=mechanismid) %>% 
  mutate(table="decanting")


## fix period to be date format
hivss<-hivss%>% 
  clean_names() %>% 
  rename(snu1=snu,
         date=period,
         community=sub_district,
         value=sum_of_value) %>% 
  mutate(table="hivss",
         date=mdy(date))


tld<-tld %>% 
  clean_names() %>% 
  rename(snu1=snu,
         community=subdistrict,
         age=agecoarse) %>% 
  mutate(table="tld")


ipc<-ipc %>% 
  clean_names() %>% 
  rename(snu1=snu,
         community=sub_district) %>% 
  mutate(table="ipc") %>% 
  left_join(sitelist,by="orgunit")


monthly<-bind_rows(covid,decanting,hivss,tld,ipc) %>% 
  mutate(mon_yr= format(date, "%Y-%m")) %>% 
  select(-date)

#combine -----------------------------------------------------------------------
final_df<-bind_rows(hfr_syzatt,monthly,index) %>% 
  filter(!is.na(value)) %>% 
  rename(facility=orgunit,
         facilityuid=orgunituid) %>%
  mutate(indicator2=indicator,
         value2=value) %>%
  spread(indicator2,value2) %>%
  rename_official() %>%
  select(-c(partner,mech_name))



write_tsv(final_df,here("Dataout/monthly","monthly_nonmer_data_combined_2021-04-09_v1.0.txt"),na="")

