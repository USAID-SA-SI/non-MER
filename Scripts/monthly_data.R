library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(lubridate)



#read in dsp viz tables --------------------------------------------------------
monthly_extracts<-here("Data/monthly")


hfr_file<-list.files(monthly_extracts,pattern="HFR") 
hfr<-read_excel(here("Data/monthly",hfr_file),
                     sheet="ForDataViz",
                skip=14)

workforce_file<-list.files(monthly_extracts,pattern="Workforce") 
workforce<-read_excel(here("Data/monthly",workforce_file),
                sheet="forDSPviz")


siyenza_file<-list.files(monthly_extracts,pattern="Siyenza") 
siyenza<-read_excel(here("Data/monthly",siyenza_file),
                      sheet="Raw data")


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
         community=sub_district,
         value=val) %>% 
  mutate(table="hfr",
         mon_yr= format(date, "%Y-%m")) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE)


hfr_combined<-bind_rows(hfr_cumulative,hfr_snapshot) %>% 
  select(-date)

rm(hfr,hfr_cumulative,hfr_snapshot)

siyenza<-siyenza %>% 
  clean_names() %>% 
  distinct(orgunituid,siyenza_start_date,siyenza_end_date,siyenzasite) %>% 
  filter(siyenzasite=="Yes")
  

hfr_syzatt<-hfr_combined%>% 
  left_join(siyenza,by="orgunituid") 


workforce<-workforce %>% #CHANGE TO AVG # IN THE MONTH!!!########
  clean_names() %>% 
  rename(psnu=district,
         indicator=data_element,
         value=sum_of_value,
         baseline=sum_of_baseline_normal_operation) %>% 
  mutate(table="workforce")

# transform monthly -------------------------------------------------------------
covid<-covid%>% 
  clean_names() %>% 
  rename(psnu=district) %>% 
  mutate(table="covid-19")


decanting<-decanting%>% 
  clean_names() %>% 
  rename(psnu=district,
         community=subdistrict) %>% 
  mutate(table="decanting")


hivss<-hivss%>% 
  clean_names() %>% 
  rename(snu1=snu,
         date=period) %>% 
  mutate(table="hivss",
         date=as.date(date))


index<-index%>% 
  clean_names() %>% 
  rename(snu1=province,
         psnu=district) %>% 
  mutate(table="index")


tld<-tld %>% 
  clean_names() %>% 
  rename(snu1=snu,
         community=subdistrict) %>% 
  mutate(table="tld")


ipc<-ipc %>% 
  clean_names() %>% 
  rename(snu1=snu,
         community=sub_district) %>% 
  mutate(table="ipc")


monthly<-bind_rows(covid,decanting,hivss,index,tld,ipc) %>% 
  mutate(mon_yr= format(date, "%Y-%m")) %>% 
  select(-date)

#combine -----------------------------------------------------------------------
final_df<-bind_rows(hfr_syzatt,monthly) 
  

write_tsv(final_df,here("Dataout/monthly","monthly_nonmer_data_combined_2021-04-02.txt"),na="")

