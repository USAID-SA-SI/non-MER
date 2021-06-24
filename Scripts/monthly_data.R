library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(lubridate)
library(ICPIutilities)



#read in dsp viz tables --------------------------------------------------------
monthly_extracts<-here("Data/monthly")


MER_file<-list.files(monthly_extracts,pattern="Genie") 
mer<-read_msd(here("Data/monthly",MER_file))
  
  
hfr_file<-list.files(monthly_extracts,pattern="HFR") 
hfr<-read_excel(here("Data/monthly",hfr_file),
                     sheet="ForDataViz",
                skip=14)

# workforce_file<-list.files(monthly_extracts,pattern="Workforce") # NEEEEEED UPDATE ########
# workforce<-read_excel(here("Data/monthly",workforce_file),
#                 sheet="forDSPviz")


siyenza_file<-list.files(monthly_extracts,pattern="Siyenza") 
siyenza<-read_excel(here("Data/monthly",siyenza_file),
                      sheet="ForDataViz")


siyenza_att<-read_excel(here("Data/monthly",siyenza_file),
                               sheet="Raw_Data")



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

siyenza_att<-siyenza_att %>% 
  clean_names() %>% 
  distinct(orgunituid,siyenza_start_date,siyenza_end_date,siyenzasite) %>% 
  filter(siyenzasite=="Yes") %>% 
  rename(facilityuid=orgunituid)


siyenza_snapshot<-siyenza %>% 
  clean_names() %>% 
  filter(indicator %in% c("TPT TX_CURR")) %>% 
  rename(snu1=snu,
         community=sub_district,
         date=end_date,
         value=sum_of_value)%>% 
  mutate(table="siyenza",
         mon_yr= format(date, "%Y-%m"),
         operatingunit="South Africa") %>% 
  group_by(mon_yr,orgunit,mech_code,indicator) %>% 
  filter(date==max(date)) %>% 
  ungroup() %>%  
  select(-c(siyenzasite,date))

  
siyenza_cumulative<-siyenza %>% 
  clean_names() %>% 
  filter(indicator %in% c("TPT TX_NEW")) %>% 
  rename(snu1=snu,
         community=sub_district,
         date=end_date,
         value=sum_of_value)%>% 
  mutate(table="siyenza",
         mon_yr= format(date, "%Y-%m"),
         operatingunit="South Africa")%>%
  select(-c(siyenzasite,date)) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  ungroup()


siyenza_combined<-bind_rows(siyenza_cumulative,siyenza_snapshot)


rm(siyenza,siyenza_cumulative,siyenza_snapshot)



sitelist<-hfr_combined %>% 
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

# MER TARGETS ------------------------------------------------------------------
targets<-mer %>% 
  reshape_msd(clean = TRUE) %>% 
  filter(period_type=="targets",
         indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW","TX_CURR"),
         fundingagency=="USAID") %>% 
  select(fundingagency,indicator,mech_code,operatingunit,primepartner,
         psnu,snu1,disaggregate,period,period_type,value) %>% 
  mutate(indicator=paste(indicator,period,period_type,sep="_"),
         table="mer",
         mon_yr="2021-05") %>% # remember to change this each time!
  select(-period,-period_type)

#combine -----------------------------------------------------------------------
final_df<-bind_rows(hfr_combined,siyenza_combined,monthly,index,targets) %>% 
  filter(!is.na(value)) %>% 
  rename(facility=orgunit,
         facilityuid=orgunituid) %>%
  rename_official() %>%
  filter(mon_yr < "2021-06") %>% #remember to change this each time!
  select(-c(partner,mech_name)) %>% 
  mutate(indicator2=indicator,
         value2=value) %>%
  spread(indicator2,value2) %>%
  left_join(siyenza_att,by="facilityuid")



write_tsv(final_df,here("Dataout/monthly","2021-05-31_monthly_nonmer_data_combined_v2.1.txt"),na="")



# core interventions -----------------------------------------------------------
# ------------------------------------------------------------------------------

siyenza_file<-list.files(monthly_extracts,pattern="Siyenza") 
siyenza_df<-read_excel(here("Data/monthly",siyenza_file),
           sheet="Raw_Data")

core_file<-list.files(monthly_extracts,pattern="Core") 
core<-read_excel(here("Data/monthly",core_file),
                 sheet="Full_Data")

ref_file<-list.files(monthly_extracts,pattern="status")
ref<-read_excel(here("Data/monthly",ref_file),
                 sheet="status")

ci_YN<-core %>% 
  filter(!ValueYesNo=="(blank)") %>% 
  mutate(val=case_when(
    `Yes OrgUnit DistinctCount` > 0 ~ "1",
    `No OrgUnit DistinctCount`>0 ~ "0",
    is.na(`Yes OrgUnit DistinctCount`) & is.na(`No OrgUnit DistinctCount`) ~ ""
  )) %>% 
  select(-c(`Yes OrgUnit DistinctCount`,`No OrgUnit DistinctCount`,
            `Total OrgUnit DistinctCount`,`Sum of ValueNumeric`,ValueYesNo)) %>% 
  mutate(val=as.numeric(val))

ci_val<-core %>% 
  filter(ValueYesNo=="(blank)") %>% 
  select(-c(`Yes OrgUnit DistinctCount`,`No OrgUnit DistinctCount`,`Total OrgUnit DistinctCount`)) %>% 
  rename(val=`Sum of ValueNumeric`) %>% 
  select(-ValueYesNo) 


ci_bound<-bind_rows(ci_YN,ci_val) %>% 
  left_join(ref, by="indicator")


rm(ci_YN,ci_val)

siyenza_att_ci<-siyenza_df %>%
  clean_names() %>%
  distinct(orgunit,siyenza_start_date,siyenza_end_date,siyenzasite) %>%
  filter(siyenzasite=="Yes")


final<-ci_bound %>% 
  left_join(siyenza_att_ci, by="orgunit") %>% 
  mutate(`Program Area/ Element`=case_when(
    indicator=="Does the facility meet the minimum standards for ethical implementation of index testing?" ~ "HTS",
    str_detect(indicator, "Synch") ~ "Decanting",
    str_detect(indicator, "Community ART") ~ "Linkage/ Retention",
    TRUE ~ `Program Area/ Element`
  )) %>% 
  filter(End_Date <as.Date("2021-06-01"))
  



filename<-paste("core_interventions", "_long", Sys.Date(), ".txt", sep="")

write_tsv(final, file.path(here("Dataout/monthly"),filename,na=""))


