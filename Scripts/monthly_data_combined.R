library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(lubridate)
library(gophr)
library(glamr)
library(Wavelength)


# install.packages("devtools")
# devtools::install_github("USAID-OHA-SI/gophr")



set_datim("gsarfaty_SA")
load_secrets()

# global
current_month<-"2022-01" # CHANGE EACH MONTH
current_month_full<-"2022-01-31" # CHANGE EACH MONTH
last_month<- "2021-12" #CHANGE EACH MONTH
current_mo_minus_3<- "2021-10" #CHANGE EACH MONTH
lastQmo<-"2021-12" #CHANGE TO BE LAST MONTH OF MOST RECENTLY REPORTED MER Q

myuser<-"gsarfaty_SA"


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

usaid_arpa_file<-list.files(monthly_extracts,pattern="ARPA")
usaid_arpa<-read_excel(here("Data/monthly",usaid_arpa_file),
                  sheet="LongData")

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


core_file<-list.files(monthly_extracts,pattern="Core") 


ci_YN<-read_excel(here("Data/monthly",core_file),
                  sheet="Yes_No Wide data")


ci_val<-read_excel(here("Data/monthly",core_file),
                   sheet="Numeric Wide data")


nhls_file_current<-list.files(monthly_extracts,pattern=paste0(current_month, "_NHLS"))
nhls_path_current<-here("Data/monthly",nhls_file_current)

nhls_c<- nhls_path_current %>% 
  excel_sheets() %>% 
  set_names() %>%
  map_dfr(read_excel,
          path = nhls_path_current, 
          .id="sheet")

nhls_file_historic<-list.files(monthly_extracts,pattern=paste0(last_month, "_nhls"))
nhls_path_historic<-here("Data/monthly",nhls_file_historic)

nhls_h<-read_tsv(nhls_path_historic) 


vl_rejection_file_historic<-list.files(monthly_extracts,pattern=paste0(last_month, "_rejections_combined"))
vl_rejection_path_historic<-here("Data/monthly",vl_rejection_file_historic)

rejections_h<-read_tsv(vl_rejection_path_historic) 

vl_rjection_file<-list.files(monthly_extracts,pattern="Rejections")
vl_rejection_path<-here("Data/monthly",vl_rjection_file)
vl_rejections<-vl_rejection_path %>% 
  map_df(read_delim, delim="|")

#transform weekly to monthly----------------------------------------------------

hfr_snapshot<-hfr %>% 
  clean_names() %>% 
  filter(indicator %in% c("cLTFU","Headcount","LATEMISSED","EARLYMISSED","TX_CURR_28",
                          "TX_CURR_90","uLTFU")) %>% 
  rename(age=agecoarse,
         disaggregate=otherdisaggregate,
         # snu1=snu,
         community=sub_district,
         value=val) %>% 
  mutate(table="hfr",
         mon_yr= format(date, "%Y-%m")) %>% 
  group_by(mon_yr,orgunit,mech_code,indicator) %>% 
  filter(date==max(date)) %>% 
  ungroup() %>% 
  mutate(period=quarter(date, with_year = TRUE, fiscal_start = 10),
         period=stringr::str_remove(period, "20"),
         period=str_replace_all(period,"\\.","Q"),
         period=paste0("FY",period))


hfr_cumulative<-hfr %>% 
  clean_names() %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW")) %>% 
  rename(age=agecoarse,
         disaggregate=otherdisaggregate,
         # snu1=snu,
         community=sub_district,
         value=val) %>% 
  mutate(period=quarter(date, with_year = TRUE, fiscal_start = 10),
         period=stringr::str_remove(period, "20"),
         period=str_replace_all(period,"\\.","Q"),
         period=paste0("FY",period)) %>% 
  mutate(table="hfr",
         mon_yr= format(date, "%Y-%m")) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>%  
  filter(mon_yr>"2020-12") %>% 
  ungroup() 




hfr_combined<-bind_rows(hfr_cumulative,hfr_snapshot) %>% 
  mutate(period_type="monthly") %>% 
  select(-date)  


rm(hfr,hfr_cumulative,hfr_snapshot)

siyenza_att<-siyenza_att %>% 
  clean_names() %>% 
  distinct(orgunituid,siyenza_start_date,siyenza_end_date,siyenzasite) %>% 
  filter(siyenzasite=="Yes") %>% 
  rename(facilityuid=orgunituid)

  
siyenza<-siyenza %>% 
  clean_names() %>% 
  filter(indicator %in% c("TPT TX_NEW", "TPT TX_CURR")) %>% 
  rename(
         # snu1=snu,
         community=sub_district,
         date=end_date,
         value=sum_of_value)%>% 
  mutate(table="siyenza",
         mon_yr= format(date, "%Y-%m"),
         operatingunit="South Africa")%>%
  mutate(period=quarter(date, with_year = TRUE, fiscal_start = 10),
         period=stringr::str_remove(period, "20"),
         period=str_replace_all(period,"\\.","Q"),
         period=paste0("FY",period)) %>% 
  select(-c(siyenzasite,date)) %>% 
  group_by_if(is.character) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  ungroup()



sitelist<-hfr_combined %>% 
  select(orgunit,orgunituid) %>% 
  distinct(orgunit,orgunituid)


index<-index%>% 
  clean_names() %>% 
  rename(snu1=province,
         psnu=district,
         mech_code=mechanismid) %>% 
  mutate(table="index",
         mon_yr= format(date, "%Y-%m"),
         period_type="monthly") %>% 
  mutate(period=quarter(date, with_year = TRUE, fiscal_start = 10),
         period=stringr::str_remove(period, "20"),
         period=str_replace_all(period,"\\.","Q"),
         period=paste0("FY",period)) %>% 
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

#ARPA Total N shenanigans ------------------------------------------------------
usaid_arpa<-usaid_arpa %>% 
  clean_names() %>% 
  rename(indicator=data_element,
         indicator_code=dataelement_code,
         numeratordenom=num_denom) %>% 
  mutate(disaggregate=case_when(
    disaggregate=="Total numerator" ~ "Total Numerator, calculated",
    disaggregate=="Denominator" ~ "Total Denominator",
    disaggregate=="Numerator" ~ "Total Numerator, partner entered",
    is.na(disaggregate) ~ "Multisectoral Coordination",
    TRUE ~ disaggregate
  )) %>% 
  mutate(standardizeddisaggregate=case_when(
    disaggregate=="Total Numerator" ~ "Total Numerator",
    disaggregate=="Total numerator" ~ "Total Numerator, calculated",
    disaggregate=="Numerator" ~ "Total Numerator, partner entered",
    disaggregate=="Total Denominator" ~ "Total Denominator",
    disaggregate=="Male" | disaggregate=="Female" | disaggregate=="Unknown sex" ~ "Sex",
    disaggregate=="Case management for severely/critically ill patients" | 
      disaggregate=="Other case management training" ~ "Training",
    disaggregate=="Oxygen concentrators" | disaggregate=="Pressure Swing Absorption (PSA) plant" |
      disaggregate=="Pulse oximeters" ~ "Oxygen-related commodity",
    indicator_code=="CoOp_6.1" & !disaggregate=="Total Numerator" ~ "Result Areas",
    disaggregate=="Healthcare worker" | disaggregate=="Non-Healthcare worker" ~ "Work cadre",
    indicator_code=="RCCE_1.1" & !disaggregate=="Total Numerator" ~ "Mass media type",
    indicator_code=="VACC_0.1" & !disaggregate=="Total Numerator" & !disaggregate=="Sex" ~ "Vaccine brand",
    indicator_code=="VACC_0.2" & disaggregate=="Direct" | disaggregate=="Indirect" ~ "Type of USAID support",
    indicator_code=="VACC_0.4" & !disaggregate=="Total Numerator" & !disaggregate=="Sex" ~ "Vaccine brand",
    indicator_code=="VACC_0.5" & !disaggregate=="Total Numerator" ~ "Mass media type",
    indicator_code=="VACC_0.6" & !disaggregate=="Total Numerator" ~ "Types",
    disaggregate=="Establishing surveillance systems" | disaggregate=="Monitoring and responding to AEFIs" |
      disaggregate=="Monitoring and responding to adverse events of special interest" |
      disaggregate=="Safety data management systems" | disaggregate=="COVID-19 vaccine safety communication" ~ "Topic areas",
    disaggregate=="Developed" | disaggregate=="Adapted" | disaggregate=="Disseminated" ~ "USAID Support",
    indicator_code=="VACC_0.8" ~ "Check as applicable",
    TRUE ~ disaggregate
  ),
  sex=case_when(
    standardizeddisaggregate=="Sex" ~ disaggregate,
    TRUE ~ ""),
  mech_code=case_when(
    partner=="ANOVA" ~ "70310",
    partner=="EQUIP" ~ "160610",
    partner=="GETF Next Mile" ~ "83001",
    partner=="Guidehouse" ~ "18321",
    TRUE ~ ""
  ))


usaid_arpa_YN<-usaid_arpa%>% 
  select(-value) %>% 
  filter(indicator_code=="VACC_0.8",
         !value_text=="(blank)") %>% 
  mutate(value=case_when(
    value_text=="Yes" ~ "1",
    TRUE ~ "0"
  )) %>% 
  select(-value_text) %>% 
  mutate(value=as.numeric(value))



usaid_arpa_val<-usaid_arpa %>% 
  select(-value_text) %>% 
  filter(!is.na(value)) 


usaid_arpa_combined<-bind_rows(usaid_arpa_val,usaid_arpa_YN) %>% 
  mutate(period=quarter(date, with_year = TRUE, fiscal_start = 10),
         period=stringr::str_remove(period, "20"),
         period=str_replace_all(period,"\\.","Q"),
         period=paste0("FY",period)) %>% 
  mutate(table="usaid arpa",
         mon_yr= format(date, "%Y-%m")) %>% 
  select(-date)

  
# COMBINING OF USAID TOTAL NUMERATORS ------------------------------------------------

# usaid_arpa_partner_totalN<-usaid_arpa2 %>% 
#   filter(disaggregate=="Total Numerator,partner entered",
#          date < as.Date("2021-10-01")) %>% 
#   mutate(disaggregate=="Total Numerator")
# 
# usaid_arpa_all_other_disaggs<-usaid_arpa2 %>% 
#   filter(!disaggregate=="Total Numerator - partner entered")
# 
# usaid_arpa3<-bind_rows(usaid_arpa_all_other_disaggs,usaid_arpa_partner_totalN)




decanting<-decanting%>% 
  clean_names() %>% 
  rename(snu1=province,
         psnu=district,
         community=subdistrict,
         orgunit=facility,
         orgunituid=facilityuid,
         mech_code=mechanismid) %>% 
  mutate(table="decanting",
         value=as.numeric(value))


## fix period to be date format
hivss<-hivss%>% 
  clean_names() %>% 
  rename(snu1=snu,
         date=period,
         community=sub_district,
         value=sum_of_value) %>% 
  mutate(table="hivss",
         date=mdy(date),
         period_type="monthly") %>% 
  select(-month)
  


tld<-tld %>% 
  clean_names() %>% 
  rename(snu1=snu,
         community=subdistrict,
         age=agecoarse) %>% 
  filter(age=="All Age") %>% 
  mutate(table="tld")


ipc<-ipc %>% 
  clean_names() %>% 
  rename(snu1=snu,
         community=sub_district) %>% 
  mutate(table="ipc") %>% 
  left_join(sitelist,by="orgunit")

# NHLS -------------------------------------------------------------------------
sitelist_nhls<-pull_hierarchy("cDGPF739ZZr", myuser, askpass::askpass()) %>%
  mutate(facility=str_sub(orgunit,4)) %>%
  select(-level,-countryname,-latitude,-longitude)

#for now only
sitelist_district<-sitelist_nhls %>% 
  distinct(operatingunit,snu1,psnu,psnuuid) %>% 
  filter(!is.na(psnu))

#for now only
nhls_districts<-read_excel(here("Data/monthly", "vl_crosswalk.xlsx"),
                           sheet="NHLS_Districts")

nhls_c<-nhls_c %>% 
  clean_names() %>% 
  # select(-province,-district,-sub_district) %>% #add back in once site list match resolved
  rename(indicator=sheet,
         sex=gender,
         age=age_segment,
         disaggregate=results_seg,
         value=volumes,
         facility_n=facility_name) %>% #change this back to facility once site list match resolved
  mutate(month_tested=as.character(month_tested),
         month_tested=str_pad(month_tested, 2, side="left", pad = "0"),
         table="NHLS",
         district=str_to_title(district),
         facility_n=str_to_title(facility_n), #change this back to facility once site list match resolved
         sex=case_when(
           sex=="F" ~ "Female",
           sex=="M" ~ "Male",
           sex=="U" ~ "Unknown"
         )) %>%
  unite(mon_yr,year_tested,month_tested,sep="-",remove=TRUE) %>% 
  filter(!is.na(province)) %>% 
  select(-province) %>% 
  left_join(nhls_districts,by=c("district"="health_district")) %>% 
  select(-district) %>% 
  left_join(sitelist_district,by="psnuuid")
  # left_join(sitelist_nhls,by=c("facility_n"="facility")) %>% #change this back to facility once site list match resolved
  # select(-facility) %>% #add back in once site list match resolved
  # rename(facility=orgunit,
  #        facilityuid=orgunituid) #add back in once site list match resolved


nhls_h<-nhls_h %>%
  filter(mon_yr <= current_mo_minus_3)


nhls<-bind_rows(nhls_c,nhls_h)
  

#export combined nhls file for use next month
write_tsv(nhls,here("Data/monthly",paste0(current_month,"_nhls_combined_output.txt")),na="")



vl_rejections<-vl_rejections %>% 
  clean_names() %>% 
  filter(vl_results=="REJCT") %>% 
  select(8:14,16:17,20:21) %>% 
  rename(district=district_name,
         facility_n=facility_name,
         disaggregate=rejection_reason1,
         otherdisaggregate=rejection_description,
         age=age_category) %>% 
  mutate(month1=as.character(month1),
         year1=as.character(year1),
         month1=str_pad(month1, 2, side="left", pad = "0"),
         table="NHLS",
         district=str_to_title(district),
         facility_n=str_to_title(facility_n),
         disaggregate=str_to_lower(disaggregate),
         disaggregate=str_replace_all(disaggregate," ","_"),
         indicator="VL_rejection") %>% 
  filter(test_method_name=="HIV VIRAL LOAD") %>% 
  unite(mon_yr,year1,month1,sep="-",remove=TRUE) %>% 
  select(-sub_district_name) %>% 
  left_join(nhls_districts,by=c("district"="health_district")) %>% 
  select(-district, -test_method_name) %>% 
  left_join(sitelist_district,by="psnuuid") %>% 
  group_by_if(is.character) %>% 
  count() %>% 
  summarize(value = sum(n, na.rm = T)) %>% 
  ungroup()

rejections_combined<-bind_rows(vl_rejections,rejections_h)
  
#export combined rejections file for use next month
write_tsv(rejections_combined,here("Data/monthly",paste0(current_month,"_rejections_combined_output.txt")),na="")


#core interventions ------------------------------------------------------------
ci_YN<-ci_YN%>% 
  gather(date,value,14:40) %>% 
  filter(!is.na(value)) %>%
  mutate(value=case_when(
    value=="Yes" ~ "1",
    value=="YES" ~ "1",
    value=="yes" ~ "1",
    value=="No" ~ "0",
    value=="NO" ~ "0",
    value=="no" ~ "0",
  )) %>% 
  mutate(value=as.numeric(value)) %>% 
  filter(!is.na(value))


ci_val<-ci_val %>% 
  gather(date,value,15:41) %>% 
  mutate(value=as.numeric(value)) %>% 
  filter(!is.na(value))


ci_bound<-bind_rows(ci_YN,ci_val) %>% 
  clean_names() %>% 
  mutate(program_area_element=case_when(
    indicator=="Does the facility meet the minimum standards for ethical implementation of index testing?" ~ "HTS",
    str_detect(indicator, "Synch") ~ "Decanting",
    str_detect(indicator, "Community ART") ~ "Linkage/ Retention",
    TRUE ~ program_area_element),
    table="core_interventions")%>% 
  rename(community=sub_district) %>% 
  mutate(date=mdy(date))


rm(ci_YN,ci_val)


monthly<-bind_rows(covid,decanting,hivss,tld,ipc,ci_bound) %>% 
  mutate(period=quarter(date, with_year = TRUE, fiscal_start = 10),
         period=stringr::str_remove(period, "20"),
         period=str_replace_all(period,"\\.","Q"),
         period=paste0("FY",period),
         mon_yr= format(date, "%Y-%m")) %>% 
  select(-date)



# MER TARGETS ------------------------------------------------------------------
targets<-mer %>% 
  filter(fiscal_year %in% c("2021","2022"),
         indicator %in% c("HTS_TST","HTS_TST_POS","HTS_INDEX","HTS_SELF","TX_NEW","TX_CURR"),
         fundingagency=="USAID",
         standardizeddisaggregate=="Total Numerator") %>% 
  reshape_msd(direction="long",include_type = TRUE) %>% 
  select(fundingagency,indicator,mech_code,operatingunit,primepartner,
         psnu,snu1,disaggregate,period,period_type,value) %>% 
  mutate(table="mer",
    indicator=case_when(
      indicator=="TX_CURR" ~ "TX_CURR_28",
      indicator=="HTS_SELF" ~ "Number of HIVSS test kits distributed",
      indicator=="HTS_INDEX" ~ "INDEX_TESTED",
      TRUE ~ indicator
    )) %>% 
  group_by_if(is.character) %>% 
  summarize(value = sum(value, na.rm = T)) %>% 
  ungroup()



#combine -----------------------------------------------------------------------
final_df<-bind_rows(hfr_combined,monthly,index,siyenza,usaid_arpa_combined) %>% 
  filter(!is.na(value)) %>%
  rename(facility=orgunit,
         facilityuid=orgunituid) %>%
  filter(mon_yr <= current_month & mon_yr >"2020-12") %>% 
  bind_rows(targets) %>% 
  select(-c(partner)) %>%
  mutate(operatingunit="South Africa") %>% 
  rename_official() %>% 
  mutate(indicator2=indicator,
         value2=value) %>%
  spread(indicator2,value2) %>%
  bind_rows(nhls,rejections_combined) %>% 
  left_join(siyenza_att,by="facilityuid") %>% 
  clean_psnu()


# EXPORT FILE ------------------------------------------------------------------
filename<-paste(current_month_full,"monthly_nonmer_data_combined_v1.0.txt",sep="_")

write_tsv(final_df, file.path(here("Dataout/monthly"),filename),na="")




# USAID ARPA EXPORT
write_tsv(usaid_arpa,here("Dataout/monthly","2021-12-31_monthly_nonmer_data_usaid_arpa.txt"),na="")




# USAID REJECTIONS
write_tsv(vl_rejections,here("Dataout/monthly","2021-12_monthly_VLrejects.txt"),na="")


install.packages("devtools")
devtools::install_github("USAID-OHA-SI/gophr")
