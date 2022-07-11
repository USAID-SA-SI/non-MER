library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(lubridate)
library(gophr)
library(glamr)
library(Wavelength)

#globals -----------------------------------------------------------------------
monthly_extracts<-here("Data/monthly/archive_nhls")



set_datim("gsarfaty_SA")
load_secrets()


myuser<-"gsarfaty_SA"

#NHLS DISTRICTS ----------------------------------------------------------------
nhls_districts<-read_excel(here("Data/monthly", "vl_crosswalk.xlsx"),
                        sheet="NHLS_Districts")

sitelist_nhls<-pull_hierarchy("cDGPF739ZZr", myuser, askpass::askpass()) %>%
  mutate(facility=str_sub(orgunit,4)) %>%
  select(-level,-countryname,-latitude,-longitude)


sitelist_district<-sitelist_nhls %>% 
  distinct(operatingunit,snu1,psnu,psnuuid) %>% 
  filter(!is.na(psnu))



# April COMBINED FULL FILE ---------------------------------------------------------------
nhls_file_historic<-list.files(monthly_extracts,pattern=paste0("2022-04_nhls_combined"))
nhls_path_historic<-here("Data/monthly/archive_nhls",nhls_file_historic)


nhls_h<-read_tsv(nhls_path_historic) 


check_historic<-nhls_h %>% 
  filter(indicator=="HIVVL") %>% 
  group_by(indicator,mon_yr) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE)

print(check_historic)

#filter out Dec 2021 + which was double counted

nhls_h_sub<-nhls_h %>% 
  filter(mon_yr<"2021-12")


# FEB 2022 File to Replace DEC ---------------------------------------------------------
nhls_file_feb<-list.files(monthly_extracts,pattern=paste0("2022-02_NHLS"))
nhls_path_feb<-here("Data/monthly/archive_nhls",nhls_file_feb)


nhls_dec21<- nhls_path_mar %>% 
  excel_sheets() %>% 
  set_names() %>%
  map_dfr(read_excel,
          path = nhls_path_feb, 
          .id="sheet")


nhls_dec21<-nhls_dec21 %>% 
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
  left_join(sitelist_district,by="psnuuid") %>% 
  filter(mon_yr=="2021-12")



# MAR 2022 File to Replace Jan ---------------------------------------------------------
nhls_file_mar<-list.files(monthly_extracts,pattern=paste0("2022-03_NHLS"))
nhls_path_mar<-here("Data/monthly/archive_nhls",nhls_file_mar)


nhls_jan22<- nhls_path_mar %>% 
  excel_sheets() %>% 
  set_names() %>%
  map_dfr(read_excel,
          path = nhls_path_mar, 
          .id="sheet")


nhls_jan22<-nhls_jan22 %>% 
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
  left_join(sitelist_district,by="psnuuid") %>% 
  filter(mon_yr=="2022-01")


# APRIL 2022 File to Replace Feb, Mar, Apr ---------------------------------------------------------
nhls_file_apr<-list.files(monthly_extracts,pattern=paste0("2022-04_NHLS"))
nhls_path_apr<-here("Data/monthly/archive_nhls",nhls_file_apr)


nhls_febtoapr<- nhls_path_mar %>% 
  excel_sheets() %>% 
  set_names() %>%
  map_dfr(read_excel,
          path = nhls_path_apr, 
          .id="sheet")


nhls_febtoapr<-nhls_febtoapr %>% 
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


# COMBINE & EXPORT -------------------------------------------------------------
combined<-bind_rows(nhls_h_sub,nhls_dec21,nhls_jan22,nhls_febtoapr)


write_tsv(combined,here("Data/monthly","2022-04_nhls_combined_output_v1.1.txt"),na="")


check_combined<-combined %>% 
  filter(indicator=="HIVVL") %>% 
  group_by(indicator,mon_yr) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE)

#----------------------------#REJECTIONS ---------------------------------------

vl_rjection_file<-list.files(monthly_extracts,pattern="John") #2021
vl_rejection_path<-here("Data/monthly/archive_nhls",vl_rjection_file)
vl_rejections<-vl_rejection_path %>% 
  map_df(read_delim, delim="|")


df_vl_rejections<-vl_rejections %>% 
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



df_cd4_rejections<-vl_rejections %>% 
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
         indicator="CD4_rejection") %>% 
  filter(test_method_name=="CD4 ARV") %>% 
  unite(mon_yr,year1,month1,sep="-",remove=TRUE) %>% 
  select(-sub_district_name) %>% 
  left_join(nhls_districts,by=c("district"="health_district")) %>% 
  select(-district, -test_method_name) %>% 
  left_join(sitelist_district,by="psnuuid") %>% 
  group_by_if(is.character) %>% 
  count() %>% 
  summarize(value = sum(n, na.rm = T)) %>% 
  ungroup()

rejections_check<-df_cd4_rejections %>% 
  group_by(indicator,mon_yr) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE)

rejections_combined<-bind_rows(df_cd4_rejections,df_vl_rejections)

write_tsv(rejections_combined,here("Data/monthly","2022-04_rejections_combined_output_v1.2.txt"),na="")
