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

myuser<-"gsarfaty_SA"



#NHLS DISTRICTS ----------------------------------------------------------------
nhls_districts<-read_excel(here("Data/monthly", "vl_crosswalk.xlsx"),
                        sheet="NHLS_Districts")


sitelist_nhls<-pull_hierarchy("cDGPF739ZZr", myuser, mypwd(myuser)) %>%
  mutate(facility=str_sub(orgunit,4)) %>%
  select(-level,-countryname,-latitude,-longitude)


sitelist_district<-sitelist_nhls %>% 
  distinct(operatingunit,snu1,psnu,psnuuid) %>% 
  filter(!is.na(psnu))



# JUNE FULL FILE ---------------------------------------------------------------
nhls_file_historic<-list.files(monthly_extracts,pattern=paste0("2022-04_nhls_combined"))
nhls_path_historic<-here("Data/monthly/archive_nhls",nhls_file_historic)


nhls_h<-read_tsv(nhls_path_historic) 


check_historic<-nhls_h %>% 
  filter(indicator=="HIVVL") %>% 
  group_by(indicator,mon_yr) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE)

print(check_historic)

nhls_h<- nhls_path_historic %>% 
  excel_sheets() %>% 
  set_names() %>%
  map_dfr(read_excel,
          path = nhls_path_historic, 
          .id="sheet")


nhls_h<-nhls_h %>% 
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
  filter(mon_yr < "2021-05") 

# JULY FILE -- 5, 6, 7 ---------------------------------------------------------
nhls_file_july<-list.files(monthly_extracts,pattern=paste0("2021-07_NHLS"))
nhls_path_july<-here("Data/monthly/Archive",nhls_file_july)

nhls_july<- nhls_path_july%>% 
  excel_sheets() %>% 
  set_names() %>%
  map_dfr(read_excel,
          path = nhls_path_july, 
          .id="sheet")


nhls_july<-nhls_july %>% 
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
  filter(mon_yr == "2021-05") #keep only may


# AUG FILE -- 6,7,8 ------------------------------------------------------------
nhls_file_aug<-list.files(monthly_extracts,pattern=paste0("2021-08_NHLS"))
nhls_path_aug<-here("Data/monthly/Archive",nhls_file_aug)

nhls_aug<- nhls_path_aug%>% 
  excel_sheets() %>% 
  set_names() %>%
  map_dfr(read_excel,
          path = nhls_path_aug, 
          .id="sheet")


nhls_aug<-nhls_aug %>% 
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
combined<-bind_rows(nhls_h,nhls_july,nhls_aug)


write_tsv(combined,here("Data/monthly","2021-08_nhls_combined_output.txt"),na="")