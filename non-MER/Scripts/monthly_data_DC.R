library(DBI)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(lubridate)
library(gophr)
library(glamr)
library(Wavelength)
library(RPostgres)
library(aws.s3)
library(redshiftTools)
library(RSQLite)
library(RPostgreSQL)
library(googledrive)
library(googlesheets4)

#set_here(path='C:/Users/qs19802/Dropbox/GitHub')


# install.packages("devtools")
# devtools::install_github("USAID-OHA-SI/Wavelength")
# devtools::install_github("sicarul/redshiftTools")


# CREDENTIALS ------------------------------------------------------------------
set_datim() #from glamr - only need to run once to store credentials


load_secrets() #loads credentials once stored once using above line
drive_auth()
gs4_auth()


# DATABASE CONNECTION ----------------------------------------------------------
#LOCAL FUNCTIONS
source(here("Scripts/00_utilities.R"))

#DB FUNCTION
aws_connect <- function(db_name, db_user, db_pass,
                        db_host,db_port) {
  ## Connections
  DBI::dbConnect(
    drv=RPostgres::Postgres(),
    host = db_host,
    port = as.integer(db_port),
    dbname = db_name,
    user = db_user,
    password = db_pass
  )
}


#ONE TIME ONLY TO STORE CREDENTIALS
#set_key(service = pg_service("DC"), "host")
#set_key(service = pg_service("DC"), "port")
#set_key(service = pg_service("DC"), "database")
#set_key(service = pg_service("DC"), "username")
#set_key(service = pg_service("DC"), "password")

#load keys
db_host <-pg_host("DC")
db_port <- pg_port("DC")
db_name <- pg_database("DC")
db_user <- pg_user("DC")
db_pwd <- pg_pwd("DC")


#establish connection
conn <- aws_connect(db_name = db_name, db_user = db_user,
                    db_pass = db_pwd, db_host = db_host, db_port = db_port)



# GLOBALS ----------------------------------------------------------------------
current_month<-"2023-06" # CHANGE EACH MONTH
current_month_full<-"2023-06-30" # CHANGE EACH MONTH
last_month<- "2023-05" #CHANGE EACH MONTH
current_mo_minus_3<- "2023-03" #CHANGE EACH MONTH
lastQmo<-"2023-06" #CHANGE TO BE LAST MONTH OF MOST RECENTLY REPORTED MER Q

myuser<-"kkehoe"


# REF FILES --------------------------------------------------------------------
ind<-read_sheet(as_sheets_id('1vXPgz5QXLv4H9uw1b2dpHUHdYnmTXGYGhy29HAgQCMs'),
                sheet = "Indicator Triangulation (for USAID Use)",
                col_types="c") %>%
  clean_names() %>%
  filter(new_indidcator_status=="Active") %>%
  select(old_indicator_name,new_indidcator_status,new_table,
         desired_data_centre_indicator_named) %>%
  rename(indicator_1=desired_data_centre_indicator_named) %>%
  mutate(new_table=case_when(
    new_table=="Community Linkage" ~ "hfr",
    TRUE ~ new_table
  ))


#HIERARCHY FROM REDSHIFT
hierarchy_list<-dbGetQuery(conn,"SELECT * FROM datim_hierarchy_codelist")

orgunits<-hierarchy_list %>%
  select(orgunit_uid,orgunit_name)

#HISTORICAL DATA JUNE 22 and PRIOR ---------------------------------------------
monthly_extracts<-here("Data/monthly")
historic_file<-list.files(monthly_extracts,pattern="historical")
df_historic<-read_tsv(here("Data/monthly",historic_file))


df_historic_munge<-df_historic %>%
  filter(!table=="mer",
         !table=="NHLS") %>%
  select(operatingunit:standardizeddisaggregate) %>%
  left_join(ind,by=c("indicator"="old_indicator_name"),keep=TRUE) %>%
  select(-table,-indicator,-old_indicator_name,-indicator_status,
         -indicator_code,-new_indidcator_status,-org_unit_status) %>%
  rename(table=new_table,
         indicator=indicator_1) %>%
  filter(!is.na(indicator)) %>%
  mutate(psnu=case_when(
    psnu=="Mopani" ~ "lp Mopani District Municipality",
    psnu=="City of Cape Town" ~ "wc City of Cape Town Metropolitan Municipality",
    psnu=="City of Johannesburg" ~ "gp City of Johannesburg Metropolitan Municipality",
    psnu=="Sedibeng" ~ "gp Sedibeng District Municipality",
    psnu=="Capricorn" ~ "lp Capricorn District Municipality",
    psnu=="King Cetshwayo" ~ "kz King Cetshwayo District Municipality",
    psnu=="Ugu" ~ "kz Ugu District Municipality",
    psnu=="Gert Sibande"~ "mp Gert Sibande District Municipality",
    psnu=="Nkangala" ~ "mp Nkangala District Municipality",
    psnu=="Alfred Nzo" ~ "ec Alfred Nzo District Municipality",
    psnu=="Buffalo City" ~ "ec Buffalo City Metropolitan Municipality",
    psnu=="Harry Gwala" ~ "kz Harry Gwala District Municipality",
    psnu=="Thabo Mofutsanyane" ~ "fs Thabo Mofutsanyane District Municipality",
    psnu=="Ehlanzeni" ~ "mp Ehlanzeni District Municipality",
    psnu=="Lejweleputswa" ~ "fs Lejweleputswa District Municipality",
    psnu=="eThekwini" ~  "kz eThekwini Metropolitan Municipality",
    TRUE ~ psnu),
    community=case_when(
      community=="lp Polokwane Local Municipality" ~ "lp Polokwane Local Municipality EHP",
      community=="ec Umzimvubu Health sub-District" ~ "ec Umzimvubu Local Municipality",
      community=="ec Buffalo City Local Municipality" ~ "ec Buffalo City Health sub-District",
      community=="fs Dihlabeng Local Municipality" ~ "fs Dihlabeng Local Municipality - LG EHS",
      TRUE ~ community
    ),
    facility=case_when(
      facility=="gp Alexandra 18th Avenue Clinic" ~ "gp Alexandra 18th Avenue",
      TRUE ~ facility
    ),
    site_type=case_when(
    table=="index" ~ "District",
    table=="covid-19" ~ "District",
    table=="hivss" & facility=="(blank)" ~ "Community", # GINA TO ADD WORKPLACE!
    table=="hivss" & !facility=="(blank)" ~ "Facility", # GINA TO ADD WORKPLACE!
    TRUE ~ "Facility"),
    orgunit_name_OLD=case_when(
      site_type=="District" ~ psnu,
      site_type=="Facility" ~ facility,
      site_type=="Community" ~ community)
    ) %>%
  left_join(orgunits,by=c("orgunit_name_OLD"="orgunit_name")) %>%
  mutate(orgunit_uid=case_when(
    is.na(orgunit_uid) & site_type=="Facility" ~ facilityuid,
    TRUE ~ orgunit_uid
  )) %>%
  select(-operatingunit,-snu,-snu1,-psnu,-community,-facility,-facilityuid,
         -orgunit_name_OLD,-fundingagency) %>%
  left_join(hierarchy_list,by="orgunit_uid") %>%
  select(-datim_hierarchy_codelist_uid,-date_created,-last_modified,-is_deleted) %>%
  relocate(orgunit_uid:facility, .before=mech_code) %>%
  relocate(indicator,standardizeddisaggregate,disaggregate,numeratordenom,
           .after=funding_agency) %>%
  relocate(site_type, .after = facility) %>%
  relocate(program_area_element, .before = indicator) %>%
  relocate(period_type, .after = period) %>%
  relocate(value, .after = table) %>%
  mutate(mech_code=as.character(mech_code)) %>%
  filter(!orgunit_uid %in% c("Ea134rZlKXX",
                             "ekRXPVQUGw4",
                             "KJ6EYihrWdp",
                             "IerN7CQQCkP",
                             "pSyJH54GQP8",
                             "HMTtf6bjH2g"))


# identify and fix missing orgunituids
nas<-df_historic_munge %>%
  filter(is.na(orgunit_uid)) %>%
  distinct(site_type,psnu,community,facility,orgunit_uid)

# identify duplicate facilities
dup<-df_historic_munge %>% distinct(orgunit_name,orgunit_uid) %>%
  count(orgunit_name) %>%
  filter(n>1)


# DATA CENTER JULY 22 ONWARD ---------------------------------------------------


# read relational tables from redshift
import_table <- dbGetQuery(conn,"SELECT * FROM import_table") %>%
  select(orgunit_uid,dataelement_codelist_uid,dataelement_codelist_uid,period_codelist_uid,
         mech_codelist_uid,indicator_value__datatype_numeric,is_cleared,indicator_value,
         comment_uid,other_comment)

data_element_list<-dbGetQuery(conn,"SELECT * FROM dataelement_codelist") %>%
  filter(is_current=="TRUE")

hierarchy_list<-dbGetQuery(conn,"SELECT * FROM datim_hierarchy_codelist") %>%
  select(-date_created,-last_modified,-is_deleted)

mech_list<-dbGetQuery(conn,"SELECT * FROM mech_codelist") %>%
  select(mech_codelist_uid,mech_code,primepartner,fundingagency,
         mech_name)

period_list<-dbGetQuery(conn,"SELECT * FROM period_codelist") %>%
  select(period_codelist_uid,period,quarter)


pops <- dbGetQuery(conn,"SELECT * FROM refpops") %>%
  filter(is_current=="TRUE") %>%
  select(orgunit_uid) %>%
  mutate(pops_site="Yes")

staffing_model<-dbGetQuery(conn,"SELECT * FROM refstaffing_model") %>%
  filter(is_current=="TRUE") %>%
  select(orgunit_uid,staffing_model)

# merge tables
df<-import_table %>%
  left_join(data_element_list, by="dataelement_codelist_uid") %>%
  left_join(hierarchy_list, by="orgunit_uid") %>%
  left_join(mech_list, by="mech_codelist_uid") %>%
  left_join(period_list, by="period_codelist_uid") %>%
  select(orgunit_uid,orgunit_name,site_type,operatingunit,snu1uid,snu1,
         psnuuid,psnu,communityuid,community,facilityuid,facility,
         fundingagency,mech_code,
         mech_name,primepartner,dataset,indicator,disaggregate,
         categoryoptioncomboname,age,sex,period,quarter,period_frequency,
         is_cleared,last_modified,indicator_value__datatype_numeric,
         comment_uid,other_comment) %>%
  rename(prime_partner_name=primepartner,
         funding_agency=fundingagency,
         period_type=period_frequency,
         table=dataset,
         mon_yr=period,
         period=quarter,
         last_refreshed=last_modified,
         value=indicator_value__datatype_numeric) %>%
  filter(mon_yr >"2022-06-30" & mon_yr <= current_month_full,
         is_cleared=="TRUE",
         # mon_yr=="2022-07-31" & is_cleared=="TRUE" | mon_yr=="2022-08-31" & is_cleared=="FALSE",
         !is.na(indicator)) %>%
  mutate(period_type="monthly",
         mon_yr= format(mon_yr, "%Y-%m"))

#summary check
df_check<-df %>%
  group_by(mon_yr,is_cleared) %>%
  summarize_at(vars(value),sum,na.rm=TRUE) %>%
  spread(mon_yr,value)



# MERGE HISTORIC AND JULY + ----------------------------------------------------

df_current<-df_historic_munge %>%
  bind_rows(df) %>%
  relocate(categoryoptioncomboname, .after = standardizeddisaggregate) %>%
  relocate(is_cleared, .after = last_refreshed) %>%
  rename_official()




# NON DC DATA ------------------------------------------------------------------
MER_file<-list.files(monthly_extracts,pattern="SITE")
mer<-read_psd(here("Data/monthly",MER_file))


nhls_file_current<-list.files(monthly_extracts,pattern=paste0(current_month, "_NHLS"))
nhls_path_current<-here("Data/monthly",nhls_file_current)

nhls_c<- nhls_path_current %>%
  excel_sheets() %>%
  purrr::set_names() %>%
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
  left_join(sitelist_district,by="psnuuid") %>%
  filter(mon_yr > current_mo_minus_3) %>%
  filter(mon_yr <= current_month)
  # left_join(sitelist_nhls,by=c("facility_n"="facility")) %>% #change this back to facility once site list match resolved
  # select(-facility) %>% #add back in once site list match resolved
  # rename(facility=orgunit,
  #        facilityuid=orgunituid) #add back in once site list match resolved


nhls_h<-nhls_h %>%
  filter(mon_yr <= current_mo_minus_3) %>%
  mutate(age=case_when(
    age=="44565" ~ "1-4",
    age=="44690" ~ "5-9",
    age=="44848" ~ "10-14",
    TRUE ~ age
  ))

nhls_h_check<-nhls_h %>%
  group_by(indicator,mon_yr) %>%
  summarize_at(vars(value),sum,na.rm=TRUE)


nhls<-bind_rows(nhls_c,nhls_h) %>%
  mutate(indicator=case_when(
    indicator=="CD4 Count" ~ "CD4_COUNT",
    indicator=="CrAg" ~ "CRAG",
    indicator=="EID PCR" ~ "EID_PCR",
    indicator=="HIVVL" ~ "HIV_VL",
    indicator=="Xpert MTB" ~ "XPERT_MTB",
    indicator=="Xpert MTB Rif" ~ "XPERT_MTB_RIF",
    TRUE ~ indicator,
  ))


#export combined nhls file for use next month
write_tsv(nhls,here("Data/monthly",paste0(current_month,"_nhls_combined_output.txt")),na="")


# REJECTIONS -------------------------------------------------------------------

#subset historic rejections as new file has all of 2022
historic_rejections_sub<-rejections_h %>%
  filter(mon_yr <= current_mo_minus_3)

prinf(distinct(historic_rejections_sub,mon_yr))


df_vl_rejections<-vl_rejections %>%
  clean_names() %>%
  filter(vl_results=="REJCT") %>%
  select(8:14,16:17,20:21) %>%
  # select(8:13,15:16,19:20) %>% #for feb 23 data only - missing field
  # mutate(age_category=rejection_code) %>% #for feb 23 data only - missing field
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
  ungroup() %>%
  filter(mon_yr<=current_month) %>%
  mutate(age=case_when(
    age=="20-14" ~ "20-24",
    age=="25-19" ~ "25-29",
    TRUE ~ age
  )) #necessary to fix sept 2022 age typos



df_cd4_rejections<-vl_rejections %>%
  clean_names() %>%
  filter(vl_results=="REJCT") %>%
  select(8:14,16:17,20:21) %>%
  # select(8:13,15:16,19:20) %>% #for feb 23 data only - missing field
  # mutate(age_category=rejection_code) %>% #for feb 23 data only - missing field
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
  ungroup() %>%
  filter(mon_yr<=current_month) %>%
  mutate(age=case_when(
    age=="20-14" ~ "20-24",
    age=="25-19" ~ "25-29",
    TRUE ~ age
  )) #necessary to fix sept 2022 age typos


current_rejections<-bind_rows(df_vl_rejections,df_cd4_rejections)

rejections_check<-current_rejections %>%
  group_by(indicator,mon_yr) %>%
  summarize_at(vars(value),sum,na.rm=TRUE)

rejections_combined<-bind_rows(current_rejections,historic_rejections_sub) %>%
  mutate(indicator=case_when(
    indicator=="VL_rejection" ~ "VL_REJECTION",
    indicator=="CD4_rejection" ~ "CD4_REJECTION",
    TRUE ~ indicator,
  ))

#export combined rejections file for use next month
write_tsv(rejections_combined,here("Data/monthly",paste0(current_month,"_rejections_combined_output.txt")),na="")

# MER TARGETS ------------------------------------------------------------------
targets<-mer %>%
  filter(fiscal_year %in% c("2021","2022","2023"),
         indicator %in% c("HTS_TST","HTS_TST_POS","HTS_INDEX","HTS_SELF","TX_NEW","TX_CURR",
                          "TX_PVLS"),
         funding_agency=="USAID",
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
  reshape_msd(direction="long",include_type = TRUE) %>%
  select(orgunituid,sitename,funding_agency,indicator,mech_code,operatingunit,prime_partner_name,
         psnu,snu1,facility,facilityuid,disaggregate,period,period_type,value) %>%
  rename(orgunit_uid=orgunituid,
         orgunit_name=sitename) %>%
  mutate(table="mer") %>%
    # indicator=case_when(
    #   indicator=="TX_CURR" ~ "TX_CURR_28",
    #   indicator=="HTS_SELF" ~ "Number of HIVSS test kits distributed",
    #   indicator=="HTS_INDEX" ~ "INDEX_TESTED",
    #   TRUE ~ indicator
    # )) %>% #no longer necessary as we aligned non-MER to MER
  group_by_if(is.character) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup()



#combine -----------------------------------------------------------------------
final_df<-df_current %>%
  filter(mon_yr >"2022-10-31") %>% #reduce non-MER to last 6 mo only
  bind_rows(targets) %>%
  left_join(pops,by="orgunit_uid") %>%
  left_join(staffing_model, by="orgunit_uid") %>%
  mutate(standardizeddisaggregate=as.character(standardizeddisaggregate),
          numeratordenom=as.character(numeratordenom),
          last_refreshed=as.character(last_refreshed),
          is_cleared=as.character(is_cleared)) %>%
  group_by_if(is.character) %>%
  summarize_at(vars(value),sum,na.rm=TRUE) %>%
  # rename_official() %>%
  mutate(indicator2=indicator,
         value2=value) %>%
  spread(indicator2,value2) %>%
  bind_rows(nhls, rejections_combined)
  # clean_psnu()



# EXPORT FILE ------------------------------------------------------------------
filename<-paste(current_month_full,"monthly_nonmer_data_combined_v2.0.txt",sep="_")

write_tsv(final_df, file.path(here("Dataout/monthly"),filename),na="")




site_att<-staffing_model %>%
  full_join(pops, by="orgunit_uid")
