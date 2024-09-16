# AUTHOR:   C.Trapence | USAID
# PURPOSE:  Automating the DQRT process for OVC non_MER indicators reported by USAID partners
# DATE:     2023-07-04
# UPDATED:  2024-09-16
# UPDATED BY:  R. Pineteh | USAID

# SOURCE FILES-----------------------------------------------------------

 #'[Source files used in the script include Partner Google Sheets]


# LOAD PACKAGES -----------------------------------------------------------
if(!require(pacman)) install.packages("pacman")
  pacman::p_load(glitr, gophr, extrafont, scales, tidytext, anytime, here, patchwork, ggtext, glue, readxl, googlesheets4,glamr, tidyverse, maditr, openxlsx,readr,stringr,sqldf)


# GLOBAL VARIABLES --------------------------------------------------------
load_secrets()
Date <- Sys.Date()

  get_last_day_of_previous_month <- function() {
    # Get today's date
    Date <- Sys.Date()
    
    # Get the first day of the current month
    first_day_of_current_month <- floor_date(Date, "month")
    
    # Subtract one day to get the last day of the previous month
    last_day_of_previous_month <- first_day_of_current_month - days(1)
    
    return(last_day_of_previous_month)
  }
    # Define reporting period
  reporting_period <- get_last_day_of_previous_month()
  
  
    #'[HIVSA (70307)] 
                    
  # Reading historical data
  range_HIVSA <- "A2:AK394"
  HIVSA_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1-EDzs3Wxf-T6r6SJ_AepsqDbln-LAM8fwy9IPhLvd8g/edit#gid=147779424"), sheet = "OVC Indicators", range = range_HIVSA)%>% 
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=1)
  
  ## Reading data for current reporting period 
  HIVSA_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1-EDzs3Wxf-T6r6SJ_AepsqDbln-LAM8fwy9IPhLvd8g/edit#gid=147779424"), sheet = "FY24_Reporting_Tool") %>% 
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=2)
  
  ## Consolidating HIVSA's historical and current data
  HIVSA<-bind_rows(HIVSA_FY24,HIVSA_FY23)
  

  #'[FHI 360 (14295)]
  
  ## Reading historical data. FHI 360 last reported in September 2023. No reporting expected for FY_2024
  range_FHI360 <- "A2:AK2466"
  FHI360<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1KLFnNV5szfw0Ns1Q4kjV5J2x7qp4biYTeDwU4YMdJlM/edit?usp=drive_link"), sheet = "OVC Indicators", range = range_FHI360 ) %>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=1)

  #'[PACT Inc (14631 and 86130)]
  
  ## Reading historical data for old mech code 14631.
  range_PACT <- "A2:AK3417"
  PACT_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1G8xIspdRzFswVk59WvYjVK22k7_Pi9EqrSp2uvdA-HA/edit#gid=0"), sheet = "OVC Indicators", range = range_PACT) %>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=1) 
  
  ## Reading data for current reporting period  new mech code 86130. PACT indicated on email they will not be reporting on OVC indicators for Oct 2023
  PACT_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1ae6N27LozI1q2iv4oQVLIoGzBmE72r7foVvHjQ0KjFQ/edit#gid=2041408209"),sheet = "FY24_Reporting_Tool")%>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=2)
  
  ## Consolidating PACT's historical and current data
  PACT<-bind_rows(PACT_FY23,PACT_FY24)
  
  
  #'[Mothers to Mothers (80004)]
  
  ## Reading historical data
  range_M2M <-  "A2:AK944"
  
  M2M_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1BLl3QcQyZJulAkxDWONJRXOcvMDEOPtmHbbtHLGCMXA/edit#gid=329992892"), sheet = "OVC Indicators", range = range_M2M) %>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=1)
 
  ## Reading data for current reporting period 
  M2M_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1BLl3QcQyZJulAkxDWONJRXOcvMDEOPtmHbbtHLGCMXA/edit#gid=329992892"), sheet = "FY24_Reporting_Tool")%>% 
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=2)
  
  ## Consolidating M2M's historical and current data
  M2M<-bind_rows(M2M_FY23,M2M_FY24)
  
  
  #'[CINDI (70311)]

  ## Reading historical data.
  range_CINDI <-  "A2:AK450"
  CINDI_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/11v2uMvKG2WSsOeKy_KFQEssE5dznkT3-TYzk88gvpuw/edit#gid=0"), sheet = "OVC Indicators", range = range_CINDI) %>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=1)
  
  ## Reading data for current reporting period 
  CINDI_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/11v2uMvKG2WSsOeKy_KFQEssE5dznkT3-TYzk88gvpuw/edit#gid=0"), sheet = "FY24_Reporting_Tool")%>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=2)
  
  ## Consolidating CINDI's historical and current data
  CINDI<-bind_rows(CINDI_FY23,CINDI_FY24)
  
  #'[G2G (81904)]
  
  ## Reading historical data.
  range_G2G<-  "A2:AK730"
  G2G_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/18sQPukk1KQhyGNogJDbjvcGJRBOJQYzAN7pdwmbfixU/edit#gid=329992892"), sheet = "OVC Indicators", range = range_G2G) %>%
    mutate(mech_code = as.character(mech_code)) %>% mutate(timer=1)
  
  ## Reading data for current reporting period 
  G2G_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/18sQPukk1KQhyGNogJDbjvcGJRBOJQYzAN7pdwmbfixU/edit#gid=351803566"), sheet = "FY24_Reporting_Tool")%>%
  mutate(mech_code = as.character(mech_code)) %>% mutate(timer=2)
  
  ## Consolidating G2G's historical and current data 
  G2G<-bind_rows(G2G_FY23,G2G_FY24)

  
  #'[MATCH (87576)]
  
  ##  Reading data for current reporting period 
  MATCH_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1piCIbC-d1-E_qIEhtEpYCHdwaLr6ihurrxdjnFDtmUA/edit#gid=147779424"), sheet = "FY24_Reporting_Tool") %>%
  mutate(mech_code = as.character(mech_code)) %>% mutate(timer=2)
  
  #'[NACOSA (80002 & 80008)]
  
  ## Reading historical data for mech code 80002.
  range_NACOSA <-  "A2:AK226"
  NACOSA_FY23<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1ePof8YITyIUiZn7FfKZeaUje5PR57MbLig95rSMeK-M/edit#gid=329992892"), sheet = "OVC Indicators", range = range_NACOSA) %>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=1)
  
  ## Reading data for current reporting period  for mech code 80008
  NACOSA_FY24<-read_sheet(as_sheets_id("https://docs.google.com/spreadsheets/d/1pbH6R54ioOjf-9rz12TNLJLqpx_9j-6CDQaSi3VDpIo/edit#gid=329992892"), sheet = "FY24_Reporting_Tool") %>%
  mutate(mech_code = as.character(mech_code))%>% mutate(timer=2)
  
  
  ## Consolidating NACOSA's historical and FY2024 data
  NACOSA<-bind_rows(NACOSA_FY23,NACOSA_FY24)
  
  
  #'[Consolidating data from all partners (historical and current data)]
  

  #---------- Update this section as follows:
        #'["AllData" dataframe: Update the dates in the select function to exclude columns with current and future dates (last date of the current month : 12/31/2025) 
        #'["AllData_v1" dataframe: update the column index (cols= ...) to grab the index of the column containing the reporting period in the pivot_longer function 
        #'[ Communicate with partners who have not reported their monthly data as level2 checks will not work without data
        #'[ Share flags with partners in their feedback tracker
        #'[ After flags have been resolved, rerun the script to output clean data and share with DAU]

  AllData <- bind_rows(HIVSA_FY24,PACT_FY24,M2M_FY24, CINDI_FY24, G2G_FY24,NACOSA_FY24, MATCH_FY24) %>% select(-(`9/30/2024`:`12/31/2025`))
  
  AllDatav1<-AllData  %>% select(-(timer) ) %>% 
    pivot_longer(cols= 9:19, values_to ="Value" ,names_to = "period") %>%  
    filter(!is.na(psnu)) %>%
    group_by_if(is_character) %>% summarise(value=sum(Value))
  
  #--------------DQRT
  OutputTableau <-AllDatav1 %>%  mutate(period=mdy(period))%>%
    mutate(community_status="",last_refreshed=today(),End_Date=period,Start_Date=period,period_type="Monthly Cummulative within Quarter") %>%
    group_by( mech_code ,primepartner, psnu,community,indicator ,last_refreshed,disaggregate ,age,otherdisaggregate , community_status, Start_Date,period,period_type ) %>%
    summarise(value=sum(value))%>% mutate(missing=if_else( is.na(value),"Yes","No"))%>%
    select(mech_code ,primepartner, psnu,community,indicator,value ,Start_Date,period,last_refreshed,missing,period_type,age,disaggregate ,otherdisaggregate , community_status) %>%
    mutate(datasource="GoogleDrive",valuetype="Results")  %>%
    mutate(partnershort=if_else(mech_code == "80008"  | mech_code== "80002","NACOSA",primepartner)) 
  
  # Preparing data for DQRT Level 1 and Level 2 checks
  DQRT_temp1 <- OutputTableau %>%
    group_by( mech_code ,primepartner, psnu,community,indicator ,last_refreshed,disaggregate ,age,otherdisaggregate , community_status, Start_Date,period,period_type  ,missing ) %>%
    summarise(value=sum(value))
  
  DQRT_temp2<-DQRT_temp1  %>% filter(age=="<18"| age == "18-20" ) %>% dplyr::group_by(primepartner,mech_code,psnu,community,age,period,indicator) %>%
    dplyr::summarise(value=sum(value))
  
  #'[Level One and Two checks/flags:
  
  level2 <- maditr::dcast(setDT(DQRT_temp2),... ~ indicator,value.var = "value") %>% filter(period == reporting_period) %>% 
    mutate(OVC_HIVSTAT=OVC_HIVSTAT_Negative  +`OVC_HIVSTAT_Positive_Not Receiving ART`+`OVC_HIVSTAT_Positive_Receiving ART`+`OVC_HIVSTAT_Test Not Required`+`OVC_HIVSTAT_Unknown_No HIV Status`)%>%
    select( primepartner,mech_code,psnu,community,period,age,OVC_HIVSTAT_Negative :OVC_HIVSTAT )%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") 
  
  # Indicators not reported
  # OutputTableau <- OutputTableau %>%
  #   filter(period == reporting_period) %>%
  #   mutate(indicator_report = ifelse(
  #     indicator %in% c("OVC_HIVSTAT_Positive_Receiving ART",
  #                      "OVC_HIVSTAT_Positive_Not Receiving ART",
  #                      "OVC_HIVSTAT_Negative",
  #                      "OVC_HIVSTAT_Test Not Required",
  #                      "OVC_HIVSTAT_Unknown_No HIV Status",
  #                      #"OVC_SERV_EWG_Service Lapse",
  #                      "OVC_SERV_Potentially Active",
  #                      "OVC_SERV_Comprehensive",
  #                      "OVC_SERV_Preventive",
  #                      "OVC_SERV_ Preventive",
  #                      "OVC_SERV_DREAMS",
  #                      "OVC_SERV Active Graduation readiness",
  #                     # "OVC_SERV Active referred for TB_Screening",
  #                     # "OVC_SERV Active referred for family planning",
  #                      "OVC_VLR",
  #                      "OVC_VLS",
  #                      "OVC_VL_ELIGIBLE"),
  #     "indicator_reported",
  #     "indicator_notreported"
  #   ))
  # 
  # Missing_ind_MATCH<-OutputTableau %>% filter(indicator_report=="indicator_notreported") %>% mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
  #   filter(mech_code=="87576")%>% filter(period== reporting_period )
  # 
  # Missing_ind_G2G_<-OutputTableau %>%  mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
  #   filter(mech_code=="81904")%>% filter(period== reporting_period )
  
  
  --------------------------------------------------------------------------
  #'[HIVSA Partner feedback]
  
  #Missing data
  Missing_data_HIVSA<-OutputTableau %>%  filter(missing=="Yes") %>% filter(period== reporting_period ) %>%
    mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
    filter(mech_code=="70307")
  

  
  #Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
  check1_HIVSA<-level2 %>% mutate(check1 = OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>%
    select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>%
    mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")
  
  # OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18
  
  #Check 2:HIVSTAT More that OVC COMPREHENSIVE
  check2_HIVSA<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>%
    mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%
    filter(mech_code=="70307")
 
   #Check 3:OVC VLS>OVC_VLR
  check3_HIVSA<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression > OVC_VL_ELIGIBLE")%>%
    select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")
  
  #Check 4:OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18
  check4_HIVSA<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>%
    mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")  
  

  
  #Check 5: This looks at instances where OVC Preventive is reported together with OVC_ELIGIBLE
  check5_HIVSA<-level2 %>% mutate(check5 = OVC_SERV_Preventive >= OVC_VL_ELIGIBLE )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Preventive,OVC_VL_ELIGIBLE,check5) %>% filter(check5==TRUE)  %>%
  mutate(Check_description=" OVC_SERV_Preventive & OVC_VL_ELIGIBLE reported together")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")
  
  #Check 6: This looks at instances where  OVC Serve Comprehensive is reported as zero 
  
  check6_HIVSA<-level2 %>% mutate(check6 = OVC_SERV_Comprehensive ==0 )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,check6) %>% filter(check6==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Comprehensive reported as zero")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70307")
  
  
  #Creating workbook with separate worksheets to document the different L1 and L2 checks
  wb <- createWorkbook()
  write.xlsx(check1_HIVSA,"Dataout/OVC_DQRT_Feedback_HIVSA.xlsx",  sheetName="Check1",append=TRUE)
  
  wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_HIVSA.xlsx")
  
  addWorksheet(wb,"check2")
  writeData(wb,sheet="check2",check2_HIVSA)
  
  addWorksheet(wb,"check3")
  writeData(wb,sheet="check3",x=check3_HIVSA)
  
  addWorksheet(wb,"check4")
  writeData(wb,sheet="check4",x=check4_HIVSA)
  
  addWorksheet(wb,"check5")
  writeData(wb,sheet="check5",x=check5_HIVSA)
  
  addWorksheet(wb,"check6")
  writeData(wb,sheet="check6",x=check6_HIVSA)
  
  addWorksheet(wb,sheetName = "Missing_Data")
  writeData(wb,sheet = "Missing_Data",x=Missing_data_HIVSA)
  
  saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_HIVSA.xlsx",overwrite = T)
 # rm(HIVSA,HIVSA_FY23,HIVSA_FY24,check1_HIVSA,check2_HIVSA,check3_HIVSA,check4_HIVSA,Missing_data_HIVSA)
  
  #'[PACT Partner feedback]
  
  # Checking for missing data
  Missing_data_PACT<-OutputTableau %>% filter(missing=="Yes"  ) %>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
    filter(mech_code=="86130")%>% filter(period== reporting_period )
  
  
  #Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
  check1_PACT<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>%
    mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="86130")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18
  
  #Check 2:HIVSTAT More that OVC COMPREHENSIVE
  check2_PACT<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>%
    mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%
    filter(mech_code=="86130")
  #Check 3:OVC VLS>OVC_VLR
  check3_PACT<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="86130")
  
  #Check 4:HIVSTAT More that OVC COMPREHENSIVE
  check4_PACT<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="86130")
  
  
  #Check 5: This looks at instances where OVC Preventive is reported together with OVC_ELIGIBLE
  check5_PACT<-level2 %>% mutate(check5 = OVC_SERV_Preventive >= OVC_VL_ELIGIBLE )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Preventive,OVC_VL_ELIGIBLE,check5) %>% filter(check5==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Preventive & OVC_VL_ELIGIBLE reported together")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="86130")
  
  #Check 6: This looks at instances where  OVC Serve Comprehensive is reported as zero 
  
  check6_PACT<-level2 %>% mutate(check6 = OVC_SERV_Comprehensive ==0 )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,check6) %>% filter(check6==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Comprehensive reported as zero")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="86130")
  
  #Creating workbook with separate worksheets to document the different L1 and L2 checks
  write.xlsx(check1_PACT,"Dataout/OVC_DQRT_Feedback_PACT.xlsx",  sheetName="Check1",append=TRUE)
  
  wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_PACT.xlsx")
  
  addWorksheet(wb,"check2")
  writeData(wb,sheet="check2",x=check2_PACT)
  
  addWorksheet(wb,"check3")
  writeData(wb,sheet="check3",x=check3_PACT)
  
  addWorksheet(wb,"check4")
  writeData(wb,sheet="check4",x=check4_PACT)
  
  addWorksheet(wb,"check5")
  writeData(wb,sheet="check5",x=check5_PACT)
  
  addWorksheet(wb,"check6")
  writeData(wb,sheet="check6",x=check6_PACT)
  
  
  addWorksheet(wb,sheetName = "Missing_Data")
  writeData(wb,sheet = "Missing_Data",x=Missing_data_PACT)
  
  saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_PACT.xlsx",overwrite = T)
 # rm(check1_PACT,check2_PACT,check3_PACT,check4_PACT,Missing_data_PACT,PACT,PACT_FY23,PACT_FY24)
  #'[PACT END]
  
  
  #'[G2G Partner feedback]
  
  # Checking for missing data
  Missing_data_G2G<-OutputTableau %>% filter(missing=="Yes")%>%   mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
    filter(mech_code=="81904")%>% filter(period== reporting_period )
  
  #Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
  check1_G2G<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>%
    mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="81904")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18
  
  #Check 2:HIVSTAT More that OVC COMPREHENSIVE
  check2_G2G<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%
    filter(mech_code=="81904")
  
  #Check 3:OVC VLS>OVC_VLR
  check3_G2G<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="81904")
  
  #Check 4:HIVSTAT More that OVC COMPREHENSIVE
  check4_G2G<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="81904")
  
  #Check 5: This looks at instances where OVC Preventive is reported together with OVC_ELIGIBLE
  check5_G2G<-level2 %>% mutate(check5 = OVC_SERV_Preventive >= OVC_VL_ELIGIBLE )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Preventive,OVC_VL_ELIGIBLE,check5) %>% filter(check5==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Preventive & OVC_VL_ELIGIBLE reported together")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="81904")
  
  #Check 6: This looks at instances where  OVC Serve Comprehensive is reported as zero 
  
  check6_G2G<-level2 %>% mutate(check6 = OVC_SERV_Comprehensive ==0 )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,check6) %>% filter(check6==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Comprehensive reported as zero")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="81904")
  
  #Creating workbook with separate worksheets to document the different L1 and L2 checks
  write.xlsx(check1_G2G,"Dataout/OVC_DQRT_Feedback_G2G.xlsx",  sheetName="Check1",append=TRUE)
  
  wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_G2G.xlsx")
  
  addWorksheet(wb,"check2")
  writeData(wb,sheet="check2",x=check2_G2G)
  
  addWorksheet(wb,"check3")
  writeData(wb,sheet="check3",x=check3_G2G)
  
  addWorksheet(wb,"check4")
  writeData(wb,sheet="check4",x=check4_G2G)
  
  addWorksheet(wb,"check5")
  writeData(wb,sheet="check5",x=check5_G2G)
  
  addWorksheet(wb,"check6")
  writeData(wb,sheet="check6",x=check6_G2G)
  
  
  addWorksheet(wb,sheetName = "Missing_Data")
  writeData(wb,sheet = "Missing_Data",x=Missing_data_G2G)
  
  saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_G2G.xlsx",overwrite = T)
 # rm(G2G,G2G_FY23,G2G_FY24,check1_G2G,check2_G2G,check3_G2G,check4_G2G,Missing_data_G2G)
  #'[G2G END]
  
  
  #'[M2M Partner feedback]
  
  # Checking for missing data
  Missing_data_M2M <-OutputTableau %>% filter(missing=="Yes") %>%    mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
    filter(mech_code=="80004")%>% filter(period== reporting_period )
  
  #Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
  check1_M2M<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>%
    mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="80004")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18
  
  #Check 2:HIVSTAT More that OVC COMPREHENSIVE
  check2_M2M<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%
    filter(mech_code=="80004")
  
  #Check 3:OVC VLS>OVC_VLR
  check3_M2M<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80004")
  
  #Check 4:HIVSTAT More that OVC COMPREHENSIVE
  check4_M2M<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80004")
  
  #Check 5: This looks at instances where OVC Preventive is reported together with OVC_ELIGIBLE
  check5_M2M<-level2 %>% mutate(check5 = OVC_SERV_Preventive >= OVC_VL_ELIGIBLE )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Preventive,OVC_VL_ELIGIBLE,check5) %>% filter(check5==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Preventive & OVC_VL_ELIGIBLE reported together")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80004")
  
  #Check 6: This looks at instances where  OVC Serve Comprehensive is reported as zero 
  
  check6_M2M<-level2 %>% mutate(check6 = OVC_SERV_Comprehensive ==0 )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,check6) %>% filter(check6==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Comprehensive reported as zero")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80004")
  
  #Creating workbook with separate worksheets to document the different L1 and L2 checks
  write.xlsx(check1_M2M,"Dataout/OVC_DQRT_Feedback_M2M.xlsx",  sheetName="Check1",append=TRUE)
  
  wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_M2M.xlsx")
  
  addWorksheet(wb,"check2")
  writeData(wb,sheet="check2",x=check2_M2M)
  
  addWorksheet(wb,"check3")
  writeData(wb,sheet="check3",x=check3_M2M)
  
  addWorksheet(wb,"check4")
  writeData(wb,sheet="check4",x=check4_M2M)
  
  addWorksheet(wb,"check5")
  writeData(wb,sheet="check5",x=check5_M2M)
  
  addWorksheet(wb,"check6")
  writeData(wb,sheet="check6",x=check6_M2M)
  
  addWorksheet(wb,sheetName = "Missing_Data")
  writeData(wb,sheet = "Missing_Data",x=Missing_data_M2M)
  
  saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_M2M.xlsx",overwrite = T)
  
 # rm(M2M,M2M_FY23,M2M_FY24, check1_M2M,check2_M2M,check3_M2M,check4_M2M,Missing_data_M2M)
  #'[M2M END]
  
  
  #'[CINDI Partner feedback]
  
  #Checking for missing data
  Missing_data_CINDI<-OutputTableau %>%filter(missing=="Yes")%>% mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
    filter(mech_code=="70311")%>% filter(period== reporting_period )
  
  
  #Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
  check1_CINDI<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>%
    mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="70311")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18
  
  #Check 2:HIVSTAT More that OVC COMPREHENSIVE
  check2_CINDI<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%
    filter(mech_code=="70311")
  
  #Check 3:OVC VLS>OVC_VLR
  check3_CINDI<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70311")
  
  #Check 4:HIVSTAT More that OVC COMPREHENSIVE
  check4_CINDI<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70311")
  
  
  #Check 5: This looks at instances where OVC Preventive is reported together with OVC_ELIGIBLE
  check5_CINDI<-level2 %>% mutate(check5 = OVC_SERV_Preventive >= OVC_VL_ELIGIBLE )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Preventive,OVC_VL_ELIGIBLE,check5) %>% filter(check5==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Preventive & OVC_VL_ELIGIBLE reported together")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70311")
  
  #Check 6: This looks at instances where  OVC Serve Comprehensive is reported as zero 
  
  check6_CINDI<-level2 %>% mutate(check6 = OVC_SERV_Comprehensive ==0 )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,check6) %>% filter(check6==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Comprehensive reported as zero")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="70311")
  
  #Creating workbook with separate worksheets to document the different L1 and L2 checks
  write.xlsx(check1_CINDI,"Dataout/OVC_DQRT_Feedback_CINDI.xlsx",  sheetName="Check1",append=TRUE)
  
  wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_CINDI.xlsx")
  
  addWorksheet(wb,"check2")
  writeData(wb,sheet="check2",x=check2_CINDI)
  
  addWorksheet(wb,"check3")
  writeData(wb,sheet="check3",x=check3_CINDI)
  
  addWorksheet(wb,"check4")
  writeData(wb,sheet="check4",x=check4_CINDI)
  
  addWorksheet(wb,"check5")
  writeData(wb,sheet="check5",x=check5_CINDI)
  
  addWorksheet(wb,"check6")
  writeData(wb,sheet="check6",x=check6_CINDI)
  
  
  addWorksheet(wb,sheetName = "Missing_Data")
  writeData(wb,sheet = "Missing_Data",x=Missing_data_CINDI)
  
  saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_CINDI.xlsx",overwrite = T)
  
#  rm(CINDI,CINDI_FY23, CINDI_FY24,check1_CINDI,check2_CINDI,check3_CINDI,check4_CINDI, Missing_data_CINDI)
  #'[CINDI END]
  
  
  #'[NACOSA Partner feedback]
  # Checking for missing data
  Missing_data_NACOSA<-OutputTableau %>% filter(missing=="Yes") %>% mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
    filter(mech_code=="80008")%>% filter(period== reporting_period )
  
  #Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
  check1_NACOSA<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>%
    mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="80008")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18
  
  #Check 2:HIVSTAT More that OVC COMPREHENSIVE
  check2_NACOSA<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%
    filter(mech_code=="80008")
  #Check 3:OVC VLS>OVC_VLR
  check3_NACOSA<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80008")
  
  #Check 4:HIVSTAT More that OVC COMPREHENSIVE
  check4_NACOSA<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80008")
  
  #Check 5: This looks at instances where OVC Preventive is reported together with OVC_ELIGIBLE
  check5_NACOSA<-level2 %>% mutate(check5 = OVC_SERV_Preventive >= OVC_VL_ELIGIBLE )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Preventive,OVC_VL_ELIGIBLE,check5) %>% filter(check5==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Preventive & OVC_VL_ELIGIBLE reported together")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80008")
  
  #Check 6: This looks at instances where  OVC Serve Comprehensive is reported as zero 
    check6_NACOSA<-level2 %>% mutate(check6 = OVC_SERV_Comprehensive ==0 )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,check6) %>% filter(check6==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Comprehensive reported as zero")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="80008")
  
  #Creating workbook with separate worksheets to document the different L1 and L2 checks
  write.xlsx(check1_NACOSA,"Dataout/OVC_DQRT_Feedback_NACOSA.xlsx",  sheetName="Check1",append=TRUE)
  
  wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_NACOSA.xlsx")
  
  addWorksheet(wb,"check2")
  writeData(wb,sheet="check2",x=check2_NACOSA)
  
  addWorksheet(wb,"check3")
  writeData(wb,sheet="check3",x=check3_NACOSA)
  
  addWorksheet(wb,"check4")
  writeData(wb,sheet="check4",x=check4_NACOSA)
  
  addWorksheet(wb,"check5")
  writeData(wb,sheet="check5",x=check5_NACOSA)
  
  addWorksheet(wb,"check6")
  writeData(wb,sheet="check6",x=check6_NACOSA)
  
  
  addWorksheet(wb,sheetName = "Missing_Data")
  writeData(wb,sheet = "Missing_Data",x=Missing_data_NACOSA)
  #'[NACOSA END]
  
  saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_NACOSA.xlsx",overwrite = T)
#  rm(NACOSA,NACOSA_FY23,NACOSA_FY24, check1_NACOSA,check2_NACOSA, check3_NACOSA, check4_NACOSA, Missing_data_NACOSA)
  
  #'[MATCH Partner feedback]
  #
  # Checking for missing data
  Missing_data_MATCH<-OutputTableau %>% filter(missing=="Yes") %>% mutate(Dead_line="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%
    filter(mech_code=="87576")%>% filter(period== reporting_period )
  
  #Check 1 :This looks at instances where there number Eligible for VL is more than those receiving ART .
  check1_MATCH<-level2 %>% mutate(check1=OVC_VL_ELIGIBLE>`OVC_HIVSTAT_Positive_Receiving ART`,checkdescription="Number eligible for VL is more than those receiving ART") %>% select(primepartner,mech_code,psnu,community,period,age,`OVC_HIVSTAT_Positive_Receiving ART`,OVC_VL_ELIGIBLE ,check1,checkdescription) %>% filter(check1==TRUE) %>%
    mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%  filter(mech_code=="87576")# OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18
  
  #Check 2:HIVSTAT More that OVC COMPREHENSIVE
  check2_MATCH<-level2 %>% mutate(check2=OVC_HIVSTAT>OVC_SERV_Comprehensive) %>% select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,OVC_HIVSTAT ,check2) %>% mutate(check_description="OVC_HIVSTAT is greater that OVC COMPREHENSIVE") %>% filter(check2==TRUE) %>%   mutate(Deadline="", Status="", Partner_Comment="", Cleared_for_analytics="") %>%
    filter(mech_code=="87576")
  #Check 3:OVC VLS>OVC_VLR
  check3_MATCH<-level2 %>% mutate(check3=OVC_VLS>OVC_VLR ,checkdescription="# OVC_VL Suppression >OVC_VL_ELIGIBLE")%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VLR ,check3,checkdescription) %>% filter(check3==TRUE) %>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="87576")
  
  #Check 4:HIVSTAT More that OVC COMPREHENSIVE
  check4_MATCH<-level2 %>% mutate(check4=OVC_VLS>OVC_VL_ELIGIBLE )%>% select(primepartner,mech_code,psnu,community,period,age,OVC_VLS,OVC_VL_ELIGIBLE,check4) %>% filter(check4==TRUE) %>% mutate(Check_description=" OVC_HIVSTAT_Pos_Rec ART <18<OVC_VL_ELIGIBLE <18")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="87576")
  
  #Check 5: This looks at instances where OVC Preventive is reported together with OVC_ELIGIBLE
  check5_MATCH<-level2 %>% mutate(check5 = OVC_SERV_Preventive >= OVC_VL_ELIGIBLE )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Preventive,OVC_VL_ELIGIBLE,check5) %>% filter(check5==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Preventive & OVC_VL_ELIGIBLE reported together")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="87576")
  
  #Check 6: This looks at instances where  OVC Serve Comprehensive is reported as zero 
  check6_MATCH<-level2 %>% mutate(check6 = OVC_SERV_Comprehensive ==0 )  %>% 
    select(primepartner,mech_code,psnu,community,period,age,OVC_SERV_Comprehensive,check6) %>% filter(check6==TRUE)  %>%
    mutate(Check_description=" OVC_SERV_Comprehensive reported as zero")%>%
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="") %>%  filter(mech_code=="87576")
  
  #Creating workbook with separate worksheets to document the different L1 and L2 checks
  write.xlsx(check1_MATCH,"Dataout/OVC_DQRT_Feedback_MATCH.xlsx",  sheetName="Check1",append=TRUE)
  
  wb<-loadWorkbook("Dataout/OVC_DQRT_Feedback_MATCH.xlsx")
  
  addWorksheet(wb,"check2")
  writeData(wb,sheet="check2",x=check2_MATCH)
  
  addWorksheet(wb,"check3")
  writeData(wb,sheet="check3",x=check3_MATCH)
  
  addWorksheet(wb,"check4")
  writeData(wb,sheet="check4",x=check4_MATCH)
  
  addWorksheet(wb,"check5")
  writeData(wb,sheet="check5",x=check5_MATCH)
  
  addWorksheet(wb,"check6")
  writeData(wb,sheet="check6",x=check6_MATCH)
  
  addWorksheet(wb,sheetName = "Missing_Data")
  writeData(wb,sheet = "Missing_Data",x=Missing_data_MATCH)
  saveWorkbook(wb,"Dataout/OVC_DQRT_Feedback_MATCH.xlsx",overwrite = T)
#  rm(MATCH_FY24, check1_MATCH,check2_MATCH, check3_MATCH, check4_MATCH, Missing_data_MATCH)
  
  #'[MATCH END]
  #'
  
  #'[OVC Dashboard output file]
  #'
  print(distinct(AllDatav1,primepartner,mech_code ))
  
  
  OutputTableau <- OutputTableau %>% mutate( mech_code=case_when(mech_code=="80002" | mech_code=="80008 "~"80008",
                                                                 mech_code=="14631"   |mech_code=="86130"~"86130",.default =  mech_code    ))
  
  
  write.xlsx(OutputTableau,"Dataout/OVC_OutputTableauv3.xlsx",sheetName="1OutputTableau",apppend=T)
  
  
  
  
