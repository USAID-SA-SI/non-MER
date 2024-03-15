
# Title: Level Two process
# Author: C. Trapence
# Date:2023-07-11
# Updated:2024:02:21
# Updated by Rosaline
# Load Required libraries
# Jerome to Review and Test
# Red text symbolizes comments

#######################################################################################################################
#  source files used in the code include:                                                                            #
#              1) Google Sheets                                                                                      #


if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, here,anydate,  gargle,glamr,anytime,patchwork,maditr, googledrive,googlesheets4,openxlsx,lubridate,janitor,readr, esquisse, flextable,stringr,sqldf)

load_secrets()


#'[RTC]
  #Reading historical data
RTC_70290_a<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Hc99weOc2CjGCKs3pruJ5_YetSwn02zZp6AhCjj6N1g/edit#gid=2092521029'), sheet = "4. Reporting tab") %>% mutate(kptype="")%>% mutate(disaggregate = "") %>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,disaggregate,'10/31/2021':'12/31/2024')

#Reading data for current reporting FY24  
RTC_70290_b<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Hc99weOc2CjGCKs3pruJ5_YetSwn02zZp6AhCjj6N1g/edit#gid=2109560495'), sheet = "FY24_Reporting tab") %>% mutate(kptype="")%>%
   mutate(disaggregate = if_else(disaggregate== "N/A", "",disaggregate))  %>%mutate(disaggregate=if_else(is.na(disaggregate),"",disaggregate)) %>% 

  select(indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,disaggregate,'10/31/2023':'12/31/2024') 

 
#Consolidating RTC's historical and current data 
RTC<- bind_rows(RTC_70290_a, RTC_70290_b)

#'[MatCH]
#'# MatCH only starts reporting on mech code 87576 from Jan 2024 so no data available for Oct-Dec 2023
MatCH_81902<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1Dfjvf-K7O0q6i6vHQkzNCFfoSkbtMtB7NvT703GvCSY/edit#gid=1044282265k'), sheet = "4. Reporting tab") %>% mutate(kptype="")%>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,'10/31/2021':'12/31/2023')

MatCH_87576<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1fMq0221ZgBqxjj3qUR9_DA9ShqkNjnwjGh4qQlpfML8/edit#gid=2092521029'), sheet = "FY24_Reporting tab") %>% mutate(kptype="")%>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,'1/31/2024':'12/31/2024')

MatCH_87575<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1zsGrxEpotzbUAQ1XuOgFjXwCZiIkEvvyabepeFQuZFU/edit#gid=1105257127'), sheet = "FY24_Reporting tab") %>% mutate(kptype="")%>% select(
  indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,'1/31/2024':'12/31/2024')

MatCH<- bind_rows(MatCH_81902, MatCH_87576, MatCH_87575)

#'[Broadreach]
 #Reading historical data
BRCH_70287_a<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1vjfCZ2QIjKS2ASfsWq4jHGNot38W_FAiaslOp4YR0sE/edit#gid=1044282265'), sheet = "4. Reporting tab") %>% mutate(kptype="")%>% mutate(disaggregate = "")%>% 
  select( indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,disaggregate,'10/31/2021':'9/30/2023') 
 #Reading data for current reporting FY224
BRCH_70287_b<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1vjfCZ2QIjKS2ASfsWq4jHGNot38W_FAiaslOp4YR0sE/edit#gid=1044282265'), sheet = "FY24_Reporting tab") %>% mutate(kptype="")%>%mutate(disaggregate=if_else(is.na(disaggregate),"",disaggregate)) %>%
  select(indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,disaggregate,sex,'10/31/2023':'12/31/2024')
 
#Consolidating BRCH's historical and current data for current reporting FY24
BRCH<-bind_rows(BRCH_70287_a, BRCH_70287_b)

BRCH_longer<- BRCH %>% pivot_longer(13:51, names_to= "period", values_to = "value")

#'[ANOVA]
 #Reading historical data                                     
ANOVA_70310_a<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1hJUAaSludW5SbXeng51oc2tbf6pCYHyp2QvxLUq5tYQ/edit#gid=2092521029'), sheet = "4. Reporting tab") %>%
  mutate(kptype="") %>%   mutate(disaggregate = "")%>% 
  select(indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,disaggregate, sex,'10/31/2021':'9/30/2023')
 #Reading data for current reporting FY24 
ANOVA_70310_b<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1hJUAaSludW5SbXeng51oc2tbf6pCYHyp2QvxLUq5tYQ/edit#gid=435605647'), sheet = "FY24_Reporting tab") %>% mutate(kptype="")%>% 
  mutate(disaggregate = if_else(disaggregate== "N/A", "",disaggregate))  %>%mutate(disaggregate=if_else(is.na(disaggregate),"",disaggregate)) %>% 
  select( indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,disaggregate, sex,'10/31/2023':'12/31/2024')
 
#Reading data for new mechanism(87577) for current reporting FY24
ANOVA_87577 <-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1VfT-U-evaHeREufdMlvx58sCOGuD4GU4U-ZZw7bY4KM/edit#gid=2092521029'), sheet = "4. Reporting tab") %>% mutate(kptype="")%>%
  select(indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,disaggregate, sex,'1/31/2024':'12/31/2024') %>% mutate(disaggregate = if_else(disaggregate== "N/A", "",disaggregate))  %>%mutate(disaggregate=if_else(is.na(disaggregate),"",disaggregate))

 #Consolidating ANOVA's historical and current data for current reporting FY24
ANOVA_70310<-bind_rows(ANOVA_70310_a, ANOVA_70310_b)


#'[WRHI]
#Reading Key population historical data and FY24 data for mech code 70306
WRHI_70306<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1CxN09HcB711uHgRAaO8HfPOgX86329f4RRKiKMiavr8/edit#gid=2092521029'), sheet = "4. Reporting tab") %>% mutate(disaggregate = "")%>% 
  select(indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,disaggregate,'10/31/2021':'12/31/2024')

 #Reading 70301 APACE historical data   
WRHI_70301_a<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1xLat8u9GZLgtpwRJWpe4WwAZ1eoulXXUtgxm7r3dsqo/edit#gid=20925210295'), sheet = "4. Reporting tab")  %>% mutate(kptype="")%>% mutate(disaggregate = "")%>% 
  select( indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,disaggregate,'10/31/2021':'12/31/2024')
#Reading 70301 APACE data for current reporting FY24
WRHI_70301_b<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1xLat8u9GZLgtpwRJWpe4WwAZ1eoulXXUtgxm7r3dsqo/edit#gid=20925210295'), sheet = "FY24_Reporting tab")  %>% mutate(kptype="")%>% 
  mutate(disaggregate=if_else(is.na(disaggregate),"",disaggregate)) %>% 
  select( indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,disaggregate,'10/31/2023':'12/31/2024')

#Consolidating WRHI's historical and current data for current reporting FY24 for mech code 70301
WRHI_70301<- bind_rows(WRHI_70301_a, WRHI_70301_b)

#Reading 80007 historical data                                   
WRHI_80007_a<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1oOVQ2swAuXUPmBrI0UOo8mjaTEB87IYC2QSKZ36hPDA/edit#gid=1105257127'), sheet = "4. Reporting tab") %>% mutate(kptype="") %>% mutate(disaggregate = "")%>% 
  select( indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,disaggregate,sex,'10/31/2021':'12/31/2024')
 
#Reading 80007 data for current reporting FY24  
WRHI_80007_b<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1oOVQ2swAuXUPmBrI0UOo8mjaTEB87IYC2QSKZ36hPDA/edit#gid=1105257127'), sheet = "FY24_Reporting tab") %>% mutate(kptype="") %>%  
  mutate(disaggregate=if_else(is.na(disaggregate),"",disaggregate)) %>% 
  select( indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,disaggregate,sex,'10/31/2023':'12/31/2024')


#Consolidating WRHI's historical and current data for current reporting FY24 for mech code 80007
WRHI_80007<- bind_rows(WRHI_80007_a, WRHI_80007_b)

#'[FHI 360]
# Reading historic and current data for FHI 360
FHI360<-read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1H1hv4EK8RYTrA1OOn38AZtLeJ0ovKbC_mNSAGgJiJ0c/edit#gid=1044282265'), sheet = "4. Reporting tab")%>% mutate(disaggregate = "")%>% 
  select(indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,disaggregate,sex,'10/31/2021':'12/31/2024')


#'[Engage Men's Health]
#Added new partner
EngageMensHealth<- read_sheet(as_sheets_id('https://docs.google.com/spreadsheets/d/1xF-nuS6WR19RgJ_dFjfoOjdC_b3zsC6lSN--JCiXHD8/edit#gid=2092521029'),sheet = "4. Reporting tab") %>% mutate(kptype="")%>% mutate(disaggregate = "")%>% 
  select( indicator,partner,mechanismid,country,snu1,psnu,snu1id,psnuuid,kptype,age,sex,disaggregate, '10/31/2023':'12/31/2024') %>% filter (sex == "Male")

#Historical Data
All_PrEP<-bind_rows(WRHI_70301,WRHI_70306,WRHI_80007,FHI360,ANOVA_70310,ANOVA_87577,MatCH, BRCH,RTC,EngageMensHealth) 

#FY24 ONLY
All_PrEPv2<-bind_rows(WRHI_70306,WRHI_70301_b,WRHI_80007_b,FHI360,ANOVA_70310_b,ANOVA_87577,MatCH_87575, MatCH_87576,BRCH_70287_b,RTC_70290_b,EngageMensHealth) 


#Historical Data
#'[for DAU PrEP dashboard Raw file
All_PrEP_longer_raw <- All_PrEP %>% pivot_longer(13:52, names_to= "period", values_to = "value")  

All_PrEP_longer_raw <- All_PrEP_longer_raw%>% mutate(period = mdy(period)) %>% 
  group_by(indicator,partner,mechanismid,country,snu1 ,psnu,snu1id,psnuuid,kptype,age,sex,disaggregate,period) %>% summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  mutate(last_refreshed=today(),End_Date=period,Start_Date=period,period_type="Monthly Cummulative within Quarter")

#FY24 Data Only
All_PrEP_longerv2<- All_PrEPv2 %>% pivot_longer(13:52, names_to= "period", values_to = "value") 

All_PrEP_longerv2 <-All_PrEP_longerv2 %>% mutate(period=mdy(period)) %>%  group_by( indicator,partner,mechanismid,country,snu1 ,psnu,snu1id,psnuuid,kptype,age,sex,disaggregate,period) %>%
  summarize(value=sum(value)) %>% mutate(missing=if_else(!is.na(value),"No","Yes")) %>% 
  mutate(check1=if_else(sex=="Female" & disaggregate!="" & !is.na(value)  ,"✔","NA")) 

#Add check to flag zero reporting across all age bands

#'[FLAGGING INCONSISTENCIES IN DATA FOR CORRECTION 

#'[Check1: "Check for females with  specified pregnancy/breastfeeding disaggregate but with missing values.",
#'[Check2: "Check for males with specified pregnancy/breastfeeding disaggregate.",
#'[Check3: "Verify if reported PrEP eligibility exceeds screenings.",
#'[Check4: "Confirm if reported new PrEP enrollments exceed screenings.",
#'[Check5: "Assess if new PrEP enrollments surpass eligible individuals.",
#'[Check6: "Validate if new PrEP enrollments surpass those with confirmed HIV test results."
#'
#Historical Data
All_PrEP_longer_hist<- All_PrEP %>% pivot_longer(13:52, names_to= "period", values_to = "value") %>% 
mutate(period=mdy(period)) %>%  group_by( indicator,partner,mechanismid,country,snu1 ,psnu,snu1id,psnuuid,kptype,age,sex,disaggregate,period) %>%
summarize(value=sum(value)) %>% mutate(missing=if_else(!is.na(value),"No","Yes")) %>% 
mutate(check1=if_else(sex=="Female" & disaggregate!="" & !is.na(value)  ,"✔","NA")) 

#FY24 Data only
All_PrEP_widerv2<- All_PrEP_longerv2 %>% pivot_wider(names_from = indicator,values_from = value) %>%
  filter(period>floor_date((ymd(today()-months(1))),'month') & period<=floor_date(ymd(today()), 'month')-days(1)) %>%
  mutate(check2=if_else(sex=="Male" & disaggregate!="" & any(!is.na(c( PrEP_ELIGIBLE, PrEP_NEW , PrEP_RETURN_1MONTH,PrEP_RETURN_4MONTHS,
                                                                       PrEP_RETURN_7MONTHS,PrEP_SCREEN,PrEP_TST_NEG,PrEP_TST_POS))) ,"Clean record","NA")) %>% 
  mutate(check3=if_else(PrEP_ELIGIBLE>PrEP_SCREEN ,"Clean record","✔")) %>% 
  mutate(check4=if_else(PrEP_NEW>PrEP_SCREEN ,"Clean record","✔")) %>% 
  mutate(check5=if_else(PrEP_NEW>PrEP_ELIGIBLE ,"Clean record","✔")) %>% 
  mutate(check6=if_else(PrEP_NEW>(PrEP_TST_NEG+PrEP_TST_POS) ,"Clean record","✔")) %>%   
  select(partner:period,missing,check1,check2,check3,check4,check5,check6,PrEP_ELIGIBLE:PrEP_TST_POS)

#Historical Data only

All_PrEP_wider<- All_PrEP_longer_hist %>% pivot_wider(names_from = indicator,values_from = value) %>%
filter(period>floor_date((ymd(today()-months(2))),'month') & period<=floor_date(ymd(today()), 'month')-days(1)) %>%
mutate(check2=if_else(sex=="Male" & disaggregate!="" & any(!is.na(c( PrEP_ELIGIBLE, PrEP_NEW , PrEP_RETURN_1MONTH,PrEP_RETURN_4MONTHS,
PrEP_RETURN_7MONTHS,PrEP_SCREEN,PrEP_TST_NEG,PrEP_TST_POS))) ,"Clean record","NA")) %>% 
mutate(check3=if_else(PrEP_ELIGIBLE>PrEP_SCREEN ,"Clean record","✔")) %>% 
mutate(check4=if_else(PrEP_NEW>PrEP_SCREEN ,"Clean record","✔")) %>% 
mutate(check5=if_else(PrEP_NEW>PrEP_ELIGIBLE ,"Clean record","✔")) %>% 
mutate(check6=if_else(PrEP_NEW>(PrEP_TST_NEG+PrEP_TST_POS) ,"Clean record","✔")) %>%   
select(partner:period,missing,check1,check2,check3,check4,check5,check6,PrEP_ELIGIBLE:PrEP_TST_POS)

Data_dictionary <- data.frame(
Check = c("Check1", "Check2", "Check3", "Check4","Check5", "Check6"),
Check_description = c(
  "Check for females without specified pregnancy/breastfeeding disaggregate but with values.",
  "Check for males with specified pregnancy/breastfeeding disaggregate.",
  "Verify if reported PrEP eligibility exceeds screenings.",
  "Confirm if reported new PrEP enrollments exceed screenings.",
  "Assess if new PrEP enrollments surpass eligible individuals.",
  "Validate if new PrEP enrollments surpass those with confirmed HIV test results."
))
  
  
#'[for DAU PrEP dashboard Raw file

# Append1<-gather(All_PrEP,period,value,`10/31/2021` : `12/31/2024`) %>% mutate(period=mdy(period)) %>%
#   mutate(last_refreshed=today(),End_Date=period,Start_Date=period,period_type="Monthly Cummulative within Quarter") %>%
#  mutate(value=(value))%>%  group_by( indicator ,partner, mechanismid,country, snu1,snu1id,psnu,psnuuid , kptype,age,sex,disaggregate, period ) %>%
#   summarise(value=sum(value)) 
  



#'[WRHI_70301 Feedback Tracker]

WRHI_70301_checks<-All_PrEP_widerv2 %>% filter(mechanismid==70301)%>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

# Save workbook to working directory

#Creating workbook with separate worksheets to document the different checks 
wb <- createWorkbook()
write.xlsx(WRHI_70301_checks,"Dataout/PrEP_DQRT_Feedback_WRHI_70301.xlsx",  sheetName="Validations",append=TRUE)

wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_WRHI_70301.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_WRHI_70301.xlsx",overwrite = T)


#'[WRHI_70306 Feedback Tracker]

WRHI_70306_checks<-All_PrEP_widerv2 %>% filter(mechanismid==70306) %>% 
  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

wb <- createWorkbook()

write.xlsx(WRHI_70306_checks,"Dataout/PrEP_DQRT_Feedback_WRHI_70306.xlsx",  sheetName="Validations",append=TRUE)

wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_WRHI_70306.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_WRHI_70306.xlsx",overwrite = T)


#'[WRHI_80007 Feedback Tracker]

WRHI_80007_checks<-All_PrEP_widerv2 %>%  
  filter(mechanismid==80007)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")
wb <- createWorkbook()

write.xlsx(WRHI_80007_checks,"Dataout/PrEP_DQRT_Feedback_WRHI_80007.xlsx",  sheetName="Validations",append=TRUE)

wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_WRHI_80007.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_WRHI_80007.xlsx",overwrite = T)

# 


#'[RTC Feedback Tracker]
RTC_checks<-All_PrEP_widerv2 %>% 
  filter(mechanismid==70290) %>% 
    mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

wb <- createWorkbook()
write.xlsx(RTC_checks,"Dataout/PrEP_DQRT_Feedback_RTC.xlsx",  sheetName="Validations",append=TRUE)

wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_RTC.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_RTC.xlsx",overwrite = T)

#'[BRCH Feedback Tracker]

BRCH_checks<-All_PrEP_widerv2 %>%  
  filter(mechanismid==70287)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")
wb <- createWorkbook()

write.xlsx(BRCH_checks,"Dataout/PrEP_DQRT_Feedback_BRCH.xlsx",  sheetName="Validations",append=TRUE)

wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_BRCH.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_BRCH.xlsx",overwrite = T)


#'[MaTCH Feedback Tracker]
MaTCH_87576_checks<-All_PrEP_widerv2 %>% 
  filter(mechanismid==87576)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

wb <- createWorkbook()

write.xlsx(MaTCH_87576_checks,"Dataout/PrEP_DQRT_Feedback_MaTCH_87576.xlsx",  sheetName="Validations",append=TRUE)
wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_MaTCH_87576.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_MaTCH_87576.xlsx",overwrite = T)


MaTCH_87575_checks<-All_PrEP_widerv2 %>% 
  filter(mechanismid==87575)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

wb <- createWorkbook()

write.xlsx(MaTCH_87575_checks,"Dataout/PrEP_DQRT_Feedback_MaTCH_87575.xlsx",  sheetName="Validations",append=TRUE)
wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_MaTCH_87575.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_MaTCH_87575.xlsx",overwrite = T)

#'[FHI360 Feedback Tracker]
#''#Filter applied to exclude Eastern Cape and Gauteng where partner is no longer reporting
FHI360_checks<-All_PrEP_widerv2 %>%
  filter(mechanismid==82199)%>% 
  filter(sex== "Male") %>% filter(snu1== "wc Western Cape Province") %>%mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")
  
wb <- createWorkbook()

write.xlsx(FHI360_checks,"Dataout/PrEP_DQRT_Feedback_FHI360.xlsx",  sheetName="Validations",append=TRUE)
wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_FHI360.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_FHI360.xlsx",overwrite = T)

#'[Engage Men's Health Feedback Tracker]

#'#Filter applied to exclude  females
EngageMensHealth_checks<-All_PrEP_widerv2 %>%
  filter(mechanismid==86131)%>% filter(sex== "Male") %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")
  
wb <- createWorkbook()
write.xlsx(EngageMensHealth_checks,"Dataout/PrEP_DQRT_Feedback_EngageMensHealth.xlsx",  sheetName="Validations",append=TRUE)
wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_EngageMensHealth.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_EngageMensHealth.xlsx",overwrite = T)

#'[ANOVA Feedback Tracker]

ANOVA_87577_checks<-All_PrEP_widerv2 %>% 
  filter(mechanismid==87577)%>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

wb <- createWorkbook()
write.xlsx(ANOVA_87577_checks,"Dataout/PrEP_DQRT_Feedback_ANOVA_87577.xlsx",  sheetName="Validations",append=TRUE)
wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_ANOVA_87577.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_ANOVA_87577.xlsx",overwrite = T)

#'[ANOVA Feedback Tracker]

ANOVA_70310_checks<-All_PrEP_widerv2 %>% 
  filter(mechanismid==70310) %>%  mutate(Deadline="", Status="", Partners_Comments="", Cleared_for_analytics="")

wb <- createWorkbook()
write.xlsx(ANOVA_70310_checks,"Dataout/PrEP_DQRT_Feedback_ANOVA_70310.xlsx",  sheetName="Validations",append=TRUE)
wb<-loadWorkbook("Dataout/PrEP_DQRT_Feedback_ANOVA_70310.xlsx")

addWorksheet(wb,"Data_dictionary")
writeData(wb,sheet="Data_dictionary",x=Data_dictionary)

saveWorkbook(wb,"Dataout/PrEP_DQRT_Feedback_ANOVA_70310.xlsx",overwrite = T)

#rm( ANOVA_70310_checks,ANOVA_87577_checks,  BRCH_checks,  EngageMensHealth_checks,  FHI360_checks,  MaTCH_81902_checks,
 #   WRHI_70301_checks,  WRHI_70306_checks,  WRHI_80007_checks,MatCH,FHI360,BRCH)


#write_csv(AGYW_DREAMS,"AGYW_PREV_Final.csv")


RawData<-All_PrEP_longer_raw %>% select(indicator,	partner,	mechanismid	,country	,snu1	,snu1id,disaggregate,	psnu,	psnuuid,	age,	sex	,period,	kptype,	value)


filename<-paste(Sys.Date(), "RawData", ".xlsx")

openxlsx::write.xlsx(RawData, here("Dataout",filename),sheetName="RawData")



