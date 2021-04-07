library(readr)
library(readxl)
library(data.table)
library(R.utils)
library(lubridate)
library(dplyr)
library(tidyr)

unzip('quiz_data.zip')

raw <- fread('data/daily_SPEC_2014.csv.bz2')

daily <- fread('data/daily_SPEC_2014.csv.bz2',
                    select= c('Parameter Name',
                              'Arithmetic Mean',
                              'State Code',
                              'State Name',
                              'County Code',
                              'Site Num',
                              'Latitude',
                              'Longitude',
                              'Date Local'))

colnames(daily) <- gsub(" +", "_", colnames(daily))

land <- read_xlsx('data/aqs_sites.xlsx')
colnames(land) <- gsub(" +", "_", colnames(land))

####

q1 <- daily %>% 
  filter(State_Name == 'Wisconsin' 
         & Parameter_Name == 'Bromine PM2.5 LC') %>% 
  summarise(mean = mean(Arithmetic_Mean, na.rm=TRUE))

q2 <- daily %>% 
  group_by(State_Name,Site_Num,Date_Local,Parameter_Name) %>% 
  summarise(mean = mean(Arithmetic_Mean, na.rm=TRUE))%>%
  arrange(desc(mean))

q3 <- daily %>% 
  filter(Parameter_Name == 'Sulfate PM2.5 LC') %>% 
  group_by(State_Code, County_Code, Site_Num) %>% 
  summarise(mean = mean(Arithmetic_Mean, na.rm=TRUE))

q4 <- daily %>% 
  filter(Parameter_Name == 'EC PM2.5 LC TOR' & 
         State_Name %in% c('California', 'Arizona')) %>% 
  group_by(State_Name) %>% 
  summarise(mean = mean(Arithmetic_Mean, na.rm=TRUE)) %>% 
  pivot_wider(names_from = State_Name, values_from = mean) %>% 
  mutate(mean_dif = California - Arizona)
         
q5 <- daily %>% 
  filter(Parameter_Name == 'OC PM2.5 LC TOR',
         Longitude < -100) %>% 
  summarise(median = median(Arithmetic_Mean, na.rm=TRUE))

table(land$'Land_Use')

q6 <- land %>% 
  filter(Land_Use == 'RESIDENTIAL' & Location_Setting == 'SUBURBAN') %>% 
  tally()

### q7

site <- land %>% 
  select(State_Code, County_Code, Site_Number, Latitude, Longitude, Land_Use, 
       Location_Setting) %>% 
  rename(Site_Num = Site_Number)

daily <- daily %>% 
  mutate(County_Code = as.numeric(County_Code),
         Site_Num = as.numeric(Site_Num),
         State_Code = as.numeric(State_Code))

#leftjoin_LL <- left_join(daily,site, by= c('Latitude','Longitude'))
leftjoin_SCS <- left_join(daily,site, by=c('State_Code','County_Code','Site_Num'))

q7 <- leftjoin_SCS %>% 
  filter(Parameter_Name=='EC PM2.5 LC TOR'
         & (Land_Use == 'RESIDENTIAL' & Location_Setting == 'SUBURBAN')) %>% 
  summarise(median = median(Arithmetic_Mean, na.rm=TRUE))

q8 <- leftjoin_SCS %>% 
  filter(Parameter_Name=='Sulfate PM2.5 LC'
         & Land_Use == 'COMMERCIAL') %>%
  mutate(month = month(Date_Local,label=TRUE)) %>% 
  group_by(month) %>% 
  summarise(mean = mean(Arithmetic_Mean, na.rm=TRUE)) %>% 
  arrange(desc(mean))

q9 <- daily %>% 
  filter(State_Code ==6 & County_Code ==65 & Site_Num ==8001 &
           Parameter_Name %in% c('Sulfate PM2.5 LC','Total Nitrate PM2.5 LC')) %>% 
  group_by(Parameter_Name,Date_Local) %>% 
  summarise(mean = mean(Arithmetic_Mean, na.rm=TRUE)) %>% 
  group_by(Date_Local) %>%
  summarise(total = sum(mean)) %>% 
  tally(total>10)



q10<- daily %>%
  filter(Parameter_Name %in% c('Sulfate PM2.5 LC','Total Nitrate PM2.5 LC')) %>% 
  group_by(State_Code,County_Code,Site_Num,Parameter_Name,Date_Local) %>% 
  select(State_Code,County_Code,Site_Num,Parameter_Name,Date_Local,Arithmetic_Mean) %>%
  summarise(mean = mean(Arithmetic_Mean, na.rm=TRUE)) %>% 
  pivot_wider(names_from= Parameter_Name,values_from = mean) %>% 
  group_by(State_Code, County_Code, Site_Num) %>%
  summarise(correlation = cor('Sulfate PM2.5 LC', 'Total Nitrate PM2.5 LC')) %>%
  arrange(desc(correlation))

summary(q10)


q11<- daily %>%
  filter(Parameter_Name %in% c('Sulfate PM2.5 LC', 'Total Nitrate PM2.5 LC')) %>%
  group_by(State_Code, County_Code, Site_Num, Parameter_Name, Date_Local) %>%
  select(State_Code, County_Code, Site_Num, Parameter_Name,Date_Local,
         Arithmetic_Mean) %>%
  summarize(mean = mean(Arithmetic_Mean, na_rm = TRUE)) %>%
  spread(Parameter_Name, mean) %>%
  group_by(State_Code, County_Code, Site_Num)%>%
  summarize(correlation = cor('Sulfate PM2.5 LC', 'Total Nitrate PM2.5 LC', use = "complete.obs"))
  
cor(q11$'Sulfate PM2.5 LC', q11$'Total Nitrate PM2.5 LC',use = "complete.obs")

q11 %>% 
  summarize(cor('Sulfate PM2.5 LC', 'Total Nitrate PM2.5 LC',use = "complete.obs"))
  

summarise(correlation = cor('Sulfate PM2.5 LC', 'Total Nitrate PM2.5 LC', use = "complete.obs")) %>%
  arrange(desc(correlation))


