library(tidyverse)
library(readxl)
#IV and DV cleaning
d <- read_excel("data/GCDF_3.0.xlsx")
##DV
chinaaid <- d %>% 
  select('Financier Country', 'Recipient','Recipient Region','Commitment Year',
         'Flow Type Simplified','Flow Class','Sector Code','Sector Name',
         'Adjusted Amount (Constant USD 2021)') %>% 
  rename("Donor" = "Financier Country", "Region" = "Recipient Region",
         "Time" = "Commitment Year", "FlowType" = "Flow Type Simplified",
         "value" = "Adjusted Amount (Constant USD 2021)",
         "Sectorcode" = "Sector Code","Sector"="Sector Name") %>% 
  filter(Region == 'Africa') %>% 
  mutate(Value = value/1000000)
  

##IV
d_usoda <- read_csv("data/US_ODA.csv")
usoda <- d_usoda %>% 
  select('Recipient','Donor','Aid type','TIME','Value','Amount type') %>% 
  rename("Aidtype" = "Aid type", "Time" = "TIME", 
         "Amounttype" = "Amount type") %>% 
  filter(Amounttype == "Constant Prices")

d_usgrant <- read_csv("data/US_grants.csv")  
usgrant <- d_usgrant %>% 
  select('Recipient','Donor','Aid type','TIME','Value','Amount type') %>% 
  rename("Aidtype" = "Aid type", "Time" = "TIME", 
         "Amounttype" = "Amount type") %>% 
  filter(Amounttype == "Constant Prices")

d_total <- read_csv("data/US_total_official_flows.csv")
ustotal <- d_total %>% 
  select('Recipient','Donor','Aid type','TIME','Value','Amount type') %>% 
  rename("Aidtype" = "Aid type", "Time" = "TIME", 
         "Amounttype" = "Amount type") %>% 
  filter(Amounttype == "Constant Prices" & Aidtype == "Total Official, Gross")

d_ussector <- read_csv("data/US_sectors.csv")
ussector <- d_ussector %>% 
  select('Recipient','Donor','TIME','Value','Amount type','SECTOR',"Sector") %>% 
  rename("Time" = "TIME", "Amounttype" = "Amount type","Sectorcode" = "SECTOR", "sc_value"="Value") %>% 
  filter(Amounttype == "Constant Prices")
ussector[ussector[, "Sectorcode"] == 331, "Sectorcode"] <- 330


usaid <- bind_rows(usoda, usgrant, ustotal)
aid <- chinaaid %>% 
  left_join(usaid, by = c("Recipient","Time")) %>% 
  left_join(ussector, by = c("Recipient","Time")) %>% 
  rename("chinasccode" = "Sectorcode.x", "chinasc" = "Sector.x", "chinaaid" = "Value.x", "usscaid" = "sc_value", 
         "ussccode" = "Sectorcode.y", "ussc" = "Sector.y", "usaid" = "Value.y",
         "chinaclass" = "Flow Class", "chinatype" = "FlowType", "ustype" = "Aidtype") %>% 
  select("Donor.x", "Donor.y", "Recipient", "Time", "chinatype", "chinaclass",
         "chinasccode", "chinasc", "chinaaid", "ustype", "usaid",
         "usscaid", "ussccode", "ussc")


#control vars cleaning
ControlofCorruption <- read_excel("data/Control/ControlofCorruption.xlsx")
corruption <- ControlofCorruption %>% 
  pivot_longer(cols = c("2013", "2014",'2015','2016','2017','2018','2019','2020','2021','2022'), 
               names_to = 'year', values_to = "corruption")

GovernmentEffectiveness <- read_excel("data/Control/GovernmentEffectiveness.xlsx")
effective <- GovernmentEffectiveness %>% 
  pivot_longer(cols = c("2013", "2014",'2015','2016','2017','2018','2019','2020','2021','2022'), 
               names_to = 'year', values_to = "effectiveness")

Political_StabilityNoViolence <- read_excel("data/Control/Political StabilityNoViolence.xlsx")
stability <- Political_StabilityNoViolence %>% 
  pivot_longer(cols = c("2013", "2014",'2015','2016','2017','2018','2019','2020','2021','2022'), 
               names_to = 'year', values_to = "stability")

RegulatoryQuality <- read_excel("data/Control/RegulatoryQuality.xlsx")
regulation <- RegulatoryQuality %>% 
  pivot_longer(cols = c("2013", "2014",'2015','2016','2017','2018','2019','2020','2021','2022'), 
               names_to = 'year', values_to = "regulation")

freedom <- read_excel("data/Control/freedom.xlsx")
freedom <- freedom %>% 
  select("Country/Territory", Total, Edition) %>% 
  rename("year" = "Edition")

gdppercapita <- read_csv("data/Control/gdppercapita.csv")
gdppc <- gdppercapita %>% 
  select(`Country Name`,"2013", "2014",'2015','2016','2017','2018','2019','2020','2021','2022') %>% 
  pivot_longer(cols = c("2013", "2014",'2015','2016','2017','2018','2019','2020','2021','2022'), 
               names_to = 'year', values_to = "gdppc") %>% 
  rename("Country/Territory" = "Country Name")

infant_mortality <- read_csv("data/Control/infant mortality.csv")
infant <- infant_mortality %>% 
  select(`Country Name`,"2013", "2014",'2015','2016','2017','2018','2019','2020','2021','2022') %>% 
  pivot_longer(cols = c("2013", "2014",'2015','2016','2017','2018','2019','2020','2021','2022'), 
               names_to = 'year', values_to = "infant") %>% 
  rename("Country/Territory" = "Country Name")

population <- read_csv("data/Control/population.csv")
population <- population %>% 
  select(`Country Name`,"2013", "2014",'2015','2016','2017','2018','2019','2020','2021','2022') %>% 
  pivot_longer(cols = c("2013", "2014",'2015','2016','2017','2018','2019','2020','2021','2022'), 
               names_to = 'year', values_to = "population") %>% 
  rename("Country/Territory" = "Country Name")

convars <- corruption %>% 
  left_join(effective, by = c("Country/Territory", "year", "Code")) %>% 
  left_join(stability, by = c("Country/Territory", "year", "Code")) %>% 
  left_join(regulation, by = c("Country/Territory", "year", "Code")) %>%
  left_join(gdppc, by = c("Country/Territory", "year")) %>% 
  left_join(infant, by = c("Country/Territory", "year")) %>% 
  left_join(population, by = c("Country/Territory", "year")) %>% 
  left_join(freedom, by = c("Country/Territory", "year")) %>% 
  mutate(year=as.numeric(year)) %>% 
  subset(select = -Code) %>% 
  rename("Recipient" = "Country/Territory", "Time" = "year")


#combine control and IV DV
aid <- aid %>% 
  left_join(convars, by = c("Recipient", "Time"))

write_csv(aid, "data/aid.csv")

