# =============================================================================
# Data Wrangling: U.S.-China Aid Competition in Africa
# Description: Cleaning and merging aid data (China: AidData GCDF 3.0; 
#              U.S.: OECD DAC) with country-level control variables 
#              (World Bank, Freedom House) for 52 African countries, 2013-2021.
# Author: Jing Xu
# =============================================================================

library(tidyverse)
library(readxl)

# Shared year range across all control variable datasets
year_cols <- as.character(2013:2022)


# =============================================================================
# 1. Dependent Variable: China's Aid to Africa (AidData GCDF 3.0)
# =============================================================================

d <- read_excel("data/GCDF_3.0.xlsx")

chinaaid <- d %>% 
  select('Financier Country', 'Recipient', 'Recipient Region', 'Commitment Year',
         'Flow Type Simplified', 'Flow Class', 'Sector Code', 'Sector Name',
         'Adjusted Amount (Constant USD 2021)') %>% 
  rename(Donor = 'Financier Country', 
         Region = 'Recipient Region',
         Time = 'Commitment Year', 
         FlowType = 'Flow Type Simplified',
         value = 'Adjusted Amount (Constant USD 2021)',
         Sectorcode = 'Sector Code', 
         Sector = 'Sector Name') %>% 
  filter(Region == 'Africa') %>% 
  mutate(Value = value / 1000000)  # convert to millions USD


# =============================================================================
# 2. Independent Variable: U.S. Aid to Africa (OECD DAC)
# =============================================================================

# Helper function to clean OECD datasets with consistent structure
clean_oecd <- function(filepath) {
  read_csv(filepath) %>% 
    select(Recipient, Donor, 'Aid type', TIME, Value, 'Amount type') %>% 
    rename(Aidtype = 'Aid type', Time = TIME, Amounttype = 'Amount type') %>% 
    filter(Amounttype == "Constant Prices")
}

usoda   <- clean_oecd("data/US_ODA.csv")
usgrant <- clean_oecd("data/US_grants.csv")

# Total official flows: additional filter for aid type
ustotal <- clean_oecd("data/US_total_official_flows.csv") %>% 
  filter(Aidtype == "Total Official, Gross")

# U.S. sector-level aid
ussector <- read_csv("data/US_sectors.csv") %>% 
  select(Recipient, Donor, TIME, Value, 'Amount type', SECTOR, Sector) %>% 
  rename(Time = TIME, Amounttype = 'Amount type', 
         Sectorcode = SECTOR, sc_value = Value) %>% 
  filter(Amounttype == "Constant Prices")

# Recode sector 331 -> 330 (trade policy subcategory merged into parent)
ussector[ussector[, "Sectorcode"] == 331, "Sectorcode"] <- 330


# =============================================================================
# 3. Merge China and U.S. Aid Data
# =============================================================================

usaid <- bind_rows(usoda, usgrant, ustotal)

aid <- chinaaid %>% 
  left_join(usaid, by = c("Recipient", "Time")) %>% 
  left_join(ussector, by = c("Recipient", "Time")) %>% 
  rename(chinasccode = Sectorcode.x, chinasc = Sector.x, chinaaid = Value.x, 
         usscaid = sc_value, ussccode = Sectorcode.y, ussc = Sector.y, 
         usaid = Value.y, chinaclass = 'Flow Class', chinatype = FlowType, 
         ustype = Aidtype) %>% 
  select(Donor.x, Donor.y, Recipient, Time, chinatype, chinaclass,
         chinasccode, chinasc, chinaaid, ustype, usaid,
         usscaid, ussccode, ussc)


# =============================================================================
# 4. Control Variables
# =============================================================================

# --- World Governance Indicators (World Bank) ---

# Helper: pivot WGI datasets from wide to long format
pivot_wgi <- function(filepath, varname) {
  read_excel(filepath) %>% 
    pivot_longer(cols = all_of(year_cols), 
                 names_to = "year", values_to = varname)
}

corruption  <- pivot_wgi("data/Control/ControlofCorruption.xlsx", "corruption")
effective   <- pivot_wgi("data/Control/GovernmentEffectiveness.xlsx", "effectiveness")
stability   <- pivot_wgi("data/Control/Political StabilityNoViolence.xlsx", "stability")
regulation  <- pivot_wgi("data/Control/RegulatoryQuality.xlsx", "regulation")

# --- Freedom House ---
freedom <- read_excel("data/Control/freedom.xlsx") %>% 
  select("Country/Territory", Total, Edition) %>% 
  rename(year = Edition)

# --- World Bank Development Indicators ---

# Helper: pivot World Bank CSV datasets from wide to long
pivot_wb <- function(filepath, varname) {
  read_csv(filepath) %>% 
    select(`Country Name`, all_of(year_cols)) %>% 
    pivot_longer(cols = all_of(year_cols), 
                 names_to = "year", values_to = varname) %>% 
    rename("Country/Territory" = "Country Name")
}

gdppc      <- pivot_wb("data/Control/gdppercapita.csv", "gdppc")
infant     <- pivot_wb("data/Control/infant mortality.csv", "infant")
population <- pivot_wb("data/Control/population.csv", "population")


# --- Merge all control variables ---
convars <- corruption %>% 
  left_join(effective, by = c("Country/Territory", "year", "Code")) %>% 
  left_join(stability, by = c("Country/Territory", "year", "Code")) %>% 
  left_join(regulation, by = c("Country/Territory", "year", "Code")) %>%
  left_join(gdppc, by = c("Country/Territory", "year")) %>% 
  left_join(infant, by = c("Country/Territory", "year")) %>% 
  left_join(population, by = c("Country/Territory", "year")) %>% 
  left_join(freedom, by = c("Country/Territory", "year")) %>% 
  mutate(year = as.numeric(year)) %>% 
  subset(select = -Code) %>% 
  rename(Recipient = "Country/Territory", Time = year)


# =============================================================================
# 5. Final Merge and Export
# =============================================================================

aid <- aid %>% 
  left_join(convars, by = c("Recipient", "Time"))

write_csv(aid, "data/aid.csv")
