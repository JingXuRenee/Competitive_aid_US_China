# =============================================================================
# Analysis: U.S.-China Aid Competition in Africa
# Description: Testing three hypotheses on how U.S. aid allocation affects 
#              China's aid behavior in Africa (52 countries, 2013-2021).
#   H1: Overall relationship between U.S. and China total aid flows
#   H2: Lagged effect of U.S. aid on China's subsequent aid (ODA, grants, total)
#   H3: Sector-level comparison of U.S. vs. China aid allocation
# Author: Jing Xu
# =============================================================================

library(tidyverse)
library(broom)
library(ggplot2)
library(forecast)

aid <- read_csv("data/overalldata/aid.csv") %>% 
  rename(freedom = Total)


# =============================================================================
# H1: Cross-Sectional — Does More U.S. Aid Predict More China Aid?
# =============================================================================

# Aggregate to country level (sum aid, average controls over all years)
inflow <- aid %>% 
  filter(ustype == "Total Official, Gross") %>% 
  group_by(Recipient) %>% 
  summarise(
    usaid = sum(usaid, na.rm = TRUE), 
    chinaaid = sum(chinaaid, na.rm = TRUE),
    corruption = mean(corruption, na.rm = TRUE),
    effectiveness = mean(effectiveness, na.rm = TRUE), 
    stability = mean(stability, na.rm = TRUE),
    regulation = mean(regulation, na.rm = TRUE),
    gdppc = mean(gdppc, na.rm = TRUE), 
    infant = mean(infant, na.rm = TRUE),
    population = mean(population, na.rm = TRUE),
    freedom = mean(freedom, na.rm = TRUE)
  )

model_h1 <- lm(
  chinaaid ~ usaid + corruption + effectiveness + stability + 
    regulation + gdppc + infant + population + freedom, 
  data = inflow
)
summary(model_h1)

# Robustness: restrict to first 5 years (2013-2017, before China's 2019 aid cut)
inflowfirst5 <- aid %>% 
  filter(ustype == "Total Official, Gross", Time >= 2013, Time <= 2017) %>% 
  group_by(Recipient) %>% 
  summarise(
    usaid = sum(usaid, na.rm = TRUE), 
    chinaaid = sum(chinaaid, na.rm = TRUE),
    corruption = mean(corruption, na.rm = TRUE),
    effectiveness = mean(effectiveness, na.rm = TRUE), 
    stability = mean(stability, na.rm = TRUE),
    regulation = mean(regulation, na.rm = TRUE),
    gdppc = mean(gdppc, na.rm = TRUE), 
    infant = mean(infant, na.rm = TRUE),
    population = mean(population, na.rm = TRUE),
    freedom = mean(freedom, na.rm = TRUE)
  )

model_h1_robust <- lm(
  chinaaid ~ usaid + corruption + effectiveness + stability + 
    regulation + gdppc + infant + population + freedom, 
  data = inflowfirst5
)
summary(model_h1_robust)
# Result: U.S. aid has a positive relationship with China aid,
# suggesting direct competition rather than gap-filling.


# =============================================================================
# H2: Panel — Lagged Effect of U.S. Aid on China's Aid
# =============================================================================

# --- Prepare panel subsets by flow type ---

ODA <- aid %>% 
  filter(chinaclass == "ODA-like", 
         ustype == "Memo: ODA Total, Gross disbursements") %>% 
  group_by(Recipient, Time) %>% 
  summarise(
    usaid = sum(usaid, na.rm = TRUE), 
    chinaaid = sum(chinaaid, na.rm = TRUE),
    corruption = mean(corruption, na.rm = TRUE),
    effectiveness = mean(effectiveness, na.rm = TRUE),
    stability = mean(stability, na.rm = TRUE),
    regulation = mean(regulation, na.rm = TRUE),
    gdppc = mean(gdppc, na.rm = TRUE),
    infant = mean(infant, na.rm = TRUE),
    population = mean(population, na.rm = TRUE),
    freedom = mean(freedom, na.rm = TRUE),
    .groups = "drop"
  )

Grant <- aid %>% 
  filter(chinatype == "Grant", ustype == "Grants, Total") %>% 
  group_by(Recipient, Time) %>% 
  summarise(
    usaid = sum(usaid, na.rm = TRUE), 
    chinaaid = sum(chinaaid, na.rm = TRUE),
    corruption = mean(corruption, na.rm = TRUE),
    effectiveness = mean(effectiveness, na.rm = TRUE),
    stability = mean(stability, na.rm = TRUE),
    regulation = mean(regulation, na.rm = TRUE),
    gdppc = mean(gdppc, na.rm = TRUE),
    infant = mean(infant, na.rm = TRUE),
    population = mean(population, na.rm = TRUE),
    freedom = mean(freedom, na.rm = TRUE),
    .groups = "drop"
  )

inflowlag <- aid %>% 
  filter(ustype == "Total Official, Gross") %>% 
  group_by(Recipient, Time) %>% 
  summarise(
    usaid = sum(usaid, na.rm = TRUE), 
    chinaaid = sum(chinaaid, na.rm = TRUE),
    corruption = mean(corruption, na.rm = TRUE),
    effectiveness = mean(effectiveness, na.rm = TRUE),
    stability = mean(stability, na.rm = TRUE),
    regulation = mean(regulation, na.rm = TRUE),
    gdppc = mean(gdppc, na.rm = TRUE),
    infant = mean(infant, na.rm = TRUE),
    population = mean(population, na.rm = TRUE),
    freedom = mean(freedom, na.rm = TRUE),
    .groups = "drop"
  )


# --- Time-series visualization: ODA trends ---

ODAtime <- ODA %>% 
  group_by(Time) %>% 
  summarise(usaid = sum(usaid, na.rm = TRUE), 
            chinaaid = sum(chinaaid, na.rm = TRUE))

# Reshape to long format for plotting both countries on one chart
ODAtime_long <- ODAtime %>% 
  pivot_longer(cols = c(usaid, chinaaid), 
               names_to = "Country", values_to = "Aid") %>% 
  mutate(Country = recode(Country, usaid = "United States", chinaaid = "China"))

ggplot(ODAtime_long, aes(x = Time, y = Aid, color = Country, shape = Country)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(se = FALSE) +
  scale_y_log10() +
  labs(x = "Year", y = "Aid (log scale)", 
       title = "Trend of China and U.S. ODA to Africa")

# Country-level China aid trends
ODA %>% 
  ggplot(aes(x = Time, y = chinaaid, color = Recipient)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(se = FALSE) +
  labs(x = "Year", y = "China Aid", 
       title = "China ODA to African Countries by Recipient") +
  theme(legend.position = "none")


# --- Lagged regression models ---
# For each panel subset, construct a 1-year lag of U.S. aid within each country,
# then regress China's aid on last year's U.S. aid with controls.

run_lagged_model <- function(df, label) {
  df_lagged <- df %>% 
    arrange(Recipient, Time) %>% 
    group_by(Recipient) %>% 
    mutate(usaid_lag1 = lag(usaid, 1)) %>% 
    ungroup()
  
  model <- lm(
    chinaaid ~ usaid_lag1 + corruption + effectiveness + stability + 
      regulation + gdppc + infant + population + freedom, 
    data = df_lagged
  )
  
  cat("\n===", label, "===\n")
  print(summary(model))
  return(model)
}

model_oda_lag   <- run_lagged_model(ODA, "Lagged Effect: ODA")
model_grant_lag <- run_lagged_model(Grant, "Lagged Effect: Grants")
model_total_lag <- run_lagged_model(inflowlag, "Lagged Effect: Total Official Flows")

# Key finding: ODA shows no significant lagged effect, but grants do —
# suggesting a demonstration effect where U.S. grants prompt China to 
# increase its own grant-giving to remain competitive.


# =============================================================================
# H3: Sector-Level Comparison of U.S. vs. China Aid
# =============================================================================

# Recode detailed sector codes into broad categories
sector_recode <- c(
  `110` = "Social Services", `120` = "Social Services", `130` = "Social Services",
  `140` = "Social Services", `150` = "Social Services", `160` = "Social Services",
  `210` = "Infrastructure",  `220` = "Infrastructure",  `230` = "Infrastructure",
  `240` = "Infrastructure",  `250` = "Infrastructure",
  `310` = "Economy", `320` = "Economy", `330` = "Economy",
  `410` = "Environment", `430` = "Environment",
  `510` = "Humanitarian", `520` = "Humanitarian", `600` = "Humanitarian",
  `720` = "Humanitarian", `730` = "Humanitarian", `740` = "Humanitarian", 
  `998` = "Humanitarian"
)

aidnew <- aid %>% 
  mutate(
    chinasccode = recode(as.character(chinasccode), !!!sector_recode, .default = NA_character_),
    ussccode = recode(as.character(ussccode), !!!sector_recode, .default = NA_character_)
  )

# Aggregate by sector and country
Chinasector <- aidnew %>% 
  group_by(chinasccode, Time) %>% 
  summarise(chinaaid = sum(chinaaid, na.rm = TRUE), .groups = "drop") %>% 
  rename(code = chinasccode)

USsector <- aidnew %>% 
  group_by(ussccode, Time) %>% 
  summarise(usaid = sum(usscaid, na.rm = TRUE), .groups = "drop") %>% 
  rename(code = ussccode)

sector <- Chinasector %>% 
  left_join(USsector, by = c("code", "Time"))

# Overall sector comparison (collapsed across years)
sectorall <- sector %>% 
  group_by(code) %>% 
  summarise(chinaaid = sum(chinaaid, na.rm = TRUE), 
            usaid = sum(usaid, na.rm = TRUE)) %>% 
  filter(!is.na(code))

# China's share of total aid by sector
sectorall %>% 
  mutate(china_share = chinaaid / (chinaaid + usaid) * 100) %>% 
  ggplot(aes(x = china_share, y = reorder(code, china_share), fill = code)) +
  geom_col() +
  labs(x = "China's Share of Total Aid (%)", y = "Sector",
       title = "China's Proportion of Combined U.S.-China Aid by Sector") +
  theme(legend.position = "none")

# Sector trends over time
sector %>% 
  filter(!is.na(code)) %>% 
  ggplot(aes(x = Time, y = chinaaid, color = code)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  facet_wrap(~code, nrow = 2, scales = "free_y") +
  labs(x = "Year", y = "China Aid (millions USD)", 
       title = "China Aid to Africa by Sector Over Time") + 
  theme(legend.position = "none")

sector %>% 
  filter(!is.na(code)) %>% 
  ggplot(aes(x = Time, y = usaid, color = code)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  facet_wrap(~code, nrow = 2, scales = "free_y") +
  labs(x = "Year", y = "U.S. Aid (millions USD)", 
       title = "U.S. Aid to Africa by Sector Over Time") + 
  theme(legend.position = "none")

# Result: U.S. concentrates on social services and humanitarian aid;
# China leads in infrastructure — consistent with a division-of-labor 
# pattern rather than direct sector-level competition.
