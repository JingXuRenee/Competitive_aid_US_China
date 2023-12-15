library(tidyverse)
aid <- read_csv("data/aid.csv") %>% 
  rename("freedom" = "Total")

# on H1
inflow <- aid %>% 
  filter(ustype == "Total Official, Gross") %>% 
  group_by(Recipient) %>% 
  summarise(usaid=sum(usaid, na.rm = TRUE), chinaaid=sum(chinaaid, na.rm = TRUE), corruption=mean(corruption,na.rm = TRUE),
            effectiveness=mean(effectiveness, na.rm = TRUE), stability=mean(stability, na.rm = TRUE), regulation=mean(regulation, na.rm = TRUE),
            gdppc=mean(gdppc, na.rm = TRUE), infant=mean(infant, na.rm = TRUE), population=mean(population, na.rm = TRUE),
            freedom=mean(freedom, na.rm = TRUE))

write_csv(inflow, "figure/inflow.csv")

## H1 regression
model1 <- lm(chinaaid~usaid+corruption+effectiveness+stability+regulation+gdppc+infant+population+freedom,data = inflow)
summary(model1)


## redo with the first 5 years
inflowfirst5 <- aid %>% 
  filter(ustype == "Total Official, Gross") %>% 
  filter(Time >= 2013 & Time <= 2017) %>% 
  group_by(Recipient) %>% 
  summarise(usaid=sum(usaid, na.rm = TRUE), chinaaid=sum(chinaaid, na.rm = TRUE), corruption=mean(corruption,na.rm = TRUE),
            effectiveness=mean(effectiveness, na.rm = TRUE), stability=mean(stability, na.rm = TRUE), regulation=mean(regulation, na.rm = TRUE),
            gdppc=mean(gdppc, na.rm = TRUE), infant=mean(infant, na.rm = TRUE), population=mean(population, na.rm = TRUE),
            freedom=mean(freedom, na.rm = TRUE))

write_csv(inflowfirst5, "figure/inflowfirst5.csv")

model2 <- lm(chinaaid~usaid+corruption+effectiveness+stability+regulation+gdppc+infant+population+freedom,data = inflowfirst5)
summary(model2)
### us aid has positive relation with china aid



# on H2
library(dplyr)
library(broom)

ODA <- aid %>% 
  filter(chinaclass == "ODA-like") %>% 
  filter(ustype == "Memo: ODA Total, Gross disbursements") %>% 
  group_by(Recipient, Time) %>% 
  summarise(usaid=sum(usaid, na.rm = TRUE), chinaaid=sum(chinaaid, na.rm = TRUE), corruption=mean(corruption,na.rm = TRUE),
            effectiveness=mean(effectiveness, na.rm = TRUE), stability=mean(stability, na.rm = TRUE), regulation=mean(regulation, na.rm = TRUE),
            gdppc=mean(gdppc, na.rm = TRUE), infant=mean(infant, na.rm = TRUE), population=mean(population, na.rm = TRUE),
            freedom=mean(freedom, na.rm = TRUE))

Grant <- aid %>% 
  filter(chinatype == "Grant") %>% 
  filter(ustype == "Grants, Total") %>% 
  group_by(Recipient, Time) %>% 
  summarise(usaid=sum(usaid, na.rm = TRUE), chinaaid=sum(chinaaid, na.rm = TRUE), corruption=mean(corruption,na.rm = TRUE),
            effectiveness=mean(effectiveness, na.rm = TRUE), stability=mean(stability, na.rm = TRUE), regulation=mean(regulation, na.rm = TRUE),
            gdppc=mean(gdppc, na.rm = TRUE), infant=mean(infant, na.rm = TRUE), population=mean(population, na.rm = TRUE),
            freedom=mean(freedom, na.rm = TRUE))


## on visualizing a timely trend of aid
## time-series analysis
library(ggplot2)
library(forecast)
library(tibble)
library(dplyr)
library(tsibble)

ODAtime <- ODA %>% 
  group_by(Time) %>% 
  summarise(usaid=sum(usaid, na.rm = TRUE), chinaaid=sum(chinaaid, na.rm = TRUE))

write_csv(ODAtime, "figure/ODAtime.csv")


### scatter: one categorical and two quant
ODAtime |>
  mutate(Aid = Aid) %>% 
  ggplot(aes(x = Time, y = Aid)) +
  scale_y_log10()+
  geom_point(aes(shape = Country, color = Country), alpha = 0.3, size = 0.5, stroke = 1) +
  geom_smooth(aes(color = Country)) +
  labs(x = "Year", y = "Aid", color = "Country", shape = "Country",
       title = "Trend of China and US aid to Africa")

### time series
ODA |>
  ggplot(aes(x = Time, y = chinaaid, color = Recipient)) + geom_point(alpha = 0.2) + geom_smooth() +
  labs(x = "Year", y = "China aid", title = "China aid to African countries", color = "Country/Region")

ODA |>
  ggplot(aes(x = Time, y = chinaaid, color = Recipient)) + geom_point(alpha = 0.5) +
  facet_wrap(~Recipient, nrow = 2) +
  labs(x = "Year", y = "China aid", title = "China aid to African countries", color = "Country/Region") + 
  theme(legend.position = "none")

### on lagged effect of ODA
#### visualization of lagged effect
pdf("odalagplot.pdf", width = 8, height = 6)
ccf_usaid_chinaaid <- ccf(ODA$usaid, ODA$chinaaid, lag.max = 8, plot = TRUE)
dev.off()

#### regression of laggaed effect
lagged_model <- lm(chinaaid ~ lag(usaid, 1)+corruption+effectiveness+stability+regulation+gdppc+infant+population+freedom, data = ODA)
summary(lagged_model)

### on lagged effect of Grants
#### visualization
pdf("grantlagplot.pdf", width = 8, height = 6)
ccf_usaid_chinaaid <- ccf(Grant$usaid, Grant$chinaaid, lag.max = 8, plot = TRUE)
dev.off()

#### regression
lagged_model <- lm(chinaaid ~ lag(usaid, 1)+corruption+effectiveness+stability+regulation+gdppc+infant+population+freedom, data = Grant)
summary(lagged_model)

### on lagged effect of total inflow
inflowlag <- aid %>% 
  filter(ustype == "Total Official, Gross") %>% 
  group_by(Recipient, Time) %>% 
  summarise(usaid=sum(usaid, na.rm = TRUE), chinaaid=sum(chinaaid, na.rm = TRUE), corruption=mean(corruption,na.rm = TRUE),
            effectiveness=mean(effectiveness, na.rm = TRUE), stability=mean(stability, na.rm = TRUE), regulation=mean(regulation, na.rm = TRUE),
            gdppc=mean(gdppc, na.rm = TRUE), infant=mean(infant, na.rm = TRUE), population=mean(population, na.rm = TRUE),
            freedom=mean(freedom, na.rm = TRUE))

lagged_model <- lm(chinaaid ~ lag(usaid, 1)+corruption+effectiveness+stability+regulation+gdppc+infant+population+freedom, data = inflowlag)
summary(lagged_model)


# on H3
aidnew <- aid |>
  mutate(
    chinasccode = case_match(
      chinasccode,
      110 ~ "Socialservices", 120 ~ "Socialservices", 130 ~ "Socialservices",
      140 ~ "Socialservices", 150 ~ "Socialservices", 160 ~ "Socialservices",
      210 ~ "Infrastructure", 220 ~ "Infrastructure", 230 ~ "Infrastructure",
      240 ~ "Infrastructure", 250 ~ "Infrastructure", 310 ~ "Economy",
      320 ~ "Economy", 330 ~ "Economy", 410 ~ "Environment",
      430 ~ "Environment", 510 ~ "Humanity", 520 ~ "Humanity", 600 ~ "Humanity",
      720 ~ "Humanity", 730 ~ "Humanity", 740 ~ "Humanity", 998 ~ "Humanity",
      .default = NA))

aidnew <- aidnew |>
  mutate(
    ussccode = case_match(
      ussccode,
      110 ~ "Socialservices", 120 ~ "Socialservices", 130 ~ "Socialservices",
      140 ~ "Socialservices", 150 ~ "Socialservices", 160 ~ "Socialservices",
      210 ~ "Infrastructure", 220 ~ "Infrastructure", 230 ~ "Infrastructure",
      240 ~ "Infrastructure", 250 ~ "Infrastructure", 310 ~ "Economy",
      320 ~ "Economy", 330 ~ "Economy", 410 ~ "Environment",
      430 ~ "Environment", 510 ~ "Humanity", 520 ~ "Humanity", 600 ~ "Humanity",
      720 ~ "Humanity", 730 ~ "Humanity", 740 ~ "Humanity", 998 ~ "Humanity",
      .default = NA))

Chinasector <- aidnew %>% 
  group_by(chinasccode, Time) %>% 
  summarise(chinaaid=sum(chinaaid, na.rm = TRUE)) %>% 
  rename("code"=chinasccode)
USsector <- aidnew %>% 
  group_by(ussccode, Time) %>% 
  summarise(usaid=sum(usscaid, na.rm = TRUE)) %>% 
  rename("code" = ussccode)

sector <- Chinasector %>% 
  left_join(USsector,by=c("code", "Time"))

sectorall <- sector %>% 
  ungroup() %>% 
  group_by(code) %>% 
  summarise(chinaaid=sum(chinaaid, na.rm = TRUE),usaid=sum(usaid, na.rm = TRUE))

## plotting
### overall sector aid comparison
sectorall %>% 
  group_by(code) %>% 
  mutate(prop_china = chinaaid / sum(chinaaid+usaid) * 100) %>% 
  ggplot(aes(x = prop_china, y = code, fill = code)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "Sector", x = "Percentage (%)", title = "Chinaaid Proportion in different sectors", fill = "code")

### trend of sector inflow by China and US
sector |>
  ggplot(aes(x = Time, y = chinaaid, color = code)) + geom_point(alpha = 0.5) +
  geom_smooth()+
  facet_wrap(~code, nrow = 2) +
  labs(x = "Year", y = "China aid", title = "China aid in different sectors", color = "Sector") + 
  theme(legend.position = "none")

sector |>
  ggplot(aes(x = Time, y = usaid, color = code)) + geom_point(alpha = 0.5) +
  geom_smooth()+
  facet_wrap(~code, nrow = 2) +
  labs(x = "Year", y = "US aid", title = "US aid in different sectors", color = "Sector") + 
  theme(legend.position = "none")
