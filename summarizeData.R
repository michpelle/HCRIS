library(tidyverse)
library(data.table)
HCRIS_Data_v1996 <- readRDS("~/ECON 470/HCRIS/Data/HCRIS_Data_v1996.rds")
HCRIS_Data_v2010 <- readRDS("~/ECON 470/HCRIS/Data/HCRIS_Data_v2010.rds")
both <- inner_join(HCRIS_Data_v1996, HCRIS_Data_v2010, by="provider_number")
both <- both %>% mutate(year = ifelse(is.na(year.x)==TRUE, year.y,year.x)) %>%
  mutate(tot_charges = ifelse(is.na(tot_charges.x)==TRUE, tot_charges.y,tot_charges.x)) %>%
  mutate(tot_discounts = ifelse(is.na(tot_discounts.x)==TRUE, tot_discounts.y,tot_discounts.x)) %>%
  mutate(ip_charges = ifelse(is.na(ip_charges.x)==TRUE, ip_charges.y,ip_charges.x)) %>%
  mutate(icu_charges = ifelse(is.na(icu_charges.x)==TRUE, icu_charges.y,icu_charges.x)) %>%
  mutate(ancillary_charges = ifelse(is.na(ancillary_charges.x)==TRUE, ancillary_charges.y,ancillary_charges.x)) %>%
  mutate(tot_mcare_payment = ifelse(is.na(tot_mcare_payment.x)==TRUE, tot_mcare_payment.y,tot_mcare_payment.x)) %>%
  mutate(tot_discharges = ifelse(is.na(tot_discharges.x)==TRUE, tot_discharges.y,tot_discharges.x)) %>%
  mutate(mcare_discharges = ifelse(is.na(mcare_discharges.x)==TRUE, mcare_discharges.y,mcare_discharges.x))

head(HCRIS_Data_v1996)
head(HCRIS_Data_v2010)
head(both)

summary(HCRIS_Data_v1996)
summary(HCRIS_Data_v2010)

#1
only2010 <- HCRIS_Data_v1996 %>% filter(year==2010)
n_distinct(only2010$provider_number)

#2
n_occur <- data.frame(table(HCRIS_Data_v1996$provider_number))
# table of provider number and how many times they occur
n_occur[n_occur$Freq > 1,]
repeats <- HCRIS_Data_v1996[HCRIS_Data_v1996$provider_number %in% n_occur$Var1[n_occur$Freq > 1],]
nrow(repeats)
nrow(n_occur[n_occur$Freq > 1,])

numberRepeats <- both %>% group_by(year) %>% summarize(repeats = length(provider_number) - n_distinct(provider_number))
summary(both)
numberRepeats
ggplot(numberRepeats, aes(x=year, y=repeats)) + geom_line()

#removing/combining multiple reports
noRepeats <- both[!duplicated(both[c("year","provider_number")]),]
#3
noRepeatsForGraph <- noRepeats %>% group_by(year) %>% summarize(count = length(year))
ggplot(noRepeatsForGraph, aes(x=year, y=count)) + geom_line()

#4
noRepeats %>% 
  ggplot(aes(x = year, y = tot_charges)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5))

#5
estimatePrices <- noRepeats %>%
  mutate(discount_factor = 1-tot_discounts/tot_charges) %>%
  mutate(price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment) %>%
  mutate(price_denom = tot_discharges - mcare_discharges) %>%
  mutate(price = price_num/price_denom)


estimatePrices %>% 
  ggplot(aes(x = year, y = tot_charges)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5))

#6 
#indicator variable for penalty
indicatorPenalty <- noRepeats %>%
  mutate(penaltyIndicator = ifelse(is.na(hrrp_payment)==TRUE, 0, 1))

penaltyProportion <- indicatorPenalty %>%
  group_by(year) %>%
  summarize(propPenalized = mean(penaltyIndicator))

penaltyProportion %>%
  ggplot(aes(x = year, y=propPenalized)) +
  geom_line()

# ESTIMATE ATEs

#1
head(both$year.y)
byPenalty <- noRepeats %>%
  mutate(penaltyIndicator = ifelse(is.na(hrrp_payment)==TRUE, 0, 1))
  group_by()


