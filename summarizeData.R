library(tidyverse)
library(data.table)
HCRIS_Data_v1996 <- readRDS("~/ECON 470/HCRIS/Data/HCRIS_Data_v1996.rds")
HCRIS_Data_v2010 <- readRDS("~/ECON 470/HCRIS/Data/HCRIS_Data_v2010.rds")
both <- inner_join(HCRIS_Data_v1996, HCRIS_Data_v2010, by="provider_number")
both <- both %>% mutate(year = ifelse(is.na(year.x)==TRUE, year.y,year.x)) %>% mutate(tot_charges = ifelse(is.na(tot_charges.x)==TRUE, tot_charges.y,tot_charges.x))

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
