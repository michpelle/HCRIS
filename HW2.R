library(tidyverse)
library(data.table)
library(cobalt)
library(Matching)
library(splusTimeDate)

HCRIS_Data_v1996 <- readRDS("~/ECON 470/HCRIS/HCRIS_Data_v1996.rds")
HCRIS_Data_v2010 <- readRDS("~/ECON 470/HCRIS/HCRIS_Data_v2010.rds")

HCRIS_Data_v1996 <- HCRIS_Data_v1996 %>% mutate(hvbp_payment=NA, hrrp_payment=NA)

both <- rbind(HCRIS_Data_v1996,HCRIS_Data_v2010) %>%
  mutate(fy_end=mdy(timeDate(c(fy_end))),fy_start=mdy(timeDate(c(fy_start))),
         date_processed=mdy(timeDate(c(date_processed))),date_created=mdy(timeDate(c(date_created))),
         tot_discounts=abs(tot_discounts), hrrp_payment=abs(hrrp_payment))

#1
only2010 <- HCRIS_Data_v1996 %>% filter(year==2010)
answerForOne <- n_distinct(only2010$provider_number)

#2
bothFOR <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(bothFOR) <- c('year', 'repeats')

for(i in 1998:2017){
  bothFor <- both%>%filter(year==i)
  n_occur <- data.frame(table(bothFor$provider_number))
  repeats <- nrow(bothFor[bothFor$provider_number %in% n_occur$Var1[n_occur$Freq > 1],])
  bothFOR[nrow(bothFOR) + 1,] <- c(i,repeats)
}

summarizeGraphForTwo <- ggplot(bothFOR, aes(x=year, y=repeats)) + geom_line() + ggtitle("Hospitals with Multiple Reports")

#3
#removing/combining multiple reports
noRepeats <- both[!duplicated(both[c("year","provider_number")]),]
noRepeatsForGraph <- noRepeats %>% group_by(year) %>% summarize(count = length(provider_number))
summarizeGraphForThree <- ggplot(noRepeatsForGraph, aes(x=year, y=count)) + geom_line()

#4
summarizeGraphForFour <- noRepeats %>% 
  ggplot(aes(x = year, y = tot_charges)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5))

#5
estimatePrices <- noRepeats %>%
  mutate(discount_factor = 1-tot_discounts/tot_charges) %>%
  mutate(price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment) %>%
  mutate(price_denom = tot_discharges - mcare_discharges) %>%
  mutate(price = price_num/price_denom)

summarizeGraphForFive <- estimatePrices %>% 
  ggplot(aes(x = year, y = tot_charges)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5))

#6
byPenalty <- estimatePrices %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom),
         price_num>0, !is.na(price_num)) %>%
  #price<100000,
  #beds>30, year==2012) %>%
  mutate(hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
         hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)),
         penalty = (hvbp_payment-hrrp_payment<0))

indicatorPenalty <- byPenalty %>%
  mutate(penaltyIndicator = ifelse(penalty==FALSE, 0, 1))

penaltyProportion <- indicatorPenalty %>%
  group_by(year) %>%
  summarize(propPenalized = mean(penaltyIndicator))

summarizeGraphForSix <- penaltyProportion %>%
  ggplot(aes(x = year, y=propPenalized)) +
  geom_line()

#indicatorPenalty <- noRepeats %>%
#mutate(penaltyIndicator = ifelse(is.na(hrrp_payment)==TRUE | hrrp_payment>=0, 0, 1))

#penaltyProportion <- indicatorPenalty %>%
#group_by(year) %>%
#summarize(propPenalized = mean(penaltyIndicator))

#summarizeGraphForSix <- penaltyProportion %>%
#ggplot(aes(x = year, y=propPenalized)) +
#geom_line()

# ESTIMATE ATEs

#1
avgByPenalty <- indicatorPenalty %>% group_by(penalty) %>% summarize(avgPrice = mean(price))

#2
summary(indicatorPenalty$beds)
byQuartile <- indicatorPenalty %>% 
  mutate(quartile = ifelse(beds<=93,1,ifelse(beds<=160,2,ifelse(beds<=281,3,4))))
byQuartile <- byQuartile %>% drop_na(quartile)
summary(byQuartile$quartile)

#3
summary(byQuartile)
#nnInverseVariance <- Matching::Match(Y=byQuartile$price,
                #Tr=byQuartile$penaltyIndicator,
                #X= byQuartile$quartile, #lp.covs, penalty
                #M=4,
                #Weight=1,
                #estimand="ATE", ties=FALSE)

#4
#nnMahalanobis <- Match(Y=byQuartile$price,
                       #Tr=byQuartile$penalty,
                       #X=byQuartile$quartile,
                       #M=1,
                       #Weight=2,
                       #estimand="ATE")

#5
logit.model <- glm(penaltyIndicator ~ quartile + price, family=binomial, data=byQuartile)
ps <- fitted(logit.model)

IPW <- byQuartile %>%
  mutate(ipw = case_when(
    penaltyIndicator == 1 ~ 1/ps,
    penaltyIndicator == 0 ~ 1/(1-ps),
    TRUE ~ NA_real_
  ))

summary(IPW)

mean.t1 <- IPW %>% filter(penaltyIndicator==1) %>%
  #select(price,ipw) %>%
  summarize(mean_p=weighted.mean(price,w=ipw))
mean.t0 <- IPW %>% filter(penaltyIndicator==0) %>%
  #select(price, ipw) %>%
  summarize(mean_p=weighted.mean(price,w=ipw))
IPWresult <- mean.t1$mean_p - mean.t0$mean_p


Ipw <- lm(price ~ penaltyIndicator, data=IPW, weights=ipw)
Ipw

#6
reg1.dat <- byQuartile %>% filter(penaltyIndicator==1)
reg1 <- lm(price ~ quartile, data=reg1.dat)
reg0.dat <- byQuartile %>% filter(penaltyIndicator==0)
reg0 <- lm(price ~ quartile, data=reg0.dat)
pred1 <- predict(reg1,new=byQuartile)
pred0 <- predict(reg0,new=byQuartile)
regression <- mean(pred1-pred0)

rm(list=c("HCRIS_Data_v1996","HCRIS_Data_v2010","both"))
