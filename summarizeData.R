library(tidyverse)
library(data.table)
HCRIS_Data_v1996 <- readRDS("~/ECON 470/HCRIS/Data/HCRIS_Data_v1996.rds")
HCRIS_Data_v2010 <- readRDS("~/ECON 470/HCRIS/Data/HCRIS_Data_v2010.rds")

head(HCRIS_Data_v1996)
head(HCRIS_Data_v2010)

summary(HCRIS_Data_v1996)
summary(HCRIS_Data_v2010)

#1
n_distinct(HCRIS_Data_v1996$provider_number)

#2
n_occur <- data.frame(table(HCRIS_Data_v1996$provider_number))
# table of provider number and how many times they occur
n_occur[n_occur$Freq > 1,]
repeats <- HCRIS_Data_v1996[HCRIS_Data_v1996$provider_number %in% n_occur$Var1[n_occur$Freq > 1],]

