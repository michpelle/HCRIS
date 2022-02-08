source('/home/pan/ECON 470/HCRIS/data-code/_HCRIS_Data.R')

HCRIS_Data_v1996 <- readRDS("~/ECON 470/HCRIS/Data/HCRIS_Data_v1996.rds")
HCRIS_Data_v2010 <- readRDS("~/ECON 470/HCRIS/Data/HCRIS_Data_v2010.rds")

head(HCRIS_Data_v1996)
head(HCRIS_Data_v2010)

summary(HCRIS_Data_v1996)
summary(HCRIS_Data_v2010)

#1
n_distinct(HCRIS_Data_v1996$provider_number)
