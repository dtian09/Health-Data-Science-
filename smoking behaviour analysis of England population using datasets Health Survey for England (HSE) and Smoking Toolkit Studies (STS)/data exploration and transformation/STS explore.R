setwd("/Volumes/shared/niaaa_cascade1/Shared/STS data/STS and ATS files Sep 22/Latest omnibus SPSS data file")
library(tidyverse)

STS <- readRDS("stsdata_sept22_wave191.RDS")


STS$basecpd
summary(STS$basecpd2)
summary(STS$bcpdcat)

summary(STS$qimw981)

summary(STS$qpropsmo)
# -1	Don't know
# 1	Most or all of them
# 2	At least half
# 3	At least a quarter
# 4	Less than a quarter
# 5	Just a few people
# 7	Other answers

STS$proppercsmok <- recode(STS$qpropsmo,
                           "-1"="dk","1"="most/all",
                           "2"="half")

summary(as.factor(STS$proppercsmok))
