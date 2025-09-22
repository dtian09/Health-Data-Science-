#script to explore HSE data
#SerQt: Ever made a serious attempt to stop smoking completely (c+sc)
#Value = 1.0	Label = Never
#Value = 2.0	Label = Yes, in the last 12 months (Quitter)
#Value = 3.0	Label = Yes, but not in the last 12 months
#Value = -9.0	Label =  Refused
#Value = -8.0	Label = Don't know
#Value = -1.0	Label = Not applicable
hse2012 <- read.delim("X:/Shared/HSE data/HSE2012/UKDA-7480-tab/tab/hse2012ai.tab")

hse2015<-read.delim("Y:/HSE/Health Survey for England (HSE)/HSE 2015/UKDA-8280-tab/tab/hse2015ai.tab")
hse2016<-read.delim("X:/Shared/HSE data/HSE2016/UKDA-8334-tab/tab/hse2016_eul.tab")
hse2016 <- mutate(hse2016,social_grade = as.factor(ifelse(nssec8 %in% c(1,2,3),"ABC1","C2DE")))
table(hse2016$SerQt,useNA="ifany")
#-9   -8   -1    1    2    3
# 6    1 8724  363  283  690
table(hse2016$QuitNum,useNA="ifany")
table(hse2016$cignow)
#-1    1    2
#5788 1364 2915
hse2017<-read.delim("X:/Shared/HSE data/HSE2017/UKDA-8488-tab/tab/hse17i_eul_v1.tab")
hse2017 <- mutate(hse2017,social_grade = as.factor(ifelse(nssec8 %in% c(1,2,3),"ABC1","C2DE")))
table(hse2017$SerQt)
#-9   -1    1    2    3
# 2 8702  339  277  662
table(hse2017$cignow)
#-9   -1    1    2
# 3 5597 1300 3082
hse2018<-read.delim("X:/Shared/HSE data/HSE2018/UKDA-8649-tab/tab/hse_2018_eul_15082022.tab")
hse2018 <- mutate(hse2018,social_grade = as.factor(ifelse(nssec8 %in% c(1,2,3),"ABC1","C2DE")))
table(hse2018$SerQt)
#-9   -1    1    2    3
# 3 8950  331  300  666
table(hse2018$cignow)
#-9   -8   -1    1    2
#1    2 5791 1316 3140
hse2019<-read.delim("X:/Shared/HSE data/HSE2019/UKDA-8860-tab/tab/hse_2019_eul_20211006.tab")
hse2019 <- mutate(hse2019,social_grade = as.factor(ifelse(nssec8 %in% c(1,2,3),"ABC1","C2DE")))
table(hse2019$SerQt)
#-8   -1    1    2    3
# 5 9061  341  236  656
table(hse2019$cignow)
#-1    1    2
#5982 1254 3063

#training set=2016,2017,2018 test set=2019
library(tidyverse)
quitter2016 <- hse2016 %>%
             filter(SerQt==2)
smoker2016 <- hse2016 %>%
  filter(cignow==1)
quitter_and_smoker2016<-rbind(quitter2016,smoker2016)

quitter2017 <- hse2017 %>%
  filter(SerQt==2)
smoker2017 <- hse2017 %>%
  filter(cignow==1)
quitter_and_smoker2017<-rbind(quitter2017,smoker2017)

quitter2018 <- hse2018 %>%
  filter(SerQt==2)
smoker2018 <- hse2018 %>%
  filter(cignow==1)
quitter_and_smoker2018<-rbind(quitter2018,smoker2018)











