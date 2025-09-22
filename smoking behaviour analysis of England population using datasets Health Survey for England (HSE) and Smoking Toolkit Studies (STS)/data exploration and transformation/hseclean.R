#use hseclean library to pre-process HSE data. The pre-processed data is STAPM data.
library(hseclean)
library(tidyverse)
source("COMB_variables_of_HSE.R")

hse2012_stapm<-read_2012(root="x:/",
                         file="Shared/HSE data/HSE2012/UKDA-7480-tab/tab/hse2012ai.tab",
                         select_cols = c("tobalc", "all")[2]
                         )

hse2012_stapm <- clean_age(hse2012_stapm)



hse2013_stapm<-read_2013(root="x:/",
                         file="Shared/HSE data/HSE2013/UKDA-7649-tab/tab/hse2013ai.tab",
                         select_cols = c("tobalc", "all")[2]
                        )

smoker_1941to1960<-hse2012_stapm %>%
  drop_na(cigst1) %>%
  filter(cohort>=1941)%>%
  filter(cohort<=1960)%>%
  filter(cigst1==4)

#select age, sex, IMD quintile and states of agents in HSE 2012. These are used to initialize agent population in ABM software V0.1
smoker_and_ex_smoker_and_never_smoker<-hse2012_stapm %>%
        drop_na(cigst1) %>%
        filter(age>=16) %>%
        mutate(state = as.factor(ifelse(cigst1==4,'smoker',ifelse(cigst1 %in% c(2,3),'ex-smoker',ifelse(cigst1==1,'never_smoker','NA')))),
               sex = recode(sex,"1"="male","2"="female")
               ) %>%
        drop_na(state) %>%
        dplyr::select(age, sex, qimd, state)

write.csv(smoker_and_ex_smoker_and_never_smoker,file="U:/smoking cessation/MBSSM software V0.1/code/hse2012_stapm.csv",row.names=F)

#dn<-setdiff(tolower(names(hse2012_stapm)),tolower(names(hse2012)))
#dn2<-setdiff(tolower(names(hse2012)),tolower(names(hse2012_stapm)))

hse2012_stapm <- clean_age(hse2012_stapm)
hse2012_stapm<-clean_economic_status(hse2012_stapm)
#smokers, ex-smokers and never smokers (HSE 2012 has no quitter variable)
comb<-tolower(comb)
n<-tolower(names(hse2012_stapm))
setdiff(comb,n)#get the COM-B variables which are not in STAPM data 
#"ill12m"  "nssec8"   "topqual2" "origin" are not in STAPM 2012 data

#using hseclean V1.11.1
hse2015_stapm<-read_2015(root="y:/",
                         file="HSE/Health Survey for England (HSE)/HSE 2015/UKDA-8280-tab/tab/hse2015ai.tab",
                         select_cols = c("tobalc", "all")[2]
                         )

hse2015_stapm <- clean_age(hse2015_stapm)
hse2015_stapm<-clean_economic_status(hse2015_stapm)

hse2016_stapm<-read_2016("y:/","/HSE/Health Survey for England (HSE)/HSE 2016/UKDA-8334-tab/tab/hse2016_eul.tab")
hse2016_stapm <- clean_age(hse2016_stapm)
hse2016_stapm<-clean_economic_status(hse2016_stapm)