
d<-readRDS(file="X:/Shared/STS data/STS and ATS files Oct 22/Latest omnibus SPSS data file/stsdata_oct22(wave1to192).rds")

#These are the COM-B variables of STS data identified so far
vars <- c("X.qfeel3",###psychological capability
          "X.qfeel4",
          "X.qfeel15",
          "X.qfcom3",
          "qimw139_0212",
          "qnharm",
          "qimw982",
          "X.qfeel20",
          "X.F632e6",
          "qharm_o",
          "eq5de",
          "qimw139_016",
          "audit1",
          "actage",###physical capability
          "sexz",
          "dis",
          "X.qfeel11",###reflective motivation
          "X.qfeel5",
          "X.qfeel6",
          "X.qfeel7",
          "X.qfeel8",
          "X.qfeel9",
          "X.qfeel10",
          "X.qfeel11",
          "X.qfeel12",
          "X.qfeel13",
          "X.qfeel14",
          "X.qfeel22",
          "q632x4",
          "X.qfac1",
          "q632a4a",
          "X.qfac9",
          "X.qfeel1",
          "X.E632e5",
          "X.qfeel6",
          "X.qfeel7",
          "X.A632e1",
          "X.qfeel3",
          "X.qfeel4",
          "X.qfeel15",
          "X.qfcom3",
          "fq6c",###automatic motivation
          "X.qfcom22",
          "X.qfcom23",
          "X.qfcom24",
          "basecpd",
          "basecpd2",
          "q632e10",###social opportunity
          "X.qfeel12",
          "qpropsmo",
          "qimw8022",
          "qimw8023",
          "qimw8024",
          "qimw8025",
          "qimw8026",
          "qimw8027",
          "qimw8028",
          "qimw981",
          "qimw891",
          "qimw892",
          "X.q632c3a_1",
          "sgz",
          "qual",
          "ethnic",
          "numkid",
          "X.qfeel17",###physical opportunity
          "X.qfcom17",
          "X.qbuy11",
          "X.qbuy117",
          "X.qbuy12",
          "X.qbuy13",
          "X.qbuy14",
          "X.qbuy15",
          "X.qbuy16",
          "X.qbuy17",
          "X.qbuy18",
          "X.qbuy19",
          "X.qbuy110",
          "X.qbuy111",
          "X.qbuy112",
          "X.qbuy113",
          "X.qbuy114",
          "X.qbuy115",
          "X.qbuy116",
          "X.qbuy118",
          "X.qbuy119",
          "X.qbuy120",
          "eversmo",####behavior variables
          "smoker",
          "exsmoker",
          "quitstat",
          "q632a2",
          "q632a2i",
          "cutdown",
          "typcig",
          "trylya",
          "trylyb",
          "trylyc",
          "X.q632b5",#use of behavior support
          "X.q632b6",
          "X.q632b7",
          "X.q632b1e",
          "X.q632b1f",
          "X.q632b1g",
          "X.q632b1h",
          "X.q632b1i",
          "X.q632b1j",
          "X.q632b3",#Use of pharmacological support
          "X.q632b4",
          "X.q632b1k",#Use of e-cigarettes
          "xyear",
          "xwave")

merge_waves_of_year <- function (df,year,vars){
  #merge/pool all STS waves of a particular year e.g. 2011 into a data frame
  #input: df, dataframe of all years
  #       year e.g. 2011
  #       vars, variables to select
  indx=df$xyear==year
  df2<-df[indx,]
  if ((vars=='all')[1])
  {
    print('select all variables of STS')
  }
  else{
    print(sprintf('select the variables of STS: %s',vars))
    df2<-subset(x=df2,select=vars)
  }
  #add fake ids to each row (participant)
  df2<-data.frame(ids=c(1:dim(df2)[1]),df2)
  return(df2)
}

df2011<-merge_waves_of_year(d,2011,'all')

#load HSE 2011 data
hse2011ah<-read.delim("X:/Shared/HSE data/HSE2011/UKDA-7260-tab/tab/hse2011ah.tab")
hse2011ai<-read.delim("X:/Shared/HSE data/HSE2011/UKDA-7260-tab/tab/hse2011ai.tab")

#library(tidyr)
library(dplyr)

summary(as.factor(hse2011ai$cignow))

qualyears<-d %>%
  group_by(nsec,xyear) %>%
  tally()

nsecyears<-d %>%
  group_by(sgz,xyear) %>%
  tally()

firstcg<- hse2011ai %>%
  filter(cignow==1) %>%        
  group_by(FirstCig) %>%
  tally()

q632b2<- df2011 %>%
  filter(smoker==1) %>%
  group_by(q632b2) %>%
  tally()

summary<-d %>%
  filter(smoker==1) %>%
  mutate(agegroup = cut(actage,
                        breaks=agecuts)) %>%
  group_by(sexz, agegroup,qual,qmotiv) %>%
  tally()

agecuts=c(0,24,34,44,54,64,100) #this creates the same age groups as Fidler paper (2010)

###create agegroup variable in HSE and STS dataframes
df2011<-mutate(df2011, agegroup = cut(actage,
                                      breaks=agecuts))

hse2011ai<-mutate(hse2011ai, agegroup = cut(Age,
                                         breaks=agecuts))
###make FirstCig of HSE compatible with q632b2 of STS by recoding values of FirstCig to the same ones of q632b2
#-1	Don't know
#1	Within 5 minutes
#2	6 - 30 minutes
#3	31 - 60 minutes
#4	More than 60 minutes
#6	Not Stated
##FirstCig (HSE)
#Value = -9	Label = Refusal
#Value = -8	Label = Don't Know
#Value = -2	Label = Schedule not applicable
#Value = -1	Label = Item not applicable
#Value = 1	Label = Less than 5 minutes
#Value = 2	Label = 5-14 minutes
#Value = 3	Label = 15-29 minutes
#Value = 4	Label = 30 minutes but less than 1 hour
#Value = 5	Label = 1 hour but less than 2 hours
#Value = 6	Label = 2 hours or more

hse2011ai$FirstCig_recoded<-recode(hse2011ai$FirstCig,
                                   "-8"=-1,
                                   "-9"=6,
                                   "-2"=6,
                                   "-1"=6,
                                   "1"=1,
                                   "2"=2,
                                   "3"=2,
                                   "4"=3,
                                   "5"=4,
                                   "6"=4)

bootstrap <- function(df2011,hse2011ai){
  #1. group HSE data by variables and create a proportion variable (probability of a group)
  stsdf <- df2011 %>%
    filter(smoker==1) %>%
    group_by(sexz,agegroup,q632b2,qmotiv) %>%
    summarize(total=sum(X.weight0)) %>%
    ungroup() %>%
    group_by(sexz, agegroup,q632b2) %>%
    mutate(prop=total/sum(total))
  #2. foreach combination of the values of the variables sex, agegroup and q632b2 in STS data
  #   {sample with replacement the corresponding proportion of individuals from the matching group in HSE data
  #   }
  b<-data.frame()
  combinations_which_no_individual_in_HSE_satisfy<-data.frame()
  for (i in 1:dim(stsdf)[1]){
    r<-stsdf[i,]
    g<-subset(hse2011ai,Sex==r$sexz & agegroup==r$agegroup & FirstCig_recoded==r$q632b2) #find the group
    if(dim(g)[1]==0){
      combinations_which_no_individual_in_HSE_satisfy<-rbind(combinations_which_no_individual_in_HSE_satisfy,r)
    }
    else {
      k<-dim(g)[1]
      indx<-seq(1:k)
      indx2<-sample(indx,replace=T,prob=rep(r$prop,k))
      s<-g[indx2,]
      b<-rbind(b,s)
    }
  }
  #3. display any combinations of values of the variables for which there is no individual in HSE data
  if (!empty(combinations_which_no_individual_in_HSE_satisfy)){
     print('There is no individual in HSE who satisfy any of the following combinations of values of the variables Sex, agegroup and FirstCig_recoded:')
     names(combinations_which_no_individual_in_HSE_satisfy)<-names(stsdf)
     print(combinations_which_no_individual_in_HSE_satisfy)
  }
  return(b)
}

b<-bootstrap(df2011,hse2011ai)

