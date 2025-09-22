#this script computes statistics of e-cigarette users in STS 

library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)

d<-read_rds(file="X:/Shared/STS data/STS and ATS files Apr 23/Latest omnibus SPSS data file/wave198.rds")

percentage_of_ecig_users <- function(d,year){
  #return percentage of e-cig users of year i.e. E[t]
  d2 <- d %>% filter(xyear==year) %>% mutate(ids=1:nrow(.))
  #table(d2$allecig,useNA = 'ifany')#display counts of values
  ecig<-d2 %>% #e-cig users
    drop_na(allecig) %>%
    filter(allecig==1) %>%
    dplyr::select(allecig)
  all<-d2 %>% #user and non-users of e-cig
    drop_na(allecig) %>%
    dplyr::select(allecig)
  p<-dim(ecig)[1]/dim(all)[1] * 100 #percentage of e-cig users
  return(p)
}

p2009<-percentage_of_ecig_users(d,2009)
print(p2009)
p2010<-percentage_of_ecig_users(d,2010)
print(p2010)
p2011<-percentage_of_ecig_users(d,2011)
print(p2011)
p2012<-percentage_of_ecig_users(d,2012)
print(p2012)
p2013<-percentage_of_ecig_users(d,2013)
print(p2013)
p2014<-percentage_of_ecig_users(d,2014)
print(p2014)
p2015<-percentage_of_ecig_users(d,2015)
print(p2015)
p2016<-percentage_of_ecig_users(d,2016)
print(p2016)
p2017<-percentage_of_ecig_users(d,2017)
print(p2017)
p2018<-percentage_of_ecig_users(d,2018)
print(p2018)
p2019<-percentage_of_ecig_users(d,2019)
print(p2019)
p2020<-percentage_of_ecig_users(d,2020)
print(p2020)
p2021<-percentage_of_ecig_users(d,2021)
print(p2021)
p2022<-percentage_of_ecig_users(d,2022)
print(p2022)
p2023<-percentage_of_ecig_users(d,2023)
print(p2023)

# Plot
library(ggplot2)
library(dplyr)
library(hrbrthemes)
#vector of E[t]s
ps<-c(p2009,p2010,p2011,p2012,p2013,p2014,p2015,p2016,p2017,p2018,p2019,p2020,p2021,p2022)
yrs<-c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)
df<-data.frame(yrs)
df<-cbind(df,ps)
names(df)<-c('year','percentage of ecigarette users')

plot_ecig <- function(df,title){
  c1<-names(df)[1]
  c2<-names(df)[2]
  names(df)[1]<-'x'
  names(df)[2]<-'y'
  df %>%
    ggplot(aes(x=x,y=y))+ 
    scale_y_continuous(name=c2)+
    scale_x_continuous(name=c1,breaks=df$x,labels=df$x)+
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
    theme_ipsum() +
    ggtitle(title)
}

plot_ecig(df,"Percentage of e-cigarette users (%)")

#change in e-cigarette users at year t i.e. deltaE[t]
deltaE <- function(d,year){
  #delta E[t] = E[t] - E[t-1] where E[t] is no. of e-cig users at t
  #return delta E[t]
  d2 <- d %>% filter(xyear==year) %>% mutate(ids=1:nrow(.))
  ecig<-d2 %>% 
    drop_na(allecig) %>%
    filter(allecig==1) %>%
    dplyr::select(allecig)
  d3 <- d %>% filter(xyear==(year-1)) %>% mutate(ids=1:nrow(.))
  ecig2<-d3 %>% 
    drop_na(allecig) %>%
    filter(allecig==1) %>%
    dplyr::select(allecig)
  dE<-dim(ecig)[1]-dim(ecig2)[1]
  return(dE)
}

d2009<-deltaE(d,2009)
d2010<-deltaE(d,2010)
d2011<-deltaE(d,2011)
d2012<-deltaE(d,2012)
d2013<-deltaE(d,2013)
d2014<-deltaE(d,2014)
d2015<-deltaE(d,2015)
d2016<-deltaE(d,2016)
d2017<-deltaE(d,2017)
d2018<-deltaE(d,2018)
d2019<-deltaE(d,2019)
d2020<-deltaE(d,2020)
d2021<-deltaE(d,2021)
d2022<-deltaE(d,2022)
#vector of deltaE[t]s
ds<-c(d2009,d2010,d2011,d2012,d2013,d2014,d2015,d2016,d2017,d2018,d2019,d2020,d2021,d2022)
yrs<-c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)
df2<-data.frame(yrs)
df2<-cbind(df2,ds)
names(df2)<-c('year','change in e-cigarette users')
plot_ecig(df2,"Change in e-cigarette users")

#linear regression
#X=(delta t, delta t E[t], delta E^2[t]) and 
#beta = (pm, q-p, -q/m).
#Linear regression is delta E[t] = X*beta
#E[t] is percentage of e-cig users at t
#deltat<-4/52#change in time in years: 4 weeks per month and 52 months per year
deltat<-12 #change in time in months: 12 months in a year
deltaT<-rep(deltat,length(ps))
deltaT_ps<-ps
for(i in 1:length(ps)){
   deltaT_ps[i]<-deltaT[i]*ps[i]
}
ps2<-ps
for(i in 1:length(ps)){
  ps2[i]<-ps[i]^2
}
X<-data.frame(deltaT)
X<-cbind(X,deltaT_ps)
X<-cbind(X,ps2)
X<-cbind(X,ds)
names(X)<-c('deltaT','deltaT_E','EE','deltaE')

ecig.lm <- lm(deltaE ~ deltaT + deltaT_E + EE, data=X)
summary(ecig.lm)
summary(ecig.lm)$r.squared

#fit a Bass model using linear regression to predict E[t] from E[t-1] (E[t] is percentage of e-cig users of year t)
#bass model: E[t]=a+b*E[t-1]+c*E[t-1]^2 where a=pm, b=q-p, c=-q/m
#Y=X'beta where Y=E[t], X=(1,E[t-1],E[t-1]^2)', beta=(a,b,c)' and ' is transpose to a column vector
#t is from 2010 to 2022
ones<-rep(1,length(ps)-1)
ps2<-ps[1:length(ps)-1] #E[t] from 2009 to 2021
ps3<-ps2
for(i in 1:length(ps2)){
  ps3[i]<-ps2[i]^2
}
Y<-ps[2:length(ps)]#E[t] from 2010 to 2022
X<-data.frame(ones)
X<-cbind(X,ps2)
X<-cbind(X,ps3)
X<-cbind(X,Y)
names(X)<-c('one','E_prev','E_prev_E_prev','E_now')
ecig.lm<-lm(E_now ~ one + E_prev + E_prev_E_prev, data=X)
summary(ecig.lm)
summary(ecig.lm)$r.squared

