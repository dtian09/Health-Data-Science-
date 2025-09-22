library(naivebayes)
library(readr)
library(tidyverse)

filter <- dplyr::filter #resolve conflicts of filter

d<-read_rds(file="X:/Shared/STS data/STS and ATS files Oct 22/Latest omnibus SPSS data file/stsdata_oct22(wave1to192).rds")

df2009 <- d %>%
  filter(xyear==2009,smoker==1) %>% 
  mutate(ids=1:nrow(.))

agecuts=c(0,24,34,44,54,64,100) #this creates the same age groups as Fidler paper (2010)
agelabs <- c("16-24","25-34","35-44","45-54","55-64","65+") #add labels for age cats 

df2009<-mutate(df2009, agegroup = cut(actage,    #add agegroup variable to df2009
                                      breaks=agecuts,
                                      labels=agelabs),
                       qsuper_recoded = as.factor(ifelse(qsuper %in% c("Refused", "Don't know","Not Stated "), "Refused or Don't know or Not Stated", levels(df2009$qsuper)[qsuper])))

#merge age groups "16-24","25-34","35-44","45-54","55-64","65+" into: 16-34, 35-54, 55+
df2009 <-mutate(df2009,agegroup2=as.factor(ifelse(agegroup %in% c("16-24","25-34"), "16-34",ifelse(agegroup %in% c("35-44","45-54"),"35-54","55+"))))

traindata <- df2009 %>%
              drop_na(qsuper_recoded) %>%
              #dplyr::select(sexz, agegroup2, sgz, qsuper_recoded)
              dplyr::select(sexz, agegroup2, sgz, nsec)
             #dplyr::select(sexz, agegroup, sgz, nsec)

traindata$nsec <- as.factor(traindata$nsec)
traindata$sexz <- as.factor(traindata$sexz)
traindata$sgz <- as.factor(traindata$sgz)
traindata$sgz <- as.numeric(traindata$sgz)
df2009$sgz <- as.numeric(df2009$sgz)
traindata$agegroup2 <- as.numeric(traindata$agegroup2)
df2009$agegroup2 <- as.numeric(df2009$agegroup2)

#nb<-naive_bayes(qsuper_recoded ~ .,traindata)
nb<-naive_bayes(nsec ~ .,traindata)
summary(nb)

testdata <- traindata[,-4]
probs<-predict(nb,testdata,type='prob')
print(probs)

prop_qsuper_recoded <- df2009 %>% 
  group_by(sexz, agegroup2, sgz, qsuper_recoded, .drop=FALSE) %>%
  drop_na(sexz) %>%
  drop_na(agegroup2) %>%
  drop_na(sgz) %>%
  drop_na(qsuper_recoded) %>%
  summarize(total=sum(X.weight0)) %>% #weighted total of each group
  group_by(sexz, agegroup2, sgz, .drop=FALSE) %>%
  mutate(cat = paste(sexz, agegroup2, sgz, sep="_"), 
         prop=total/sum(total)) %>%
  dplyr::select(cat, qsuper_recoded, prop)

prop_nsec <- df2009 %>% 
  group_by(sexz, agegroup2, sgz, nsec, .drop=FALSE) %>%
  drop_na(sexz) %>%
  drop_na(agegroup2) %>%
  drop_na(sgz) %>%
  summarize(total=sum(X.weight0)) %>% #weighted total of each group
  group_by(sexz, agegroup2, sgz, .drop=FALSE) %>%
  mutate(cat = paste(sexz, agegroup2, sgz, sep="_"), 
         prop=total/sum(total)) %>%
  dplyr::select(cat, nsec, prop)

prop_nsec <- df2009 %>% 
  group_by(sexz, agegroup, sgz, nsec, .drop=FALSE) %>%
  drop_na(sexz) %>%
  drop_na(agegroup) %>%
  drop_na(sgz) %>%
  summarize(total=sum(X.weight0)) %>% #weighted total of each group
  group_by(sexz, agegroup, sgz, .drop=FALSE) %>%
  mutate(cat = paste(sexz, agegroup, sgz, sep="_"), 
         prop=total/sum(total)) %>%
  dplyr::select(cat, nsec, prop)

sample_qsuper_recoded <- function(data, rates){
  vals<-levels(as.factor(rates$qsuper_recoded))
  inputs<-data[,-ncol(data)]
  probs<-predict(nb,inputs,type='prob')
  data$prob <- probs[,1] #prob of class 0
  rates <- rates %>% 
    filter(cat==unique(data$cat))
  data$qsuper_recoded_imputed = ifelse(data$prob<=rates$prop[1], vals[1], vals[2])
  return(data)
}

sample_nsec <- function(data, rates){
  vals<-levels(as.factor(rates$nsec))
  inputs<-data[,-ncol(data)]
  probs<-predict(nb,inputs,type='prob')
  data$prob <- probs[,1] #prob of class 0
  rates <- rates %>% 
    filter(cat==unique(data$cat))
  data$nsec_imputed = ifelse(data$prob<=rates$prop[1], vals[1], vals[2])
  return(data)
}

w39to42 <- d %>% 
  filter(xwave>=39 & xwave<=42,smoker==1) %>% #only waves 39 to 42 have complete nsec data
  filter(actage>=25 & actage<=65) %>%     
  mutate(ids=1:nrow(.))

w39to42 <-
  mutate(w39to42,
         agegroup = cut(actage,   
                        breaks=agecuts,
                        labels=agelabs),
         qsuper_recoded = as.factor(ifelse(qsuper %in% c("Refused", "Don't know","Not Stated "), "Refused or Don't know or Not Stated", levels(w39to42$qsuper)[qsuper]))) %>%
  drop_na(sexz) %>%
  drop_na(sgz) %>%
  drop_na(qsuper_recoded) %>%
  drop_na(agegroup) %>%
  drop_na(sregz) %>%
  #drop_na(gore) %>%
  mutate(agegroup2=as.factor(ifelse(agegroup %in% c("16-24","25-34"), "16-34",ifelse(agegroup %in% c("35-44","45-54"),"35-54","55+")))) %>%
  drop_na(agegroup2) %>%
  #dplyr::select(sexz, agegroup2, sgz, qsuper_recoded)
  dplyr::select(sexz, agegroup2, sgz, nsec)
  #dplyr::select(sexz, agegroup, sgz, nsec)

#testdata<-traindata
testdata<-w39to42
testdata$sexz <- as.factor(testdata$sexz)
testdata$sgz <- as.factor(testdata$sgz)
testdata$sgz <- as.numeric(testdata$sgz)
testdata$agegroup2 <- as.numeric(testdata$agegroup2)
testdata$nsec <- as.factor(testdata$nsec)

testdata_imputed <- testdata %>% 
  mutate(cat = paste(sexz, agegroup2, sgz, sep="_")) %>% 
  #mutate(cat = paste(sexz, agegroup, sgz, sep="_")) %>% 
  group_by(cat) %>% 
  do(sample_nsec(., prop_nsec))

cor(as.numeric(testdata_imputed$nsec),as.numeric(testdata_imputed$nsec_imputed),method='pearson')
tbl<-table(testdata_imputed$nsec,testdata_imputed$nsec_imputed)#contingency table is a confusion matrix:  (imputed nsec)

testdata_qsuper_recoded_imputed <- testdata %>% 
  mutate(cat = paste(sexz, agegroup2, sgz, sep="_")) %>% 
  group_by(cat) %>% 
  do(sample_qsuper_recoded(., prop_qsuper_recoded))

cor(as.numeric(testdata_qsuper_recoded_imputed$qsuper_recoded),as.numeric(testdata_qsuper_recoded_imputed$qsuper_recoded_imputed),method='pearson')
tbl<-table(testdata_qsuper_recoded_imputed$qsuper_recoded,testdata_qsuper_recoded_imputed$qsuper_recoded_imputed)#contingency table is a confusion matrix:  (imputed nsec)

#                                              0  1 
#                              (actual nsec) 0 TN FP 
#                                            1 FN TP
print(tbl)
TNR<-tbl[1]/(tbl[1]+tbl[3]) * 100 #true negative rate (accuracy of imputed nsec taking 0) 
TPR <-tbl[4]/(tbl[2]+tbl[4])* 100 #true positive rate (accuracy of imputed nsec taking 1)
print(TPR)
print(TNR)
#when setting sex and nsec as factors and setting agegroup2 and sgz as numeric, the testing results:
#Gaussian kernels used to model the numeric features
#   0   1
#0 199  76
#1  92 196
#TPR=68
#TNR=72.4
#when setting sex, agegroup2 and nsec as factors and sgz as numeric, the testing results:
# 0   1
#0 211  64
#1 102 186
#TPR=64.6
#TNR=76.7
#when setting sex, agegroup2, sgz and nsec to factors and testing results:  
#   0   1
#0 150 125
#1  98 190  
#TPR=66.0
#TNR=54.5
#for predicting qsuper_recoded from sex, agegroup2 and sgz, when setting sex and nsec as factors and setting agegroup2 and sgz as numeric, the testing results:
#Gaussian kernels used to model the numeric features
#    No  Yes
#No  247 129
#Yes 93  94
#TPR=50
#TNR=65


