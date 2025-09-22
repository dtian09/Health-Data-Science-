#program to calculate non-missing values of STS data
library(readr)
library(tidyverse)

#d<-read_rds(file="X:/Shared/STS data/STS and ATS files Oct 22/Latest omnibus SPSS data file/stsdata_oct22(wave1to192).rds")
#d<-read_rds(file="X:/Shared/STS data/STS and ATS files Dec 22/Latest omnibus SPSS data file/wave1to194.rds")
#d<-read_rds(file="X:/Shared/STS data/STS and ATS files Jan 23/Latest omnibus SPSS data file/wave1to195.rds")
#d<-read_rds(file="X:/Shared/STS data/STS and ATS files May 23/Latest omnibus SPSS data file/wave199.rds")
#d<-read_rds(file="X:/Shared/STS data/STS and ATS files August 23/STS and ATS files August 23/Latest omnibus SPSS data file/wave202.rds")
#d<-read_rds(file="X:/Shared/STS data/STS and ATS files Dec 23/Latest omnibus SPSS data file/wave206.rds")
d<-read_rds(file="X:/Shared/STS data/STS and ATS files August 24/STS and ATS files August 24/Latest omnibus SPSS data file/wave214.rds")
  
indx<-d$xwave==195
w195<-d[indx,]

df2012 <- d %>% filter(xyear==2012) %>% mutate(ids=1:nrow(.))

missing_values <- function(df,var,print_results){
  #compute missing values of a variable var in dataframe
  #input: df, a dataframe  (rows are participants and columns are variables)
  #       var a variable (a column of a data frame)
  #       print_results (T or F)
  #output: percentage of missing values (NA) of var
  missing <- sum(is.na(var))
  percent_missing <- round((missing / dim(df)[1]) * 100,2) 
  if (print_results==T){
    print(sprintf("missing values: %i, percentage of missing values: %f percent",missing,percent_missing))
  }
  return(percent_missing)
}

non_missing_values_of_each_variable <- function(df){
  #compute percentage of non-missing values of each variable of a data frame (rows are participants and columns are variables)
  #input: df, a dataframe  (rows are participants and columns are variables)
  #output: a list of percentages of non-missing values of variables
  c=dim(df)[2]
  r=dim(df)[1]
  n<-names(df)
  l<-list()
  for (j in 1:c){
    percent_missing<-missing_values(df,df[,j],F)
    percent_non_missing<-100-percent_missing
    l<-append(l,percent_non_missing)
  }
  return(unlist(l))
}

non_missing_values_of_waves <- function(df,vars){
  #calculte percentage of non-missing values (response rate) of each variable of each wave in a STS dataframe
  #input: df, dataframe of numerous waves 
  #       vars, variables to select
  #output: a dataframe with each column representing a wave number and each row containing the non-missing percentages of a variable across different waves
  #format of output:
  #variables wave1 wave2 ... wave194
  #q632x5  40.6  37.95 ... 25.5  
  #q632x7  25.9  26.19 ... 23.63
  #X.qfcom22  0.0  0.00 ... 0.00
  #X.qfcom23  0.0  0.00 ... 0.00
  
  w<-unique(df$xwave)
  df2<-data.frame(vars)#percentage of non-missing values of each variable of each wave in STS dataset
                       #first column of dataframe is variables names
  for(i in 1:length(w)){
    indx=df$xwave==w[i]
    wave=df[indx,]
    wave <- subset(x=wave,select=vars)
    non_missing_values <- non_missing_values_of_each_variable(wave)
    df2<-cbind(df2,non_missing_values)
    #df2<-data.frame(df2,non_missing_values)
  }
  n<-list('variables')
  for(i in 1:length(w)){
    n<-append(n,paste('wave',as.character(w[i]),sep=""))
  }  
  names(df2)<-unlist(n)#replace names of columns with meaningful names
  return(df2)
}

non_missing_waves <- function(df,vars){
  #for each variable, get those wave numbers with > 0% non-missing values for the variable 
  #input: df, dataframe of numerous waves 
  #       vars, variables to select
  #output: a list of strings with each string value representing a variable and its non-missing wave numbers
  #format of output: ["X.qfeel3,5-49,52,55-149","X.qfeel4,1-149",...]
  non_missing <- non_missing_values_of_waves(df,vars)
  r<-nrow(non_missing)
  c<-ncol(non_missing)
  n<-names(non_missing)#e.g. n=[variables,wave1,wave2,...waven]
  non_missing_waves_of_vars<-list()
  for(i in 1:r){
    set_init_wave<-T
    var_name<-non_missing[i,1]
    l<-list(var_name)
    prev_wave<-NULL #the wave which the previous value corresponds to
    current_wave<-NULL #the wave which the current value corresponds to
    waves<-""
    for(j in 2:c)
    {
      if(non_missing[i,j]>0)
      {
        current_wave<-j-1
        if(set_init_wave==T){
          init_wave<-current_wave #initialize wave number of this wave sequence e.g. 1 of sequence 1,2,3,...,10
          set_init_wave<-F
          waves<-paste(waves,init_wave)
        }
        if(is.null(prev_wave)){#this column is the 1st wave with non-missing values of the variable
          prev_wave<-current_wave
          init_wave<-current_wave
        }
        else{
          if(current_wave - prev_wave==1 && j < c){#case1: input=1,2,3,...,10 and reaches an element before 10
            prev_wave<-current_wave
          }
          else if(current_wave - prev_wave==1 && j == c){#case2: input=1,2,3,...,max column and reaches max column  
            s=paste(as.character(init_wave),as.character(current_wave),sep=" - ")
            s=paste(s,",",sep="")
            waves=paste(waves,s)
          }
          else if(current_wave - prev_wave > 1)#case2: current_wave - prev_wave > 1 e.g. 10 (prev_wave), 13 (current_wave)
          {
            if(init_wave < prev_wave){#case2.1: 1 (init_wave),2,3...,10 (prev_wave), 13 (current_wave) or 1 (init_wave),2,3...,10 (prev_wave), 13 (current_wave), 14, ..., 30 and create "1 - 10"
              s=paste(as.character(init_wave),as.character(prev_wave),sep=" - ")
              s=paste(s,",",sep="")
              waves=paste(waves,s)
            }
            else{#case2.2: 10 (init_wave and prev_wave), 13 (current_wave) and create "10"
              s=paste(as.character(prev_wave),",",sep="")
              waves=paste(waves,s)
            }
            prev_wave<-current_wave
            init_wave<-current_wave
          }
        }
      }
      else{#non_missing[i,j]==0
        if(prev_wave == current_wave && j == c){#case2: input=5,...,10 and reaches the max column > 10
          s=paste(as.character(init_wave),as.character(current_wave),sep=" - ")
          s=paste(s,",",sep="")
          waves=paste(waves,s)
        }
      }
    }
    l<-append(l,waves)
    non_missing_waves_of_var<-paste(l,collapse = '')
    non_missing_waves_of_vars<-append(non_missing_waves_of_vars,non_missing_waves_of_var)
  }
  return(non_missing_waves_of_vars)
}

non_missing_waves2 <- function(df,vars){
  #for each variable, get those wave numbers with > 0% non-missing values
  #input: df, dataframe of numerous waves 
  #       vars, variables to select
  #output: a list of strings with each string value representing a variable and its non-missing wave numbers 
  #format of output: ["X.qfeel3,5,6,9,149","X.qfeel4,10,12,13,123",...]
  non_missing <- non_missing_values_of_waves(df,vars)
  r<-nrow(non_missing)
  c<-ncol(non_missing)
  non_missing_waves_of_vars<-list()
  for(i in 1:r){
    var_name<-non_missing[i,1]
    l<-list(var_name)
    for(j in 2:c)
    {
      if (non_missing[i,j]>0){
        l<-append(l,paste(",",j-1))
      }
    }
    non_missing_waves_of_var<-paste(l,collapse = '')
    non_missing_waves_of_vars <- append(non_missing_waves_of_vars,non_missing_waves_of_var)
  }
  return(non_missing_waves_of_vars)
}

non_missing_values_of_years <- function(df,vars){
  #calculate percentage of non-missing values (response rate) of each variable in all the waves of each year in a STS dataframe
  #input: df, dataframe of many waves 
  #       vars, variables to select
  #output: a dataframe with columns representing years and each row representing non-missing values of a variable across different years
  #format of output:
  #variables  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021 2022
  #q632x5 41.35 41.36 35.84 34.18 27.60 24.20 16.02 18.08 21.06 20.97 19.41 18.06 18.28 16.61 16.38 17.35 17.5
  #q632x7 26.04 25.52 23.04 21.98 21.65 21.33 14.18 15.53 17.71 17.25 15.96 14.71 14.95 13.71 12.43 13.09 18.2
  #X.qfcom22  0.00  0.00  0.00  0.00  2.40  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00 0
  #X.qfcom23  0.00  0.00  0.00  0.00  2.40  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00 0
  y<-unique(df$xyear)
  df2<-data.frame(vars)#percentage of non-missing values of each variable of all the waves of each year in STS dataset
  #first column of dataframe is variables names
  for(i in 1:length(y)){
    indx=df$xyear==y[i]
    wave=df[indx,]
    wave <- subset(x=wave,select=vars)
    non_missing_values <- non_missing_values_of_each_variable(wave)
    df2<-cbind(df2,non_missing_values)
  }
  n<-list('variables')
  for(i in 1:length(y)){
    n<-append(n,as.character(y[i]))
  }  
  names(df2)<-unlist(n)#replace names of columns with meaningful names
  return(df2)
}

non_missing_years <- function(df,vars){
  #for each variable, get those years with > 0% non-missing values for the variable
  #input: df, dataframe of numerous waves of different years 
  #       vars, variables to select
  #output: a list of string values with each value representing a variable and the years with > 0% non-missing values for the variable 
  #format of output:
  #["X.qfeel3,2009-2013,2015,2016-2019","X.qfeel4,2009-2019",...]
  non_missing <- non_missing_values_of_years(df,vars)
  r<-nrow(non_missing)
  c<-ncol(non_missing)
  non_missing_years_of_vars<-list()
  for(i in 1:r){
    set_init_year<-T
    var_name<-non_missing[i,1]
    l<-list(var_name)
    prev_year<-NULL #the year which the previous value corresponds to
    current_year<-NULL #the year which the current value corresponds to
    years<-""
    for(j in 2:c)
    {
      if(non_missing[i,j]>0)
      {
        current_year<-as.integer(names(non_missing)[j])
        if(set_init_year==T){
          init_year<-current_year #initialize year number of this sequence e.g. 2011 of sequence 2011,2012,2013,...,2019
          set_init_year<-F
          years<-paste(years,init_year)
        }
        if(is.null(prev_year)){#this column is the 1st year with non-missing values of the variable
          prev_year<-current_year
          init_year<-current_year
        }
        else{
          if(current_year - prev_year==1 && j < c){#case1: input is like 2011,2012,2013,...,2019 and reaches an element before 2019
            prev_year<-current_year
          }
          else if(current_year - prev_year==1 && j == c){#case2: input is like 2011,2012,2013,...,2019 and reaches 2019  
            s=paste(as.character(init_year),as.character(current_year),sep=" - ")
            s=paste(s,",",sep="")
            years=paste(years,s)
          }
          else if(current_year - prev_year > 1)#case2: e.g. 2010 (prev_year), 2013 (current_year)
          {
            if(init_year < prev_year){#case2.1: input is like 2011 (init_year),2012,...,2018 (prev_year), 2019 (current_year) or 2009 (init_year),2010,...,2014 (prev_year), 2015 (current_year), 2016, ..., 2019 and create "init_year - prev_year"
              s=paste(as.character(init_year),as.character(prev_year),sep=" - ")
              s=paste(s,",",sep="")
              years=paste(years,s)
            }
            else{#case2.2: 2010 (init_year and prev_year), 2013 (current_year),...,2019 and create "2010"
              s=paste(as.character(prev_year),",",sep="")
              years=paste(years,s)
            }
            prev_year<-current_year
            init_year<-current_year
          }
        }
      }
      else{#non_missing[i,j]==0
        if(prev_year == current_year && j == c){#case2: input=5,...,10 and reaches the max column > 10
          s=paste(as.character(init_year),as.character(current_year),sep=" - ")
          s=paste(s,",",sep="")
          years=paste(years,s)
        }
      }
    }
    l<-append(l,years)
    non_missing_years_of_var<-paste(l,collapse = '')
    non_missing_years_of_vars<-append(non_missing_years_of_vars,non_missing_years_of_var)
  }
  return(non_missing_years_of_vars)
}

get_questionnaire_text_of_variables <- function(vars2){
  excelfile<-"X:/Shared/STS data/STS and ATS files Dec 22/Latest wave variables/Omnibus Variables Wave on Wave DEC22 W194_v1.xlsx"
  library(readxl)
  df<-read_excel(excelfile, sheet = "Omnibus Variables Spec")
  vars3<-vars2
  for(i in 1:length(vars2)){
    vars3[i]<-gsub("X.", "@", vars2[i])
    vars3[i]<-tolower(vars3[i])
  }
  df$var_names<-tolower(df$var_names)
  text <- filter(df,var_names %in% vars3) %>%
          dplyr::select(var_names, Wave_number) 
  vars4<-setdiff(vars3,text$var_names)
  if(length(vars4)==0){
    return(text)
  }
  else{
      excelfile2<-"X:/Shared/STS data/STS and ATS files Dec 22/Variable meta information/Variable meta information Nov 2022 VB.xlsx"
      df2<-read_excel(excelfile2, sheet = "Baseline")
      df2$Variables<-tolower(df2$Variables)
      text2 <- filter(df2,Variables %in% vars4) %>%
              dplyr::select(Variables, Description)
      vars5<-setdiff(vars4,text2$Variables)
      text<-rename(text, Variables=var_names, Description=Wave_number)
      return(rbind(text,text2))
  }
}

non_missing<-non_missing_values_of_waves(d,vars2)
non_missing<-non_missing_values_of_years(d,vars2)
#write.csv(non_missing,file="percentage_non_missing.csv",row.names = F)
#non_missing<-non_missing_values_of_waves(d,c('qmotiv'))
#non_missing<-non_missing_values_of_waves(d,c('nsec'))
#write.csv(non_missing,file="percentage_non_missing_of_nsec.csv",row.names = F)
write.csv(non_missing,file="percentage_non_missing_of_COMB.csv",row.names = F)
write.csv(non_missing,file="percentage_non_missing_of_COMB_years.csv",row.names = F)
non_missing_waves_of_vars<-non_missing_waves(non_missing)
write_lines(non_missing_waves_of_vars,file="waves_with_non_missing_values_of_variables.txt")
non_missing_waves2_of_vars<-non_missing_waves2(non_missing)
write_lines(non_missing_waves2_of_vars,file="waves_with_non_missing_values_of_variables2.txt")
non_missing_years_of_vars<-non_missing_years(non_missing)
write_lines(non_missing_years_of_vars,file="years_with_non_missing_values_of_variables.txt")
non_missing<-non_missing_values_of_waves(d,urges_to_smoke)
non_missing_waves_of_vars<-non_missing_waves(d,urges_to_smoke)
non_missing_years_of_vars<-non_missing_years(d,urges_to_smoke)
non_missing<-non_missing_values_of_waves(w195,vars2)
non_missing_waves_of_vars<-non_missing_waves(w195,vars2)

text<-get_questionnaire_text_of_variables(exsmoker)

knowledgeofsmokingharms <- c("X.qfeel3",
           "X.qfeel4",
           "X.qfeel15",
           "X.qfcom3")

relative_harm_perceptions_of_e_cigarettes <- c(
           "qimw139_0212",
           "qnharm",
           "qimw982")

capacity_for_self_efficacy <- c(
           "X.qfeel20",
           "X.F632e6")

mental_health <- c(
           "qharm_o",
           "eq5de",
           "qimw139_016")

alcohol_use <- c("audit1")

actualage <- c("actage")

sex <- c("sexz")

disability <- c("dis")

motivation_to_stop_smoking <- c(
           "qmotiv",
           "X.qfeel11",
           "X.qfeel5",
           "X.qfeel6",
           "X.qfeel7",
           "X.qfeel8",
           "X.qfeel9",
           "X.qfeel10",
           "X.qfeel11",
           "X.qfeel12",
           "X.qfeel13",
           "X.qfeel14")

enjoyment_of_smoking <- c(
           "X.qfeel22",
           "q632x4",
           "X.qfac1")

positive_smoker_identity <- c(
           "q632a4a",
           "X.qfac9",
           "X.qfeel1",
           "X.E632e5")

nonsmoker_identity <- c(
           "X.qfeel6",
           "X.qfeel7",
           "X.A632e1")
        
cessation_harm_motives <- c(
           "X.qfeel3",
           "X.qfeel4",
           "X.qfeel15",
           "X.qfcom3")

urges_to_smoke <- c(
            "q632x5",
            "q632x7",
            "X.qfcom22",
            "X.qfcom23",
            "X.qfcom24")

heaviness_of_smoking_index <- c(
           "q632a9",
           "q632a9_1",
           "qhand1",
           "qhand1_1",
           "q632a0",
           "q632a0_1",
           "qhand2",
           "qhand2_1",
           "q632b1",
           "q632b1_1",
           "qhand3",
           "qhand3_1",
           "basecpd",
           "basecpd2")

anti_smoking_norms <- c(
           "q632e10",
           "X.qfeel12",
           "qpropsmo")

exposure_to_regular_smoking <- c(
           "qimw8022",
           "qimw8023",
           "qimw8024",
           "qimw8025",
           "qimw8026",
           "qimw8027",
           "qimw8028",
           "qimw981")

proportion_of_smoking_that_occurs_in_social_situations <- c("qimw891")

identity_as_a_social_smoker <- c("qimw892")

brief_health_professional_advice <- c("X.q632c3a_1")

social_grade <- c("sgz")

education <- c("qual")

ethnicity <- c("ethnic")

children_in_the_household <- c("numkid2")

availability_of_behavioural_support <- c("qimw125_1")

availability_of_e_cigarettes <- c("usingecig")

financial_cost_of_smoking <- c(
           "X.qfeel17",
           "X.qfcom17")

availability_of_cigarettes <- c(
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
           "X.qbuy120")

never_smoker <- c("eversmo")

smoker <-c("smoker")

exsmoker <- c("exsmoker")

quitstat <- c("quitstat")

cutting_down <- c(
           "q632a2",
           "q632a2i",
           "cutdown")

type_of_cigarette_smoked <- c(
            "typcig" #"Typcig"
             )

past_year_quit_attempt <- c(
           "trylya",
           "trylyb",
           "trylyc")

use_of_behavioural_support <- c(
           "X.q632b5",
           "X.q632b6",
           "X.q632b7",
           "X.q632b1e",
           "X.q632b1f",
           "X.q632b1g",
           "X.q632b1h",
           "X.q632b1i",
           "X.q632b1j")

use_of_pharmacological_support <- c(
           "X.q632b3",
           "X.q632b4")

use_of_e_cigarettes <- c("X.q632b1k")

#plot heatmap to visualize waves with >0% non-missing values
binarize_values_of_dataframe <- function(df,vars){
  #input: df, dataframe of numerous waves 
  #       vars, variables to select
  #output: a matrix of 0s and 1s corresponding to 0 and > 0 values in non-missing dataframe
  non_missing<-non_missing_values_of_waves(df,vars)
  r<-nrow(non_missing)
  c<-ncol(non_missing)
  non_missing_binary_values <- matrix(0,nrow(non_missing),ncol(non_missing)-1)
  for(i in 1:r){
    var_name<-non_missing[i,1]
    for(j in 2:c)
    {
      if (non_missing[i,j]>0){
        non_missing_binary_values[i,j-1] <- 1
      }
    }
  }
  #print(non_missing_binary_values)
  return(non_missing_binary_values)
}
vars2 <- vars2[-c(length(vars2)-1,length(vars2))] #remove xyear and xwave variables
non_missing2<-binarize_values_of_dataframe(d,vars2) 

heatmap(non_missing2,margin=c(2,0.5),Rowv=NA,Colv=NA, scale = "column", cexRow = 0.1 + 1/log10(dim(non_missing2)[1]), labRow = non_missing$variables,labCol = unique(d$xwave),col = c("yellow", "blue"))#yellow represent 100% missing values (0% non-missing values) of a variable, blue represent >0% non-missing values of a variable
legend(x="bottomright", legend=c("0% non-missing values", ">0% non-missing values"),fill=c("yellow", "blue"))

df2010 <- d %>% 
  filter(xyear==2010,smoker==1) %>%
  mutate(ids=1:nrow(.))

non_missing<-non_missing_values_of_waves(df2010,c('nsec'))

#social demographic variables plus xwave
vars<-c( "xwave",
         "sexz",
         "agegroup",
         "sregz",
         "gore",
         "gor",
         "qhhold",
         "qwork",
         "qemploy",
         "q632e55",
         "qstaff",
         "qsuper",
         "qprof",
         "work",
         "cie",
         "mshop",
         "super",
         "wrkcie",
         "maritl",
         "parent5",
         "numhhd",
         "numkid",
         "person1", #credit card
         "person2", #debit card
         "person3", #sky sports
         "durables10",
         "durablesn32", #credit card, debit card, store card etc ownership (0 or 1)
         "durablesn33",
         "durablesn34",
         "durablesn35",
         "durablesn36",
         "durablesn37",
         "durablesn38",
         "daily1", #daily news paper read regularly (0 or 1)
         "daily2",
         "daily3",
         "daily4",
         "daily5",
         "daily6",
         "daily7",
         "daily8",
         "daily9",
         "daily10",
         "daily11",
         "daily12",
         "daily13",
         "daily14",
         "daily15",
         "daily16",
         "daily17",
         "daily18",
         "daily19",
         "daily20",
         "daily21",
         "daily22",
         "daily23",
         "daily24",
         "daily25",
         "sunday1", #sunday news papers read regularly (0 or 1)
         "sunday2",
         "sunday3",
         "sunday4",
         "sunday5",
         "sunday6",
         "sunday7",
         "sunday8",
         "sunday9",
         "sunday10",
         "sunday11",
         "sunday12",
         "sunday13",
         "sunday14",
         "sunday15",
         "sunday16",
         "sunday17",
         "sunday18",
         "sunday19",
         "sunday20",
         "tenure",
         "tennet",
         "income3",
         "ethnic",
         "lstage",
         "qual",
         "LAcode",
         "nsec",
         "sgz"
)

df2012 <- d %>% 
  filter(xyear==2012,smoker==1) %>%
  mutate(ids=1:nrow(.))

agecuts=c(0,24,34,44,54,64,100) #this creates the same age groups as Fidler paper (2010)
agelabs <- c("16-24","25-34","35-44","45-54","55-64","65+") #add labels for age cats 

df2012<-mutate(df2012, agegroup = cut(actage,    #add agegroup variable to df2011
                                      breaks=agecuts,
                                      labels=agelabs))

non_missing<-non_missing_values_of_waves(df2012,vars)
write.csv(non_missing,file="percentage_non_missing_of_STS2012_social_demographics.csv",row.names = F)

non_missing<-non_missing_values_of_waves(df2011,vars2) #COMB variables
write.csv(non_missing,file="percentage_non_missing_of_STS2011_COM-B.csv",row.names = F)

df2014 <- d %>% 
  filter(xyear==2014,smoker==1) %>%
  mutate(ids=1:nrow(.))

df2014<-mutate(df2014, agegroup = cut(actage,    #add agegroup variable to df2011
                                      breaks=agecuts,
                                      labels=agelabs))

non_missing<-non_missing_values_of_waves(df2014,vars)
write.csv(non_missing,file="percentage_non_missing_of_STS2014_social_demographics.csv",row.names = F)

non_missing2<-read.csv("percentage_non_missing.csv")
non_missing2<-rbind(non_missing2,non_missing)
names(non_missing)[1]<-"variables"
write.csv(non_missing2,file="percentage_non_missing.csv",row.names = F)

order_waves_by_total_non_missing_values <- function(non_missing){
  #order waves by sum of percentages of non-missing values of variables
  s<-data.frame()
  for(j in 2:length(non_missing)){
    s<-rbind(s,c(j-1,sum(non_missing[,j])))
  }
  names(s)<-c('wave','sum_of_percentages_of_non-missing_values')
  return(s[order(s$'sum_of_percentages_of_non-missing_values',decreasing=T),])
}

s<-order_waves_by_total_non_missing_values(non_missing)
#heatmap to visualize percentages of non-missing values
#library(ggplot2)
#library(reshape)
#library(dplyr)
#non_missing<-read.csv("percentage_non_missing.csv")
#names(non_missing)<-c('variables',1:192)
#g<-melt(non_missing)
#ggplot(g, aes(variable,variables,fill=value)) + geom_tile()

#colnames(non_missing) <- paste0("wave", 1:c)
#rownames(non_missing) <- vars2
#my_colors <- colorRampPalette(c("cyan", "darkgreen"))
#heatmap(non_missing,col = my_colors(100))

#get the subset consisting of COM-B variables, xyear, xwave
vars2 <- c("X.qfeel3",
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
          "actage",
          "sexz",
          "dis",
          "qmotiv",
          "X.qfeel11",
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
          "q632x5",
          "q632x7",
          "X.qfcom22",
          "X.qfcom23",
          "X.qfcom24",
          "q632a9",
          "q632a9_1",
          "qhand1",
          "qhand1_1",
          "q632a0",
          "q632a0_1",
          "qhand2",
          "qhand2_1",
          "q632b1",
          "q632b1_1",
          "qhand3",
          "qhand3_1",
          "basecpd",
          "basecpd2",
          "q632e10",
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
          "numkid2",
          "qimw125_1",
          "usingecig",
          "X.qfeel17",
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
          "eversmo",
          "smoker",
          "exsmoker",
          "quitstat",
          "q632a2",
          "q632a2i",
          "cutdown",
          "typcig", #"Typcig",
          "trylya",
          "trylyb",
          "trylyc",
          "X.q632b5",
          "X.q632b6",
          "X.q632b7",
          "X.q632b1e",
          "X.q632b1f",
          "X.q632b1g",
          "X.q632b1h",
          "X.q632b1i",
          "X.q632b1j",
          "X.q632b3",
          "X.q632b4",
          "X.q632b1k",
          "xyear",
          "xwave")

s <- subset(x=d,select=vars)

vars <- vars[-(59:60)] #remove xyear and xwave variables

#select the subset of each year
indx=s$xyear == 2006
s2006=s[indx,]
s2006 <- subset(x=s2006,select=vars)
indx=s$xyear == 2007
s2007=s[indx,]
s2007 <- subset(x=s2007,select=vars)
indx=s$xyear == 2008
s2008=s[indx,]
s2008 <- subset(x=s2008,select=vars)
indx=s$xyear == 2009
s2009=s[indx,]
s2009 <- subset(x=s2009,select=vars)
indx=s$xyear == 2010
s2010=s[indx,]
s2010 <- subset(x=s2010,select=vars)
indx=s$xyear == 2011
s2011=s[indx,]
s2011 <- subset(x=s2011,select=vars)
indx=s$xyear == 2012
s2012=s[indx,]
s2012 <- subset(x=s2012,select=vars)
indx=s$xyear == 2013
s2013=s[indx,]
s2013 <- subset(x=s2013,select=vars)
indx=s$xyear == 2014
s2014=s[indx,]
s2014 <- subset(x=s2014,select=vars)
indx=s$xyear == 2015
s2015=s[indx,]
s2015 <- subset(x=s2015,select=vars)
indx=s$xyear == 2016
s2016=s[indx,]
s2016 <- subset(x=s2016,select=vars)
indx=s$xyear == 2017
s2017=s[indx,]
s2017 <- subset(x=s2017,select=vars)
indx=s$xyear == 2018
s2018=s[indx,]
s2018 <- subset(x=s2018,select=vars)
indx=s$xyear == 2019
s2019=s[indx,]
s2019 <- subset(x=s2019,select=vars)
indx=s$xyear == 2020
s2020=s[indx,]
s2020 <- subset(x=s2020,select=vars)
indx=s$xyear == 2021
s2021=s[indx,]
s2021 <- subset(x=s2021,select=vars)
indx=s$xyear == 2022 #up to wave191
s2022=s[indx,]
s2022 <- subset(x=s2022,select=vars)

wave192 <- readRDS(file="X:/Shared/STS data/STS and ATS files Oct 22/Latest omnibus SPSS data file/wave192.rds")
wave192 <- subset(x=wave192,select=vars)

####visualize missing values of all waves of each year
library(visdat)
library(ggplot2)
vis_miss(s2006,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2006.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2007,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2007.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2008,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2008.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2009,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2009.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2010,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2010.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2011,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2011.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2012,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2012.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2013,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2013.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2014,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2014.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2015,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2015.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2016,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2016.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2017,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2017.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2018,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2018.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2019,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2019.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2020,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2020.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2021,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2021.jpeg",path="U:/smoking cessation/plots",dpi=100)
vis_miss(s2022,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2022 (up to wave191).jpeg",path="U:/smoking cessation/plots",dpi=100)

vis_miss(wave192)
ggsave(file="missing values of COM-B variables wave192.jpeg",path="U:/smoking cessation/plots",dpi=100)

#visualize missing values of 2006 to 2022 (wave1 to wave191)
s <- subset(x=s,select=vars)
vis_miss(s,warn_large_data = F)
ggsave(file="missing values of COM-B variables 2006to2022(wave1 to wave191).jpeg",path="U:/smoking cessation/plots",dpi=100)

vars2 <- c(
  "qimw8022", 
  "qimw8023",
  "qimw8024",
  "qimw8025",
  "qimw8026",
  "qimw8027",
  "qimw8028",
  "qimw891",
  "qimw892")

s <- subset(x=d,select=vars2)
vis_miss(s,warn_large_data = F)
ggsave(file="missing values of more COM-B variables 2006to2022(wave1 to wave191).jpeg",path="U:/smoking cessation/plots",dpi=100)

#missing values of HSE data
hse <- readRDS(file="Z:/Data/eng_nat_tob_alc_data/intermediate_data/HSE_2013_to_2018.rds")
vis_miss(hse,warn_large_data = F)
ggsave(file="missing values of HSE_2013_to_2018.jpeg",path="U:/smoking cessation/plots",dpi=100)

#visualize missing values of each wave of 2008, 2009, 2010 and 2011 survies which contain the least amount of missing values i.e. the most information.
#get the subset consisting of COM-B variables, xyear, xwave

s <- subset(x=d,select=vars)

l<-length(vars)
l2<-l-1
vars2 <- vars[-(l2:l)] #remove xyear and xwave variables

plot_missing_values_of_each_wave_of_year <- function(df,vars,year,filepath){
  #plot percentage of missing values of each wave of a year e.g. 2008
  #df, dataframe of all the years 
  #vars, variables to select 
  #year, year of wave e.g. 2008
  #filepath, path of plot to save to
  #select the subset of a year
  indx=df$xyear==year
  df2=df[indx,]
  w<-unique(df2$xwave)
  for(i in 1:length(w)){
    indx=df2$xwave==w[i]
    wave=df2[indx,]
    wave <- subset(x=wave,select=vars)
    names(wave)<-c(1:length(vars))
    vis_miss(wave,warn_large_data = F)
    w_number <- as.character(w[i])
    year <- as.character(year)
    filename <- paste("missing values of COM-B variables of wave",w_number," of ",year,".jpeg",sep="")
    ggsave(file= filename,path=filepath,width=150,height=100,unit="mm",dpi=300)
  }
}

print_variables_names <- function(variablesnames){
  #print variable names corresponding to the variable numbers in the plot
  for (i in 1:length(variablesnames)){
    print(sprintf("%i: %s",i,variablesnames[i]))
  }
}

plot_missing_values_of_each_wave_of_year(d,vars2,2008,"U:/smoking cessation/plots/2008")
print_variables_names(vars2)
plot_missing_values_of_each_wave_of_year(d,vars2,2009,"U:/smoking cessation/plots/2009")
plot_missing_values_of_each_wave_of_year(d,vars2,2010,"U:/smoking cessation/plots/2010")
plot_missing_values_of_each_wave_of_year(d,vars2,2011,"U:/smoking cessation/plots/2011")


#hse has row numbers. This causes loop not working. To remove the row numbers, do the following transformation
hse <- readRDS("Z:/Data/eng_nat_tob_alc_data/intermediate_data/HSE_2013_to_2018.rds")#original data
write.csv(hse,file="hse_my.csv",col.names = F, row.names = F)
hse_my<-read.csv("hse_my.csv")
saveRDS(hse_my,file="hse_my.rds")
hse_my<-readRDS(file="hse_my.rds")

missing_values_of_each_variable <- function(df){
  #display and sort percentage of missing values of each variable of a dataframe
  c=dim(df)[2]
  r=dim(df)[1]
  n<-names(df)
  df2<-data.frame()
  for (j in 1:c){
    percent_missing<-missing_values(df,df[,j],F)
    var<-paste(n[j],"(",as.character(j),")",sep="")
    df2<-rbind(df2,list(var,percent_missing))
  }
  names(df2)<-c("variable","percentage_of_missing_values")
  df2 <- df2[order(df2$percentage_of_missing_values),]#sort in ascending order of percentage of missing values
  return(df2)
}

missing_values_of_each_variable(hse_my)

missing_values_of_each_wave_of_year <- function(df,vars,year){
  #sort and display percentage of missing values of each variable of each wave of a year
  #df, dataframe of all the years 
  #vars, variables to select 
  #year, year of wave e.g. 2008
  indx=df$xyear==year
  df2=df[indx,]
  w<-unique(df2$xwave)
  l<-list(data.frame())
  for(i in 1:length(w)){
    indx=df2$xwave==w[i]
    wave=df2[indx,]
    wave <- subset(x=wave,select=vars)
    print(sprintf("wave %i",w[i]))
    missing_values_of_wave <- missing_values_of_each_variable(wave)
    l<-append(l,paste("wave",as.character(w[i]),sep=""))
    l<-append(l,missing_values_of_wave)
  }
  return(l)
}

l<-missing_values_of_each_wave_of_year(d,vars,2008)
l<-missing_values_of_each_wave_of_year(d,vars,2009)
l<-missing_values_of_each_wave_of_year(d,vars,2010)
l<-missing_values_of_each_wave_of_year(d,vars,2011)

missing_values_of_wave <- function(df,vars,wavenum){
  #sort and display percentage of missing values of each variable of a wave
  #input: df, dataframe of all the years 
  #       vars, variables to select 
  #       wavenum, e.g. 13
  indx=df$xwave==wavenum
  wave=df[indx,]
  wave <- subset(x=wave,select=vars)
  print(sprintf("wave %i",wavenum))
  missing_values_of_wave <- missing_values_of_each_variable(wave)
}

print(missing_values_of_wave(d,vars,17))
#display all wave numbers of a year 
year<-2008
indx=d$xyear==year
d2=d[indx,]
table(d2$xwave)


