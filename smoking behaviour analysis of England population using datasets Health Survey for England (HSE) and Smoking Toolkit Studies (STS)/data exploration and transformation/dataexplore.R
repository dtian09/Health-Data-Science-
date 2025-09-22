#analysing smoking prevalence of Sept 22
#variable: q632e64 (J11 Current smoking habits)
#values: -1 (don't know), 1 (yes), 2 (no), 4(not stated)
#variable: q632a1 (A1 Whether smoke or have ever smoked)
#values of q632a1:
# -1	Don't know
# 1	I smoke cigarettes (including hand-rolled) every day
# 2	I smoke cigarettes (including hand-rolled), but not every da
# 3	I do not smoke cigarettes at all, but I do smoke tobacco of
# 4	I have stopped smoking completely in the last year
# 5	I stopped smoking completely more than a year ago
# 6	I have never been a smoker (ie. smoked for a year or more)
# 8	Not Stated

#dataset: stsdata_sept22_wave191.rds (wave1 to wave191)

plot_percent_of_non_missing_values <- function(list_of_dataframes,variablesnames){
  #input: list_of_dataframes where each dataframe contains TRUE counts and FALSE counts of a variable
  #       variablesnames, a vector of variable names 
  v <- list_of_dataframes
  l<-list()
  l2<-list()
  l3<-list()
  for (i in 1:length(v)){
    f <- v[[i]]
    out <- grep("FALSE",names(f),value=TRUE)
    out2<- grep("TRUE",names(f),value=TRUE)
    if (identical(out, character(0)) && out2 == "TRUE") #this variable contains missing values only
    {
      percent <- 0
      l2<-append(l2,as.character(0))
      l3<-append(l3,0)
    }
    else if(out == "FALSE" && identical(out2, character(0)))
    {
      percent <- 100
      l2<-append(l2,as.character(100))
      l3<-append(l3,100-1)
    }
    else{
      percent<- f[["FALSE"]]/sum(f)*100 #(f[["TRUE"]]+f[["FALSE"]])*100
      l2<-append(l2,as.character(f[["FALSE"]]))
      l3<-append(l3,f[["FALSE"]]/sum(f)*100-1)
    }
    l<-append(l,round(percent,2))
  }
  #print variable names corresponding to the variable numbers in the plot
  for (i in 1:length(l)){
    print(sprintf("%i: %s",i,variablesnames[i]))
  }
  H <- unlist(l)
  k <-length(l)
  M <- c(1:k)
  barplot(H,names.arg=M, xlab="Quitting variables",
          ylab="Percentage of non-missing responses (%)",
          col="white",
          #yaxp=c(0,18,18),
          ylim=c(0,100),
          #xlim=c(1,26),
          main="Non-missing (non-NA) responses of Quitting Variables")
  
  #add no. of non-NA responses to the bars 
  #v2<-unlist(l2)
  #v3<-unlist(l3)
  #df <- data.frame(responses=v2,
  #                 x=seq(1:length(l2)),
  #                 y=v3)
  #text(df$x, df$y, labels=df$responses)
}

d <- readRDS(file="X:/Shared/STS data/STS and ATS files Sep 22/Latest omnibus SPSS data file/stsdata_sept22_wave1to191.rds")
d2 <- readRDS(file="X:/Shared/STS data/STS and ATS files Oct 22/Latest omnibus SPSS data file/stsdata_oct22(wave1to192).rds")

#get the new variables of stsdata_oct22(wave1to192).rds
n<-names(d)
n2<-names(d2)
v<-n$V1 #convert a dataframe column to vector
v2<-n2$V1
new_vars<-setdiff(V2,V)
write.csv(new_vars,file="new_variables.csv",col.names = F, row.names = F)
#missing values of COM-B variables

missing_values <- function(d,var){
  #compute missing values of a variable var in d
  missing <- sum(is.na(var))
  percent_missing <- round((missing / dim(d)[1]) * 100,2) 
  print(sprintf("missing values: %i, percentage of missing values: %f percent",missing,percent_missing))
}

missing_values(d,d$X.qfeel3)

#no. of respondents of each wave
table(d$xwave)

#get the data of wave 191
indx=d$xwave == 191
wave191=d[indx,]

#count NA of wave 191 data (all 2423 responses are NA (missing values (unfilled)))
table(is.na(wave191$q632e64))
sum(is.na(wave191$q632e64))

#check dim of wave191 data
dim(wave191)

#count the frequency of each value of the variable (1st row: the values, 2nd row: their frequencies)
t<- table(wave191$q632a1)

#total no. of respondents of STS questionnaire (total=2423)
sum(t)

#percent of smokers of sept 22 (16.55%)
round((t["1"]+t["2"]+t["3"])/sum(t)*100,2)

###cutting down###
#plot percentage of non-missing responses (non-NA) of cutting down variables
f1<-table(is.na(wave191$q632a2))
f2<-table(is.na(wave191$q632a2i))#responses: 401 non-NA, 2022 NA
f3<-table(is.na(wave191$cutdown))  #cutdown is a derived variable
H <- c(f1["FALSE"]/sum(f1)*100,f2["FALSE"]/sum(f2)*100,f3["FALSE"]/sum(f3)*100)
M <- c("q632a2","q632a2i","cutdown")
barplot(H,names.arg=M, xlab="Cutting down variables",
        ylab="Percentage of non-missing responses (%)",
        col="white",
        yaxp=c(0,18,18),
        main="Non-missing (non-NA) responses of Cutting down Variables")
#add no. of non-NA responses to the bars 
df <- data.frame(responses=c(as.character(f1["FALSE"]), as.character(f2["FALSE"]), as.character(f3["FALSE"])),
                 x=c(1, 2, 3),
                 y=c(f1["FALSE"]/sum(f1)*100-1,f2["FALSE"]/sum(f2)*100-1, f3["FALSE"]/sum(f3)*100-1))
text(df$x, df$y, labels=df$responses)

###quit###
#variable: q632e4 (B5 Whether have plans to quit)
#values: 1	I plan to stop smoking within the next month
#        2	I plan to stop smoking within the next 3 months
#        3	I plan to stop smoking within the next 6 months
#        4	I have no plans to stop smoking within the next 6 months
#        5	Don't Know
#        6	Not Stated

t<- table(wave191$q632e4)
#plot percentage of non-missing responses (non-NA) of quitting variables
#f21<-table(is.na(wave191$unplan))#Unplanned most recent quit attempt
#f22<-table(is.na(wave191$q632c2))#Whether planned most recent serious quit attempt
f1<-table(is.na(wave191$q632e4)) #count NA of wave 191 data (all 2423 responses are NA (missing values (unfilled)))
f2<-table(is.na(wave191$q632b7))
f3<-table(is.na(wave191$q632b7_1))
f4<-table(is.na(wave191$trylya))
f5<-table(is.na(wave191$trylyb))
f6<-table(is.na(wave191$trylyc))
f7<-table(is.na(wave191$trieslyc))
f8<-table(is.na(wave191$trylm1))
f9<-table(is.na(wave191$trylm2))
f10<-table(is.na(wave191$trylm3))
f11<-table(is.na(wave191$trylma))
f12<-table(is.na(wave191$trylmc))
f13<-table(is.na(wave191$quitstat))#derived variable
f14<-table(is.na(wave191$quit1d))#relapsed within 1 day
f15<-table(is.na(wave191$quit7d))#relapsed within 7 days
f16<-table(is.na(wave191$quit30d))#relapsed within 1 month
f17<-table(is.na(wave191$quit60d))#relapsed within 2 month
f18<-table(is.na(wave191$quit90d))#relapsed within 3 month
f19<-table(is.na(wave191$quit180d))#relapsed within 6 month
f20<-table(is.na(wave191$abrupt))#Abrupt most recent quit attempt
f21<-table(is.na(wave191$fq5d_1))#Q5d_1. Did you try to stop smoking gradually or suddenly?
f22<-table(is.na(wave191$fq5d_2))#Q5d_2. Did you try to stop smoking gradually or suddenly?
f23<-table(is.na(wave191$fq5d_3))#Q5d_3. Did you try to stop smoking gradually or suddenly?
f24<-table(is.na(wave191$fq5e_1))#Q5e_1. Please circle which applies to each quit attempt. The answer “2: I tried to quit as soon as I made the decision” means spontaneous quit attempt
f25<-table(is.na(wave191$fq5e_2))#Q5e_2. Please circle which applies to each quit attempt. The answer “2: I tried to quit as soon as I made the decision” means spontaneous quit attempt
f26<-table(is.na(wave191$fq5e_3))#Q5e_3. Please circle which applies to each quit attempt. The answer “2: I tried to quit as soon as I made the decision” means spontaneous quit attempt
v<-list(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26)
variablesnames <- c("q632e4","q632b7","q632b7_1","trylya","trylyb","trylyc","trieslyc","trylm1","trylm2","trylm3","trylma","trylmc","quitstat","quit1d","quit7d","quit30d","quit60d","quit90d","quit180d","abrupt","fq5d_1","fq5d_2","fq5d_3","fq5e_1","fq5e_2","fq5e_3")
plot_percent_of_non_missing_values(v,variablesnames)

###motivation to quit
f1<-table(is.na(wave191$q632a4))
f2<-table(is.na(wave191$q632e4))
f3<-table(is.na(wave191$q632e8))
f4<-table(is.na(wave191$q632e10))
f5<-table(is.na(wave191$qmotiv)) #derived variable
f6<-table(is.na(wave191$q632o1))
f7<-table(is.na(wave191$q632o2))
f8<-table(is.na(wave191$newwcons))
f9<-table(is.na(wave191$qimw104_1))

#plot percentage of responses of motivation variables
H <- c(f1["FALSE"],f2["FALSE"],f3["FALSE"],f4["FALSE"],f5["FALSE"],f6["FALSE"],f7["FALSE"],f8["FALSE"],f9["FALSE"])
M <- c("q632a4","q632e4","q632e8","q632e10","qmotiv","q632o1","q632o2","newwcons","qimw104_1")

                  
###social demographic variables###
round(mean(wave191$agez),2) #mean=3.85, values of agez: 1	16-24, 2	25-34, 3	35-44, 4	45-54, 5	55-64, 6	65+

round(mean(wave191$sexz,na.rm=T),2)#mean=1.5 (values of sexz: men:1, woman:2)

