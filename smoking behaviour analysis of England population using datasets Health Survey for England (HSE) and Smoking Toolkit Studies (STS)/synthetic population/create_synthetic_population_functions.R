#functions used by create_test_data.R

select_random_values_of_variable <-function(df,df2,vars) {
  #select n random values of variables (vars) of df2 where n is number of rows of df
  vals<-unique(df2[[vars[1]]])
  vals2<-na.omit(vals)
  if(is_empty(vals2)==F) {
    return(base::sample(vals2,replace=T,size=nrow(df)))
  }
  else
  { #print('empty vector')
    na_vector <- rep(NA, nrow(df))
    return(na_vector)
  }
}

change_some_exsmokers_to_newquitters_and_ongoingquitters <- function(df) {
  #randomly change some ex-smokers to newquitters and ongoingquitters 
  exsmokers <- df %>% filter(bState=='ex-smoker')

  nonexsmokers <- df %>% filter(bState!='ex-smoker') %>%
                          mutate(bState = factor(bState, 
                                 levels=c("smoker",
                                    "never_smoker",
                                    "newquitter",
                                    "ongoingquitter",
                                    "ex-smoker")))
  cprob<-c(1:11) #cumulative prob of ongiong quitters1,..,11
  cprob[1]<-1/20 #prob of ongoing quitter1
  prob_ongoquitter=1/40
  for(i in 2:length(cprob)){
    cprob[i]<-cprob[i-1]+prob_ongoquitter
  }
  prob_newquitter=1/10
  cprob2=cprob[11]+prob_newquitter #cumulative prob of new quitters
  prob<-runif(nrow(exsmokers))
  #define values "newquitter", "ongoingquitter", for the state variable
  exsmokers<-exsmokers %>% 
    mutate(bState = factor(bState, 
                          levels=c("smoker",
                                   "never_smoker",
                                   "newquitter",
                                   "ongoingquitter",
                                   "ex-smoker")))

  for (i in 1:nrow(exsmokers)){
    exsmokers$bState[i] <- ifelse(prob[i] <= cprob[11], "ongoingquitter",
                            ifelse(prob[i] > cprob[11] & prob[i] <= cprob2, "newquitter", "ex-smoker"))
    
    exsmokers$bMonthsSinceQuit[i] <- if(exsmokers$bState[i]=="ongoingquitter"){ 
                                          sample(1:11, 1)
                                        #ifelse(prob[i] <= cprob[1], 1,
                                        #  ifelse(prob[i] > cprob[1] & prob[i] <= cprob[2], 2,
                                         #    ifelse(prob[i]>cprob[2] & prob[i]<=cprob[3], 3,
                                         #       ifelse(prob[i]>cprob[3] & prob[i]<=cprob[4], 4, 
                                         #             ifelse(prob[i]>cprob[4] & prob[i]<=cprob[5], 5,
                                         #                    ifelse(prob[i]>cprob[5] & prob[i]<=cprob[6], 6,
                                        #                            ifelse(prob[i]>cprob[6] & prob[i]<=cprob[7], 7,
                                          #                                 ifelse(prob[i]>cprob[7] & prob[i]<=cprob[8], 8,
                                          #                                        ifelse(prob[i]>cprob[8] & prob[i]<=cprob[9], 9,
                                          #                                               ifelse(prob[i]>cprob[9] & prob[i]<=cprob[10], 10,
                                         #                                                       ifelse(prob[i]>cprob[10] & prob[i]<=cprob[11], 11, 11)))))))))))
                                      }else{exsmokers$bMonthsSinceQuit[i]}
  }
  
  exsmokers<-exsmokers %>% mutate(cprob=NULL,cprob2=NULL,prob_ongoquitter=NULL)#delete columns cprob, cprob2 and prob_ongoquitter
  data<-rbind(exsmokers,nonexsmokers)
  return(data)
}

convert_to_integer_type <-function(df,vars) {
  #convert vars of factor type to numeric type so that the numeric values won't have double quotes when written to a csv file
  n<-length(vars)
  for(i in 1:n){
    print(vars[i])
    if (all(is.na(df[[vars[i]]]))==F){#not all values are NA, convert values to integer type
      df[[vars[i]]]<-as.numeric(as.character(df[[vars[i]]]))  
    }
  }
  return(df)
}
