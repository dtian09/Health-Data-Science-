library(aod)
library(pROC)
library(caret)
###How to run:
#Rscript logreg.R training_testing "D:\EIS preterm prediction\results\workflow1\my_filtered_data\15april_train60_valid20_test20\trainset0.csv"  "D:\EIS preterm prediction\results\workflow1\my_filtered_data\15april_train60_valid20_test20\trainset0.csv" outfile.txt lr.rda
#Rscript logreg.R testing lr.rda "D:\EIS preterm prediction\results\workflow1\my_filtered_data\15april_train60_valid20_test20\testset0.csv" outfile2.txt
###training and testing performance of logistic regression###
#compute the p-value of coefficients of logistic regression
#The P value indicates whether the independent variable has statistically significant predictive capability. 
#The p value shows the probability of the coefficient being attributed to random variation. The lower the probability, the more significant the impact of the coefficient.
#input: training set
#       test set
#       results file name
#output: a results file containing the following:
#            p values of training set
#            AUC of test set
#            AUC confidence interval of test set
log_reg_train_test <- function (trainset,testset,classvariable){
#return: p-value of the coefficients
#        auc
#        confidence interval of auc
    print('===logistic regression===')
    if (classvariable=='before37weeksCell') {
     lr<-glm(formula=trainset$before37weeksCell ~ ., data=trainset, family = binomial)
    }
    else if (classvariable=='PTB') { 
      lr<-glm(formula=trainset$PTB ~ ., data=trainset, family = binomial)
    }
    else
    {
      stop("classvariable is invalid", call.=FALSE)
    }
    print(summary(lr))
    n<-length(lr$coefficients)
    t<-wald.test(b = coef(lr), Sigma = vcov(lr), Terms = 2:n)
    r<-t$result
    p_value<-r$chi2[3]
    y<-predict(lr,testset,type="response")
    if (classvariable=='before37weeksCell') {
      rocobj<-roc(testset$before37weeksCell,y,ci=TRUE,of="auc")
    }
    else if (classvariable=='PTB') { 
        rocobj<-roc(testset$PTB,y,ci=TRUE,of="auc")
    }
    else
    {
      stop("classvariable is invalid", call.=FALSE)
    }
    ci_test<-rocobj$ci
    auc_test<-rocobj$auc
    return(list(lr,p_value,auc_test,ci_test))
}

log_reg_test <- function (model,testset,classvariable){
#input: model
#       testset
#return: auc of test set
#        confidence interval of auc
    variables<-names(testset)
    l<-length(variables)
    classvariable=variables[l]
    y<-predict(model,testset,type="response")
    y<-as.numeric(as.character(unlist(y)))#convert a list to list of numeric
    if (classvariable=='before37weeksCell') {
     rocobj<-roc(response=testset$before37weeksCell,predictor=y,ci=TRUE,of="auc")
    }
    else if (classvariable=='PTB') { 
     rocobj<-roc(response=testset$PTB,predictor=y,ci=TRUE,of="auc")
    }
    else
    {
      stop("classvariable is invalid", call.=FALSE)
    }
    ci_test<-rocobj$ci
    auc_test<-rocobj$auc
    return(list(auc_test,ci_test))
}

args = commandArgs(trailingOnly=TRUE)

option<-args[1]
if (option=='training_testing'){
    ##train a model, then, test it on testset and save it to file
    if (length(args)<6){
        stop("6 arguments must be supplied", call.=FALSE)
    }
    trainset<-read.csv(args[2], header=T)
    testset<-read.csv(args[3], header=T)
    outfile<-args[4]
    modelfile<-args[5]
    classvariable<-args[6]
    r<-log_reg_train_test(trainset,testset,classvariable)
    model<-r[1]
    saveRDS(model,file=modelfile)
    results<-c(as.character(as.numeric(r[2])),as.character(r[4]))
    #write p-value, auc ci to file
    #file format: p-value
    #             c(lower bound, auc, upper bound)
    #             e.g. 0.0540239231023021
    #                  c(0.819831363641396, 0.88048048048048, 0.941129597319565)
    writeLines(results,con=outfile,sep="\n",useBytes=FALSE)
    print('==test performance==')
    print(r[2])#p-value
    print(r[3])#auc
    print(r[4])#ci
} else if(option=='testing'){
          ##load model from file and test model on testset
          if (length(args)<5){
            stop("5 arguments must be supplied", call.=FALSE)
          }
          modelfile<-args[2]   
          testset<-read.csv(args[3], header=T)
          outfile<-args[4]
          classvariable<-args[5]
          model<-readRDS(modelfile)
          r<-log_reg_test(model,testset,classvariable)
          #results<-c(as.character(as.numeric(r[1])),as.character(r[2]))
          #writeLines(results,con=outfile,sep="\n",useBytes=FALSE)
          #write AUC confidence interval to file
          #file format: c(lower bound, auc, upper bound)
          #        e.g. c(0.455287598319069, 0.531794871794872, 0.608302145270675) 
          writeLines(as.character(r[2]),con=outfile,sep="\n",useBytes=FALSE)         
          print('==test performance==')
          print(r[1])
          print(r[2])
       } else {
               stop("invalid option supplied", call.=FALSE)
               }
