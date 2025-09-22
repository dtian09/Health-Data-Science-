library(rfPermute)
library(randomForest)
library(foreach)
library(pROC)
library(aod)
library(caret)
###How to run:
#Rscript rf.R training_testing 123456 10 20 "D:\EIS preterm prediction\results\workflow1\my_filtered_data\15april_train60_valid20_test20\trainset0.csv"  "D:\EIS preterm prediction\results\workflow1\my_filtered_data\15april_train60_valid20_test20\trainset0.csv" outfile.txt rf.rda
#Rscript rf.R testing rf.rda "D:\EIS preterm prediction\results\workflow1\my_filtered_data\15april_train60_valid20_test20\testset0.csv" outfile2.txt
###training and testing performance of random forest###
#compute the p-value of mean descrease in Gini index of the features of a random forest
#The P value indicates whether the independent variable has statistically significant predictive capability. 
#input: training set
#       test set
#       results file name
#output: a results file containing the following:
#            p values of training set
#            AUC of test set
#            AUC confidence interval of test set
random_forest_train_test <- function (s,features,trees,trainset,testset,classvariable){
    #return: p-value of the coefficients
    #        auc
    #        confidence interval of auc
    #compute the p-values of the mean descrease in Gini index of the features of a random forest
    #format of p-values output by rfPermute:
    #                  Good        Poor MeanDecreaseAccuracy
    #aSAH$gos6  0.004975124 0.004975124          0.004975124
    #aSAH$s100b 0.109452736 0.363184080          0.119402985
    #           MeanDecreaseGini
    #aSAH$gos6       0.004975124
    #aSAH$s100b      1.000000000
    print('===random forest===')
    s<-as.numeric(s)
    features<-as.numeric(features)
    print(paste('features',features,sep=':'))
    trees<-as.numeric(trees)
    set.seed(s)
    if (classvariable=='before37weeksCell') {  
        rf.model<-randomForest(formula=factor(trainset$before37weeksCell) ~ ., data=trainset, mtry=features, ntree=trees)
        rfobj<-rfPermute(formula=factor(trainset$before37weeksCell) ~ ., data=trainset, mtry=features, ntree=trees,nrep=200)
    }
    else if(classvariable=='PTB'){
        rf.model<-randomForest(formula=factor(trainset$PTB) ~ ., data=trainset, mtry=features, ntree=trees)
        rfobj<-rfPermute(formula=factor(trainset$PTB) ~ ., data=trainset, mtry=features, ntree=trees,nrep=200)
    }
    else
    {
      stop("classvariable is invalid", call.=FALSE)
    }
    pvals2<-rfobj$pval
    l<-length(pvals2)
    #p values of the m variables of a random forest are at indices l-m,l-(m-1),...,l
    cols<-dim(trainset)
    m<-cols[2]-1 #no. of features
    q<-m
    pval_min <- 99
    pval_max <- 0
    pval_mean <- 0
    ### foreach (k=1:m) %do% {
    for (k in 1:m) {
        pval<-pvals2[l-q]
        #print(pval)
        if (pval < pval_min){
          pval_min <- pval
         }
        if (pval > pval_max){
          pval_max <- pval
        }
        pval_mean <- pval_mean + pval
        q <- m-k
    }
    pval_mean <- pval_mean/m
    print(pval_min)
    print(pval_max)
    print(pval_mean)
    y<-predict(rf.model,testset,type="response")
    y<-as.numeric(as.character(unlist(y)))#convert a list to list of numeric
    if (classvariable=='before37weeksCell') {
        rocobj<-roc(testset$before37weeksCell,y,ci=TRUE,of="auc")
    }
    else if (classvariable=='PTB'){
        rocobj<-roc(testset$PTB,y,ci=TRUE,of="auc")
    }
    ci_test<-rocobj$ci
    auc_test<-rocobj$auc    
    return(list(rf.model,paste(pval_mean,pval_min,pval_max,sep=','),auc_test,ci_test))
    }

random_forest_test <- function (model,testset,classvariable){
#input: model
#       testset
#return: auc of test set
#        confidence interval of auc
    y<-predict(model,testset,type="response")
    y<-as.numeric(as.character(unlist(y)))#convert a list to list of numeric
    if (classvariable=='before37weeksCell') {  
        rocobj<-roc(response=testset$before37weeksCell,predictor=y,ci=TRUE,of="auc")
    }
    else if (classvariable=='PTB'){
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
    if (length(args)<9){
        stop("9 arguments must be supplied", call.=FALSE)
    }
    seed<-args[2]
    features<-args[3]
    trees<-args[4]
    trainset<-read.csv(args[5], header=T)
    testset<-read.csv(args[6], header=T)
    outfile<-args[7]
    modelfile<-args[8]
    classvariable<-args[9]
    r<-random_forest_train_test(seed,features,trees,trainset,testset,classvariable)
    model<-r[1]
    saveRDS(model,file=modelfile)
    results<-c(as.character(r[2]),as.character(r[4]))
    #file format: mean p-value, min p-value, max p-value
    #             c(lower bound, auc, upper bound)
    #             e.g. 0.56680881307747,0.00497512437810945,0.985074626865672
    #                  c(0.936040010826326, 0.972972972972973, 1)
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
          r<-random_forest_test(model,testset,classvariable)
          #write AUC confidence interval to file
          #format: c(lower bound, auc, upper bound)
          #        e.g. c(0.455287598319069, 0.531794871794872, 0.608302145270675) 
          writeLines(as.character(r[2]),con=outfile,sep="\n",useBytes=FALSE)
          print('==test performance==')
          print(r[1])
          print(r[2])
       } else {
               stop("invalid option supplied", call.=FALSE)
               }