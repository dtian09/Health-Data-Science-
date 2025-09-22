library(aod)
library(caret)
library(pROC)
###cross validation with logistic regression

log_reg <- function (trainset,testset){
#return: p-value of the coefficients
#        auc
#        confidence interval of auc
    print('===logistic regression===')
    lr<-glm(formula=trainset$before37weeksCell ~ ., data=trainset, family = binomial)
    print(summary(lr))
    n<-length(lr$coefficients)
    t<-wald.test(b = coef(lr), Sigma = vcov(lr), Terms = 2:n)
    r<-t$result
    p_value<-r$chi2[3]
    y<-predict(lr,trainset,type="response")
    rocobj<-roc(trainset$before37weeksCell,y,ci=TRUE,of="auc")
    ci_train<-rocobj$ci
    auc_train<-rocobj$auc
    y<-predict(lr,testset,type="response")
    rocobj<-roc(testset$before37weeksCell,y,ci=TRUE,of="auc")
    ci_test<-rocobj$ci
    auc_test<-rocobj$auc
    return(list(lr,p_value,auc_train,ci_train,auc_test,ci_test))
}

cv_3fold <- function (dataset) {
#3-fold cross validation
#input: a dataset
set.seed(345)
#set.seed(0)
cv <- createFolds(dataset$before37weeksCell, k = 3, returnTrain = TRUE)
total_train_auc<-0
total_test_auc<-0
min_train_auc<-1
max_train_auc<-0
min_test_auc<-1
max_test_auc<-0
print('===fold 1===')
trainindx1 <- cv$Fold1
trainset <- dataset[trainindx1,]
testset <- dataset[-trainindx1,]
r1<-log_reg(trainset,testset)
pval<-as.numeric(r1[2])
train_auc<-as.numeric(r1[3])
test_auc<-as.numeric(r1[5])
if (train_auc < min_train_auc){
   min_train_auc<-train_auc
}
if (train_auc > max_train_auc){
   max_train_auc <- train_auc
}
if (test_auc < min_test_auc){
   min_test_auc<-test_auc
}
if (test_auc > max_test_auc){
   max_test_auc <- test_auc
}
total_train_auc<-total_train_auc+train_auc
total_test_auc<-total_test_auc+test_auc
print('===Fold2===')
trainindx2 <- cv$Fold2
trainset <- dataset[trainindx2,]
testset <- dataset[-trainindx2,]
r2<-log_reg(trainset,testset)
pval<-as.numeric(r2[2])
train_auc<-as.numeric(r2[3])
test_auc<-as.numeric(r2[5])
if (train_auc < min_train_auc){
   min_train_auc<-train_auc
}
if (train_auc > max_train_auc){
   max_train_auc <- train_auc
}
if (test_auc < min_test_auc){
   min_test_auc<-test_auc
}
if (test_auc > max_test_auc){
   max_test_auc <- test_auc
}
total_train_auc<-total_train_auc+train_auc
total_test_auc<-total_test_auc+test_auc
print('===Fold3===')
set.seed(123456)
cv <- createFolds(dataset$before37weeksCell, k = 3, returnTrain = TRUE)
trainindx3 <- cv$Fold3
trainset <- dataset[trainindx3,]
testset <- dataset[-trainindx3,]
r3<-log_reg(trainset,testset)
pval<-as.numeric(r3[2])
train_auc<-as.numeric(r3[3])
test_auc<-as.numeric(r3[5])
if (train_auc < min_train_auc){
   min_train_auc<-train_auc
}
if (train_auc > max_train_auc){
   max_train_auc <- train_auc
}
if (test_auc < min_test_auc){
   min_test_auc<-test_auc
}
if (test_auc > max_test_auc){
   max_test_auc <- test_auc
}
total_train_auc<-total_train_auc+train_auc
total_test_auc<-total_test_auc+test_auc
mean_train_auc<-total_train_auc/3
mean_test_auc<-total_test_auc/3
print(paste('mean training auc',mean_train_auc,sep='='))
print(paste('max training auc',max_train_auc,sep='='))
print(paste('min training auc',min_train_auc,sep='='))
print(paste('mean testing auc',mean_test_auc,sep='='))
print(paste('max testing auc',max_test_auc,sep='='))
print(paste('min testing auc',min_test_auc,sep='='))
print('==Fold1: training performance==')
print(r1[2]) #p-value
print(r1[3]) #training auc 
print(r1[4]) #auc ci
print('==Fold1: test performance==')
print(r1[5]) #test auc
print(r1[6]) #auc ci
print('==Fold2: training performance==')
print(r2[2]) #p-value
print(r2[3]) #training auc 
print(r2[4]) #auc ci
print('==Fold2: test performance==')
print(r2[5]) #test auc
print(r2[6]) #auc ci
print('==Fold3: training performance==')
print(r3[2]) #p-value
print(r3[3]) #training auc 
print(r3[4]) #auc ci
print('==Fold3: test performance==')
print(r3[5]) #test auc
print(r3[6]) #auc ci
}

cv_5fold <- function (dataset) {
#5-fold cross validation
#input: a dataset

set.seed(345)
#set.seed(0)
cv <- createFolds(dataset$before37weeksCell, k = 5, returnTrain = TRUE)
total_train_auc<-0
total_test_auc<-0
min_train_auc<-1
max_train_auc<-0
min_test_auc<-1
max_test_auc<-0
print('===fold 1===')
trainindx1 <- cv$Fold1
trainset <- dataset[trainindx1,]
testset <- dataset[-trainindx1,]
r1<-log_reg(trainset,testset)
pval<-as.numeric(r1[2])
train_auc<-as.numeric(r1[3])
test_auc<-as.numeric(r1[5])
if (train_auc < min_train_auc){
   min_train_auc<-train_auc
}
if (train_auc > max_train_auc){
   max_train_auc <- train_auc
}
if (test_auc < min_test_auc){
   min_test_auc<-test_auc
}
if (test_auc > max_test_auc){
   max_test_auc <- test_auc
}
total_train_auc<-total_train_auc+train_auc
total_test_auc<-total_test_auc+test_auc
print('===Fold2===')
trainindx2 <- cv$Fold2
trainset <- dataset[trainindx2,]
testset <- dataset[-trainindx2,]
r2<-log_reg(trainset,testset)
pval<-as.numeric(r2[2])
train_auc<-as.numeric(r2[3])
test_auc<-as.numeric(r2[5])
if (train_auc < min_train_auc){
   min_train_auc<-train_auc
}
if (train_auc > max_train_auc){
   max_train_auc <- train_auc
}
if (test_auc < min_test_auc){
   min_test_auc<-test_auc
}
if (test_auc > max_test_auc){
   max_test_auc <- test_auc
}
total_train_auc<-total_train_auc+train_auc
total_test_auc<-total_test_auc+test_auc
print('===Fold3===')
set.seed(123456)
cv <- createFolds(dataset$before37weeksCell, k = 5, returnTrain = TRUE)
trainindx3 <- cv$Fold3
trainset <- dataset[trainindx3,]
testset <- dataset[-trainindx3,]
r3<-log_reg(trainset,testset)
pval<-as.numeric(r3[2])
train_auc<-as.numeric(r3[3])
test_auc<-as.numeric(r3[5])
if (train_auc < min_train_auc){
   min_train_auc<-train_auc
}
if (train_auc > max_train_auc){
   max_train_auc <- train_auc
}
if (test_auc < min_test_auc){
   min_test_auc<-test_auc
}
if (test_auc > max_test_auc){
   max_test_auc <- test_auc
}
total_train_auc<-total_train_auc+train_auc
total_test_auc<-total_test_auc+test_auc
print('===Fold4===')
set.seed(6789)
cv <- createFolds(dataset$before37weeksCell, k = 5, returnTrain = TRUE)
trainindx4 <- cv$Fold4
trainset <- dataset[trainindx4,]
testset <- dataset[-trainindx4,]
r4<-log_reg(trainset,testset)
pval<-as.numeric(r4[2])
train_auc<-as.numeric(r4[3])
test_auc<-as.numeric(r4[5])
if (train_auc < min_train_auc){
   min_train_auc<-train_auc
}
if (train_auc > max_train_auc){
   max_train_auc <- train_auc
}
if (test_auc < min_test_auc){
   min_test_auc<-test_auc
}
if (test_auc > max_test_auc){
   max_test_auc <- test_auc
}
total_train_auc<-total_train_auc+train_auc
total_test_auc<-total_test_auc+test_auc
print('===Fold5===')
set.seed(6789)
cv <- createFolds(dataset$before37weeksCell, k = 5, returnTrain = TRUE)
trainindx5 <- cv$Fold5
trainset <- dataset[trainindx5,]
testset <- dataset[-trainindx5,]
r5<-log_reg(trainset,testset)
pval<-as.numeric(r5[2])
train_auc<-as.numeric(r5[3])
test_auc<-as.numeric(r5[5])
if (train_auc < min_train_auc){
   min_train_auc<-train_auc
}
if (train_auc > max_train_auc){
   max_train_auc <- train_auc
}
if (test_auc < min_test_auc){
   min_test_auc<-test_auc
}
if (test_auc > max_test_auc){
   max_test_auc <- test_auc
}
total_train_auc<-total_train_auc+train_auc
total_test_auc<-total_test_auc+test_auc
mean_train_auc<-total_train_auc/5
mean_test_auc<-total_test_auc/5
print(paste('mean training auc',mean_train_auc,sep='='))
print(paste('max training auc',max_train_auc,sep='='))
print(paste('min training auc',min_train_auc,sep='='))
print(paste('mean testing auc',mean_test_auc,sep='='))
print(paste('max testing auc',max_test_auc,sep='='))
print(paste('min testing auc',min_test_auc,sep='='))
print('==Fold1: training performance==')
print(r1[2]) #p-value
print(r1[3]) #training auc 
print(r1[4]) #auc ci
print('==Fold1: test performance==')
print(r1[5]) #test auc
print(r1[6]) #auc ci
print('==Fold2: training performance==')
print(r2[2]) #p-value
print(r2[3]) #training auc 
print(r2[4]) #auc ci
print('==Fold2: test performance==')
print(r2[5]) #test auc
print(r2[6]) #auc ci
print('==Fold3: training performance==')
print(r3[2]) #p-value
print(r3[3]) #training auc 
print(r3[4]) #auc ci
print('==Fold3: test performance==')
print(r3[5]) #test auc
print(r3[6]) #auc ci
print('==Fold4: training performance==')
print(r4[2]) #p-value
print(r4[3]) #training auc 
print(r4[4]) #auc ci
print('==Fold4: test performance==')
print(r4[5]) #test auc
print(r4[6]) #auc ci
print('==Fold5: training performance==')
print(r5[2]) #p-value
print(r5[3]) #training auc 
print(r5[4]) #auc ci
print('==Fold5: test performance==')
print(r5[5]) #test auc
print(r5[6]) #auc ci
}
###top level code
dataset<-read.csv("438_V1_previous_history_and_demographics2.csv",header=T)
#dataset<-subset(dataset, select=c("number_previous_early_miscarriages","before37weeksCell"))
dataset<-subset(dataset, select=c("no_preterm_brithsCell","number_previous_early_miscarriages","before37weeksCell"))
#dataset<-read.csv("D:\\EIS preterm prediction\\EIS_Data\\EIS_Data\\438_V1_7inputs.csv", header=T)
#dataset<-subset(dataset, select=c("no_preterm_birthsCell", "before37weeksCell"))
#dataset<-subset(dataset, select=c("no_term_birthsCell","before37weeksCell"))
#dataset<-subset(dataset, select=c("no_term_birthsCell","no_MTLCell","cervical_cerclageCell","progesteroneCell","tocolysisCell","visits_steroidsCell","before37weeksCell"))
#dataset<-subset(dataset, select=c("no_preterm_birthsCell","no_term_birthsCell","no_MTLCell","cervical_cerclageCell","tocolysisCell","visits_steroidsCell","before37weeksCell"))
#dataset<-subset(dataset, select=c("no_preterm_birthsCell","no_term_birthsCell","cervical_cerclageCell","progesteroneCell","tocolysisCell","visits_steroidsCell","before37weeksCell"))
cv_3fold(dataset)
#cv_5fold(dataset)