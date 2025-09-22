library(pROC)
#Rscript ci.R "H:\data\EIS preterm prediction\results\workflow1\9oct19_59iter_filtered_data\targets.csv" "H:\data\EIS preterm prediction\results\workflow1\9oct19_59iter_filtered_data\pred.csv" "H:\data\EIS preterm prediction\results\workflow1\9oct19_59iter_filtered_data\outfile.txt"
#load true labels and predicted probabilities of testset
args = commandArgs(trailingOnly=TRUE)
targets<-read.csv(args[1], header=T)
pred<-read.csv(args[2], header=T)
outfile<-args[3]
rocobj<-roc(targets$before37weeksCell,pred$before37weeksCell,ci=TRUE,of="auc")
ci<-rocobj$ci
auc<-rocobj$auc
print(ci)
print(auc)
result<-list(ci)
writeLines(as.character(result[1]),con=outfile,useBytes=FALSE)

