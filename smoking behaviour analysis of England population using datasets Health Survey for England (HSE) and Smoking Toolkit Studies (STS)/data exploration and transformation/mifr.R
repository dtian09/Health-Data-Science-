#Mutual information feature ranking
#library(mlbench)
library(varrank)
#data(PimaIndiansDiabetes)
  
##forward search for all variables
df2009$nsec<-as.factor(df2009$nsec)

out1 <- varrank(data.df = df2009,
                  method = "battiti",
                  variable.important = "nsec",
                  #discretization.method = "sturges",
                  discretization.method = "kmeans",
                  algorithm = "forward", 
                  scheme = "mid", 
                  ratio=0) #set ratio=0 to calculate mutual information

#information gain
library(FSelector)

df2009_2$nsec<-as.factor(df2009_2$nsec)
weights<-information.gain(nsec~.,df2009_2)
print(weights)
