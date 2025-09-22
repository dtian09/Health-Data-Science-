###scripts to test significance of association in the association rules output by Apriori algorithm (weka) and calculate strength of association using Cramer's V test
###association rules files of Apriori: 
#   associate_rules_of_Lactate_equal_width_discretized_3_intervals.txt
#   associate_rules_of_SUCCINATE_equal_width_discretized_3_intervals.txt

library(MASS)
###examples of chisquared test, fisher test and Cramer's V (strength of association) on web
#https://www.datacamp.com/community/tutorials/contingency-tables-r
#http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence
#https://rdrr.io/cran/greybox/man/cramer.html

data<-read.csv("D:\\EIS preterm prediction\\summer projects\\datasets\\asymp_22wks_filtered_data_28inputs_equal_width_discretized.csv", header=T)
#create a contingency table of 2 variables
#classification association rule of Apriori (weka) : 27_EIS_Amplitude2='(-inf-78.483933]' 288 ==> LACTATE='(-inf-26662145.666667]' 273    conf:(0.95)

tbl<-table(data$LACTATE,data$Amplitude2)
chisq.test(tbl)

#without using a contingency table
chisq.test(data$LACTATE,data$Amplitude2)

fisher.test(data$LACTATE,data$Amplitude2)

#test significance of association: 27_EIS_Amplitude1='(-inf-99.386467]' 27_EIS_Amplitude2='(-inf-78.483933]' 288 ==> LACTATE='(-inf-26662145.666667]' 273    conf:(0.95) 
chisq.test(data$LACTATE,data$Amplitude1)
chisq.test(data$LACTATE,data$Amplitude2)
chisq.test(data$Amplitude1,data$Amplitude2)

tbl2<-table(data$LACTATE,data$Amplitude3)
chisq.test(tbl2)

chisq.test(data$LACTATE,data$Amplitude3)

fisher.test(data$LACTATE,data$Amplitude3)

chisq.test(data$SUCCINATE,data$Phase1)

#calculate strength of association based on Cramer?s V test
######outputs from R console######
#
#> cramer(data$LACTATE,data$Amplitude2)
#Cramer's V: 0.2505
#Chi^2 statistics = 37.2742, df: 4, p-value: 0
#> 
#> cramer(data$LACTATE,data$Amplitude1)
#Cramer's V: 0.0725
#Chi^2 statistics = 3.124, df: 4, p-value: 0.5373
#> cramer(data$LACTATE,data$Amplitude3)
#Cramer's V: 0.2505
#Chi^2 statistics = 37.2742, df: 4, p-value: 0
#> cramer(data$LACTATE,data$Amplitude4)
#Cramer's V: 0.0608
#Chi^2 statistics = 2.1987, df: 4, p-value: 0.6993
#> cramer(data$LACTATE,data$Amplitude5)
#Cramer's V: 0.0448
#Chi^2 statistics = 1.1902, df: 4, p-value: 0.8797
#> cramer(data$LACTATE,data$Amplitude6)
#Cramer's V: 0.1155
#Chi^2 statistics = 7.9193, df: 4, p-value: 0.0946
#> cramer(data$LACTATE,data$Amplitude7)
#Cramer's V: 0.0143
#Chi^2 statistics = 0.0609, df: 2, p-value: 0.97
#> cramer(data$LACTATE,data$Amplitude8)
#Cramer's V: 0.0143
#Chi^2 statistics = 0.0609, df: 2, p-value: 0.97
#> cramer(data$SUCCINATE,data$Phase1)
#Cramer's V: 0.4992
#Chi^2 statistics = 148.0459, df: 6, p-value: 0
cramer(data$SUCCINATE,data$Amplitude7)
#Cramer's V: 0.0108
#Chi^2 statistics = 0.035, df: 3, p-value: 0.9983
cramer(data$SUCCINATE,data$Amplitude8)
#Cramer's V: 0.0108
#Chi^2 statistics = 0.035, df: 3, p-value: 0.9983
cramer(data$SUCCINATE,data$Amplitude9)
#Cramer's V: 0.0108
#Chi^2 statistics = 0.035, df: 3, p-value: 0.9983

library(greybox)

cramer(data$LACTATE,data$Amplitude1)
cramer(data$LACTATE,data$Amplitude2)
cramer(data$LACTATE,data$Amplitude3)
cramer(data$LACTATE,data$Amplitude4)
cramer(data$LACTATE,data$Amplitude5)

cramer(data$SUCCINATE,data$Phase1)
cramer(data$SUCCINATE,data$Amplitude7)

#Test Association of Acetate and EIS features
#Acetate has a significant but weak association (p-value < 0.05 and Cramer V = 0.15) with Phase12 only and no significant associations with all the other EIS features
#>cramer(data$ACETATE,data$Phase12)
#Cramer's V: 0.1451
#Chi^2 statistics = 12.5131, df: 4, p-value: 0.0139
#
#> GTest(data$ACETATE,data$Phase12)
#
#        Log likelihood ratio (G-test) test of independence without correction
#
#data:  data$ACETATE and data$Phase12
#G = 9.178, X-squared df = 4, p-value = 0.0568

data<-read.csv("D:\\EIS preterm prediction\\summer projects\\datasets\\asymp_22wks_filtered_data_28inputs_equal_width_discretized.csv", header=T)

data2<-read.csv("D:\\EIS preterm prediction\\summer projects\\datasets\\asymp_22wks_filtered_data_28inputs_equal_width_discretized_boolean_features.csv", header=T)

data3<-read.csv("D:\\EIS preterm prediction\\summer projects\\datasets\\asymp_22wks_filtered_data_28inputs_with_no_outliers_equal_width_discretized.csv", header=T)

names(data3)

#remove missing values from dataframe
library(dplyr)

data<-filter(data,SUCCINATE != '?')
data<-filter(data,SUCCINATE != '?' & Phase12 != '?')

data4 <- filter(data3, GLUCOSE != '?' & Phase11 != '?')

#increase workspace to 2e8 and perform fisher test
fisher.test(data4$GLUCOSE,data4$Phase11,workspace=2e8)

library(DescTools)

GTest(data3$SUCCINATE,data3$Amplitude5)


