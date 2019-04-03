setwd("//filesrv1/CMS_Caregiver/DATA/Poster B/Stata Data For Analysis")
getwd()

install.packages("e1071")
#import the dta import library
library(e1071)
library(foreign)

#read in my sas file on preventable hospitlization

hcbs <- read.dta("************************************")

#Now I have my data set in the R environment need to ru SVM classification 

svm_vars <- c("hcbs_rate", "age", "female" , "black" , "hispanic", "other_race" , "white",
              "assist_home", "assist_al", "pac", "nutrition", "risk", "pain1" , "pain2", 
              "adl_sum", "ulcer2_up", "surg_wd_lesion", "lesion", "dyspenic", "respritory",
              "uti", "u_incntn", "bwl_incntn", "cog_fun_high", "depression_mid", "depression_high",
              "acsc_hha_60")
#creating my subset with the varibles I require for analysis
svm_hcbs <- hcbs[svm_vars]

#descriptive statistics
sapply(svm_hcbs,mean, na.rm=TRUE) 

#create test and traiing set 
library(caTools)
set.seed(123)
split = sample.split(svm_hcbs, SplitRatio = 0.75)
svm_hcbs_train = subset(svm_hcbs, split == TRUE)
svm_hcbs_test = subset(svm_hcbs, split == FALSE)


#creating my regressor
svmfit = svm(formula =acsc_hha_60~.,
                data = svm_hcbs_train,
                type = "C-classification",
                kernel = "linear")

svm_pred <- predict(svmfit, svm_predict = svm_hcbs_test)

print(svmfit)

plot(svmfit,svm_hcbs_train, hcbs_rate~adl_sum )



