# Attaching packages
xfun::pkg_attach(c('haven','foreign','boot','caret','dplyr','readr','readxl','ggplot2','HH','labelled','pwr','psych','MASS','officer','pls','Hmisc','gtsummary','tidyverse'))
xfun::pkg_attach(c('gplots'))

#Random number generator(RNG)reproducing copy or images of
set.seed(1000)
# importing data set
# setting up a working directory
getwd()
setwd("C:/Users/User/OneDrive/Documents/HKL AMREC")
library(readxl)
low_birth_weight_final_2 <- read.csv("low birth weight_final_2.csv")
View(low_birth_weight_final_2)


#generating new variable for low(indicator of birth weight less than 2.5kg )
table(low_birth_weight_final_2$low)
low_birth_weight_final_2$low_=NA
low_birth_weight_final_2$low_[low_birth_weight_final_2$low==1]=1
low_birth_weight_final_2$low_[low_birth_weight_final_2$low==0]=0
#labeling low_
low_birth_weight_final_2$low_=factor(low_birth_weight_final_2$low_,levels = c(1,0),labels = c("Yes","No"))
table(low_birth_weight_final_2$low_)
View(low_birth_weight_final_2)

#generating new variable for age(mother's age in years )
table(low_birth_weight_final_2$age)
low_birth_weight_final_2$agecut=NA
low_birth_weight_final_2$agecut[low_birth_weight_final_2$age<=25]=1
low_birth_weight_final_2$agecut[low_birth_weight_final_2$age>25]=0
#labeling agecut
low_birth_weight_final_2$agecut=factor(low_birth_weight_final_2$agecut,levels = c(1,0),labels = c("below 25yrs","above 25yrs"))
table(low_birth_weight_final_2$agecut)

#generating new variable for lwt(mother's weight in pounds at last menstrual period )
table(low_birth_weight_final_2$lwt)
low_birth_weight_final_2$lwt_=NA
low_birth_weight_final_2$lwt_[low_birth_weight_final_2$lwt<=160]=1
low_birth_weight_final_2$lwt_[low_birth_weight_final_2$lwt>160]=0
#labeling lwt_
low_birth_weight_final_2$lwt_=factor(low_birth_weight_final_2$lwt_,levels = c(1,0),labels = c("below 160 pounds","above 160 pounds"))
table(low_birth_weight_final_2$lwt_)


#generating new variable for smoke
table(low_birth_weight_final_2$smoke)
low_birth_weight_final_2$smokecut=NA
low_birth_weight_final_2$smokecut[low_birth_weight_final_2$smoke==1]=1
low_birth_weight_final_2$smokecut[low_birth_weight_final_2$smoke==0]=0
#labeling smokecut
low_birth_weight_final_2$smokecut=factor(low_birth_weight_final_2$smokecut,levels = c(1,0),labels = c("smoke","don't smoke"))
table(low_birth_weight_final_2$smokecut)

#generate new variable for ht
table(low_birth_weight_final_2$ht)
low_birth_weight_final_2$ht_=NA
low_birth_weight_final_2$ht_[low_birth_weight_final_2$ht==1]=1
low_birth_weight_final_2$ht_[low_birth_weight_final_2$ht==0]=0
#labeling ht_
low_birth_weight_final_2$ht_=factor(low_birth_weight_final_2$ht_,levels = c(1,0),labels = c("Yes","No"))
table(low_birth_weight_final_2$ht_)

#generate new variiable for ui
table(low_birth_weight_final_2$ui)
low_birth_weight_final_2$ui_=NA
low_birth_weight_final_2$ui_[low_birth_weight_final_2$ui==1]=1
low_birth_weight_final_2$ui_[low_birth_weight_final_2$ui==0]=0
#labeling ui_
low_birth_weight_final_2$ui_=factor(low_birth_weight_final_2$ui_,levels = c(1,0),labels =c("Yes","No"))
table(low_birth_weight_final_2$ui_)


#generate new variiable for ftv
table(low_birth_weight_final_2$ftv)
low_birth_weight_final_2$ftv_=NA
low_birth_weight_final_2$ftv_[low_birth_weight_final_2$ftv==1]=1
low_birth_weight_final_2$ftv_[low_birth_weight_final_2$ftv==0]=0
#labeling ftv_
low_birth_weight_final_2$ftv_=factor(low_birth_weight_final_2$ftv_,levels = c(1,0),labels =c("visits during the trimester","no visits during the trimester"))
table(low_birth_weight_final_2$ftv_)

#generate new variable for ptl
table(low_birth_weight_final_2$ptl)
low_birth_weight_final_2$ptl_=NA
low_birth_weight_final_2$ptl_[low_birth_weight_final_2$ptl==1]=1
low_birth_weight_final_2$ptl_[low_birth_weight_final_2$ptl==0]=0
#labeling ptl_
low_birth_weight_final_2$ptl_=factor(low_birth_weight_final_2$ptl_,levels = c(1,0),labels =c("previous premature labours","no previous premature labours"))
table(low_birth_weight_final_2$ptl_)

#generate new variable for bwt
table(low_birth_weight_final_2$bwt)
low_birth_weight_final_2$bwt_=NA
low_birth_weight_final_2$bwt_[low_birth_weight_final_2$bwt<=2500.00]=1
low_birth_weight_final_2$bwt_[low_birth_weight_final_2$bwt>2500.00]=0
#labeling bwt_
low_birth_weight_final_2$bwt_=factor(low_birth_weight_final_2$bwt_,levels = c(1,0),labels = c("below 2500 grams","above 2500 grams"))
table(low_birth_weight_final_2$bwt_)

#Data visualization
hist(low_birth_weight_final_2$age)
hist(low_birth_weight_final_2$lwt)
barchart(low_birth_weight_final_2$race)

#Descriptive statistics
summary(low_birth_weight_final_2$age)
summary(low_birth_weight_final_2$lwt)
summary(low_birth_weight_final_2$bwt_)

#variance and std(standard deviation)
with(low_birth_weight_final_2,var(age))
with(low_birth_weight_final_2,sd(age))
with(low_birth_weight_final_2,var(lwt))
with(low_birth_weight_final_2,sd(lwt))

##labeling new variables
#require functions same as library
require(labelled)

#we are labeling to make the appearance look better
var_label(low_birth_weight_final_2$low_) <- "birth weight less than 2.5 kg"
var_label(low_birth_weight_final_2$agecut) <- "mother's age in years"
var_label(low_birth_weight_final_2$lwt_) <- "mother's weight in pounds"
var_label(low_birth_weight_final_2$smokecut) <- "smoke"
var_label(low_birth_weight_final_2$ht_) <- "history of hypertension"
var_label(low_birth_weight_final_2$ui_) <- "uterine irritability"
var_label(low_birth_weight_final_2$ftv_) <- "visits"
var_label(low_birth_weight_final_2$bwt_) <- "birth weight in grams"
var_label(low_birth_weight_final_2$ptl_) <-"number of previous premature labours"

#Hypothesis
Vr<-low_birth_weight_final_2 %>% select(low_,agecut,lwt_,race,smokecut,ht_,ui_,ftv_,ptl_,bwt_)
Vr %>% 
  tbl_summary(by = low_,missing=  'no',percent = "row") %>% 
  add_overall() %>% 
  modify_header(label~"*Factor*") %>%
  add_p()%>%
  modify_spanning_header(c("stat_1","stat_2")~"*Low birth weight*") %>% 
  bold_labels()%>%
  modify_caption("**: Risk factors associated with low infant birth weight**")

#Exploratory bivariate analysis for risk factors of low birth weight 
#Anova
two_way<-aov(low_birth_weight_final_2$low~low_birth_weight_final_2$age+low_birth_weight_final_2$lwt+low_birth_weight_final_2$age*low_birth_weight_final_2$lwt,data = low_birth_weight_final_2)
print(two_way)
summary(two_way)
#H0: there is no significant interaction between the independent variables
#H1: there is significant interaction between the two independent variables  (p value=0.7636)
#since p-value>0.05,therefore we fail to reject the null hypothesis and conclude that there is no significant interaction between the two independent variables.


two_way<-aov(low_birth_weight_final_2$low~low_birth_weight_final_2$age+low_birth_weight_final_2$bwt+low_birth_weight_final_2$age*low_birth_weight_final_2$bwt,data = low_birth_weight_final_2)
print(two_way)
summary(two_way)
#H0: there is no significant interaction between the independent variables
#H1: there is significant interaction between the two independent variables  (p value=0.723)
#Since p-value>0.05,therefore we fail to reject the null hypothesis and conclude that there is no significant interaction between the two independent variables.



#chi square
tbl2<-table(low_birth_weight_final_2$low,low_birth_weight_final_2$smoke)
tbl3<-table(low_birth_weight_final_2$low,low_birth_weight_final_2$ptl)
result2<-chisq.test(tbl2,correct=F)
result2
result3<-chisq.test(tbl3,correct=F)
result3


#correlation test
##converting new variables generated/populated into numeric 
low_birth_weight_final_2$low_<-as.numeric(low_birth_weight_final_2$low_)
low_birth_weight_final_2$agecut<-as.numeric(low_birth_weight_final_2$agecut)
low_birth_weight_final_2$lwt_<-as.numeric(low_birth_weight_final_2$lwt_)
result4=cor.test(low_birth_weight_final_2$agecut,low_birth_weight_final_2$low_)
result4

result5=cor.test(low_birth_weight_final_2$lwt_,low_birth_weight_final_2$low_)
result5

#multivariate logistic regression model of risk factors of low birth weight
model1=glm(low_~low_birth_weight_final_2$smokecut+low_birth_weight_final_2$ui_+low_birth_weight_final_2$ptl_+low_birth_weight_final_2$bwt_,family=binomial(link=logit),data=low_birth_weight_final_2)
tbl_regression(model1,exponentiate=T,intercept=T)
summary(model1)
