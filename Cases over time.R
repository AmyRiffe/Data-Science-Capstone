if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(oddsratio)) install.packages("oddsratio", repos = "http://cran.us.r-project.org")


library(ggplot2)
library(magrittr)
library(dplyr)
library(caret)
library(oddsratio)

#Call in data file
urlfile<-"https://raw.githubusercontent.com/AmyRiffe/Data-Science-Capstone/master/Cases_Over_Time.csv"
cases<-read.csv(urlfile)
#remove url for data file
rm(urlfile)

#Look at first few records
head(cases)

#Describe the data file
str(cases)
#3 variables with 168 observations
#Cases variable is the count of Covid19 cases on the date listed
#in variable ?..Day.of.Report.Date is the date the case was reported
#the variable Day.number is the number of days from the initial case

#rename column
names(cases)[1]<-"ReportDate"

#how many days are in the file?
max(cases$Day.number)
#smallest and largest number of cases on a day
min(cases$Cases)
max(cases$Cases)

#last day with the smallest number of cases
cases$ReportDate[which.min(cases$Cases)]

#plot out the cases by day
cases%>% ggplot(aes(Day.number, Cases))+
  geom_point(color="blue")

#There has clearly been an increase in cases over time
cases%>% ggplot(aes(Day.number, Cases))+
  geom_line()
#The daily line chart is much too jagged
#How much do the high counts impact the trend?

#plot adding in a locally weighted smoothed line (LOESS)
#this takes smaller windows of data and plots a trend
#then continues for the next window
#then builds a smoother trend based on those window trends
cases%>% ggplot(aes(Day.number, Cases))+
  geom_point(color="blue")+
  geom_smooth(color="green", span=.1, method.args=list(degree=1))


#call in case line list
urlfile<-"https://raw.githubusercontent.com/AmyRiffe/Data-Science-Capstone/master/case_list.csv"
caselist<-read.csv(urlfile)
#remove url
rm(urlfile)

#look at file structure
str(caselist)
#this needs some cleaning

#rename variable
names(caselist)[1]<-"Age"

#look at the variables
table(caselist$Hospitalized.for.COVID)
#recode into better categories
caselist$Hospitalized.for.COVID[caselist$Hospitalized.for.COVID=="No"]<-"no"
caselist$Hospitalized.for.COVID[caselist$Hospitalized.for.COVID==""]<-"no"
caselist$Hospitalized.for.COVID[caselist$Hospitalized.for.COVID=="pending"]<-"no"
caselist$Hospitalized.for.COVID[caselist$Hospitalized.for.COVID=="Pending"]<-"no"
caselist$Hospitalized.for.COVID[caselist$Hospitalized.for.COVID=="Yes"]<-"yes"

#drop unused factors after recode
caselist$Hospitalized.for.COVID<-(droplevels(caselist$Hospitalized.for.COVID))
table(caselist$Hospitalized.for.COVID)

#recode race
levels(caselist$Race)
caselist$Race[caselist$Race==""]<-"unknown"
caselist$Race[caselist$Race=="Black"]<-"black"
caselist$Race[caselist$Race=="Other"]<-"unknown"
caselist$Race[caselist$Race=="other"]<-"unknown"
caselist$Race[caselist$Race=="pending"]<-"unknown"
caselist$Race[caselist$Race=="Pending"]<-"unknown"
caselist$Race[caselist$Race=="Unknown"]<-"unknown"
caselist$Race[caselist$Race=="White"]<-"white"
caselist$Race[caselist$Race=="hispanic/LatinX"]<-"Hispanic/LatinX"

#change order of race factor so white is the reference group
caselist$Race<-relevel(caselist$Race, "white")

#drop unused factors after recode
caselist$Race<-(droplevels(caselist$Race))
table(caselist$Race)

#recode Gender
levels(caselist$Gender)
caselist$Gender[caselist$Gender=="f"]<-"F"
caselist$Gender[caselist$Gender==""]<-NA
table(caselist$Gender)

#drop unused factors after recode
caselist$Gender<-(droplevels(caselist$Gender))
table(caselist$Gender)

#demographics charts
caselist%>% filter(!is.na(Gender))%>%
  ggplot(aes(Gender))+
  geom_bar()

caselist%>%
  ggplot(aes(Age))+
  geom_bar()

caselist%>%
  ggplot(aes(x=forcats::fct_infreq(Race)))+
  geom_bar()

#outcome variable is whether a case was hospitalized
table(caselist$Hospitalized.for.COVID)
#percent of cases that are hospitalized
table(caselist$Hospitalized.for.COVID)[2]/nrow(caselist)*100

#demographics charts for hospitalizations
caselist%>% filter(!is.na(Gender)& Hospitalized.for.COVID=="yes")%>%
  ggplot(aes(Gender))+
  geom_bar()

caselist%>%filter(Hospitalized.for.COVID=="yes")%>%
  ggplot(aes(Age))+
  geom_bar()

caselist%>%filter(Hospitalized.for.COVID=="yes")%>%
  ggplot(aes(x=forcats::fct_infreq(Race)))+
  geom_bar()

#divide caselist into a training and test set
#divide as 50/50 because the number of outcome observations is low
set.seed(1)
test_index <- createDataPartition(y=caselist$Hospitalized.for.COVID, times = 1, p = 0.5, list = FALSE)
train_cases <- caselist%>% slice(-test_index)
test_cases <- caselist%>% slice(test_index)

#try a linear model
lm_fit <- mutate(train_cases, y = as.numeric(Hospitalized.for.COVID == "yes")) %>% lm(y ~ Age+Gender+Race+DayNumber, data = .)
p_hat <- predict(lm_fit, test_cases)
cutoff<-seq(p_hat[which.min(p_hat)], p_hat[which.max(p_hat)], 0.05)
#cutoffs are -0.12, -0.08, -0.03, 0.02, 0.07, 0.12, 0.17, 0.22, 0.27, 0.32, 0.37

y_hat1 <- ifelse(p_hat > -0.12, "yes", "no") %>% factor()
confusionMatrix(y_hat1, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat2 <- ifelse(p_hat > -0.08, "yes", "no") %>% factor()
confusionMatrix(y_hat2, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat3 <- ifelse(p_hat > -0.03, "yes", "no") %>% factor()
confusionMatrix(y_hat3, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat4 <- ifelse(p_hat > 0.02, "yes", "no") %>% factor()
confusionMatrix(y_hat4, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat5 <- ifelse(p_hat > 0.07, "yes", "no") %>% factor()
confusionMatrix(y_hat5, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat6 <- ifelse(p_hat > 0.12, "yes", "no") %>% factor()
confusionMatrix(y_hat6, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat7 <- ifelse(p_hat > 0.17, "yes", "no") %>% factor()
confusionMatrix(y_hat7, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat8 <- ifelse(p_hat > 0.22, "yes", "no") %>% factor()
confusionMatrix(y_hat8, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat9 <- ifelse(p_hat > 0.27, "yes", "no") %>% factor()
confusionMatrix(y_hat9, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat10 <- ifelse(p_hat > 0.32, "yes", "no") %>% factor()
confusionMatrix(y_hat10, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat11 <- ifelse(p_hat > 0.37, "yes", "no") %>% factor()
confusionMatrix(y_hat11, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]

#make a table of the accuracies
Accuracies<-rbind(confusionMatrix(y_hat1, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat2, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat3, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat4, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat5, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat6, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat7, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat8, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat9, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat10, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat11, test_cases$Hospitalized.for.COVID)$overall["Accuracy"])

#table of cutoff's and accuracies             
cbind(cutoff, Accuracies)     
#cutoff of 0.37 had best accuracy
#an accuracy of 0.9375

#lm model
summary(lm_fit)




rm(y_hat1)                  
rm(y_hat2)   
rm(y_hat3)
rm(y_hat4)
rm(y_hat5)
rm(y_hat6)
rm(y_hat7)
rm(y_hat8)
rm(y_hat9)
rm(y_hat10)
rm(y_hat11)

#try a logistic regression model
# fit logistic regression model
glm_fit <- train_cases %>% 
  mutate(y = as.numeric(Hospitalized.for.COVID == "yes")) %>%
  glm(y ~ Age+Gender+Race+DayNumber, data=., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_cases, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.37, "yes", "no") %>% factor
confusionMatrix(y_hat_logit, test_cases$Hospitalized.for.COVID)$overall[["Accuracy"]]

p_hat_logit[which.min(p_hat_logit)]
#0.0008
p_hat_logit[which.max(p_hat_logit)]
#0.89

cutoff_logit<-seq(0, 0.9, .1)

y_hat_logit1 <- ifelse(p_hat_logit > 0, "yes", "no") %>% factor()
confusionMatrix(y_hat_logit1, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat_logit2 <- ifelse(p_hat_logit > 0.1, "yes", "no") %>% factor()
confusionMatrix(y_hat_logit2, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat_logit3 <- ifelse(p_hat_logit > 0.2, "yes", "no") %>% factor()
confusionMatrix(y_hat_logit3, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat_logit4 <- ifelse(p_hat_logit > 0.3, "yes", "no") %>% factor()
confusionMatrix(y_hat_logit4, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat_logit5 <- ifelse(p_hat_logit > 0.4, "yes", "no") %>% factor()
confusionMatrix(y_hat_logit5, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat_logit6 <- ifelse(p_hat_logit > 0.5, "yes", "no") %>% factor()
confusionMatrix(y_hat_logit6, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat_logit7 <- ifelse(p_hat_logit > 0.6, "yes", "no") %>% factor()
confusionMatrix(y_hat_logit7, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat_logit8 <- ifelse(p_hat_logit > 0.7, "yes", "no") %>% factor()
confusionMatrix(y_hat_logit8, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat_logit9 <- ifelse(p_hat_logit > 0.8, "yes", "no") %>% factor()
confusionMatrix(y_hat_logit9, test_cases$Hospitalized.for.COVID)$overall["Accuracy"]
y_hat_logit10 <- ifelse(p_hat_logit > 0.9, "yes", "no") %>% factor()

#make a table of the accuracies
Accuracies_logit<-rbind(confusionMatrix(y_hat_logit1, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat_logit2, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat_logit3, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat_logit4, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat_logit5, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat_logit6, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat_logit7, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat_logit8, test_cases$Hospitalized.for.COVID)$overall["Accuracy"],confusionMatrix(y_hat_logit9, test_cases$Hospitalized.for.COVID)$overall["Accuracy"])                

rm(y_hat_logit1)                  
rm(y_hat_logit2)   
rm(y_hat_logit3)
rm(y_hat_logit4)
rm(y_hat_logit5)
rm(y_hat_logit6)
rm(y_hat_logit7)
rm(y_hat_logit8)
rm(y_hat_logit9)

#table of cutoff's and accuracies             
cbind(cutoff_logit, Accuracies_logit)     
#cutoff of 0.7 had best accuracy
#accuracy is 0.9375
#this is the same accuracy as lm

#Look at odds ratio for predictors
or_glm(data=train_cases, 
       model = glm_fit, 
       incr = list(DayNumber=7, Age=10))

#Odds ratios that do not include 1 are generally significant
#those were age, Raceblack, RaceNH/OPI, Raceunknown, and DayNumber
#age, Raceblack, RaceNH/OPI had a higher likelihood
#Raceunknown, and DayNumber had a lower likelihood

#chart showing counts by age, colored by hospitalization status
caselist%>%
  ggplot(aes(Age, color=Hospitalized.for.COVID))+
  geom_bar()
caselist%>%
  filter(!is.na(Gender))%>%
  ggplot(aes(Gender, color=Hospitalized.for.COVID))+
  geom_bar()

