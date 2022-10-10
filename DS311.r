library(readxl)
library(table1)
library(ggplot2)
library(anytime)
DS311 <- read_excel("C:/Users/nqhun/Downloads/salary_data_states.xlsx")
View(DS311)

unique(DS311$JOB_TITLE_SUBGROUP)
length(unique(DS311$JOB_TITLE))
table1(~PAID_WAGE_PER_YEAR|JOB_TITLE_SUBGROUP,data=DS311, overall="Total")
table1(~WORK_STATE|JOB_TITLE_SUBGROUP,data=DS311, overall="Total")
table(DS311$VISA_CLASS)
table(DS311$CASE_STATUS)
table1(~PAID_WAGE_PER_YEAR|COLLEGE_MAJOR_REQUIRED,data=DS311,overall="Total")

#country of citizenship
x<-DS311$COUNTRY_OF_CITIZENSHIP[DS311$COUNTRY_OF_CITIZENSHIP!="NA"]
barplot(table(x)/length(x),ylim=c(0,0.8))
table1(~x)

#days waiting for decision
DS311$CASE_RECEIVED_DATE=as.Date(DS311$CASE_RECEIVED_DATE,format = "%m/%d/%Y")
DS311$DECISION_DATE=as.Date(DS311$DECISION_DATE,format = "%m/%d/%Y")
DS311$diff<-as.numeric(difftime(DS311$DECISION_DATE,DS311$CASE_RECEIVED_DATE,units="days"))
table1(~diff|CASE_STATUS,data=DS311,overall="Total")
