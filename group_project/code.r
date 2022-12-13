library(readxl)
library(table1)
library(ggplot2)
library(anytime)
library(dplyr)
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

table1(~CASE_STATUS|VISA_CLASS,data=DS311,overall="Total")

#days waiting for decision
DS311$CASE_RECEIVED_DATE=as.Date(DS311$CASE_RECEIVED_DATE,format = "%m/%d/%Y")
DS311$DECISION_DATE=as.Date(DS311$DECISION_DATE,format = "%m/%d/%Y")
DS311$day<-as.numeric(difftime(DS311$DECISION_DATE,DS311$CASE_RECEIVED_DATE,units="days"))
table1(~day|CASE_STATUS,data=DS311,overall="Total")

#job sub-categories get overpaid or underpaid?
DS311.new=subset(DS311,DS311$PREVAILING_WAGE_PER_YEAR!="NA")
overall=c(mean(DS311$PAID_WAGE_PER_YEAR),median(DS311$PAID_WAGE_PER_YEAR),mean(as.numeric(DS311.new$PREVAILING_WAGE_PER_YEAR)),median(as.numeric(DS311.new$PREVAILING_WAGE_PER_YEAR))) #total mean and median 
table1(~PAID_WAGE_PER_YEAR|JOB_TITLE_SUBGROUP,data=DS311, overall="Total")

DS311.new=subset(DS311,DS311$PREVAILING_WAGE_PER_YEAR!="NA")
a=DS311.new %>% group_by(JOB_TITLE_SUBGROUP) %>%
    summarise(mean_wage=mean(PAID_WAGE_PER_YEAR),median_wage=median(PAID_WAGE_PER_YEAR)
              ,mean_prevail=mean(as.numeric(PREVAILING_WAGE_PER_YEAR)),median_prevail=median(as.numeric(PREVAILING_WAGE_PER_YEAR)))
b=data.frame(rbind(a[,-1],overall),stringsAsFactors = TRUE)
#mean received vs median received
barplot(as.matrix(t(b))[-c(3:4),],beside=TRUE,col=c("red","green"),lwd=2,name=c(a$JOB_TITLE_SUBGROUP,"overall"))
legend("topright",legend=c("mean received","median received"),fill=c("red","green"))

#mean received vs mean prevailing
barplot(as.matrix(t(b))[-c(2,4),],beside=TRUE,col=c("red","blue"),lwd=2,name=c(a$JOB_TITLE_SUBGROUP,"overall"))
legend("topright",legend=c("mean received","mean prevailing"),fill=c("red","blue"))

par(mfrow=c(1,1))
b
#compare with prevailing wage
DS311$evaluate=rep(NA,length(DS311$CASE_NUMBER))
for (i in 1:length(DS311$evaluate)){
  if (DS311$PREVAILING_WAGE_PER_YEAR[i]=="NA"){
    DS311$evaluate[i]="not certified"
  }
  else if(DS311$PAID_WAGE_PER_YEAR[i]>DS311$PREVAILING_WAGE_PER_YEAR[i]){
    DS311$evaluate[i]="overpay"
  }else{
    DS311$evaluate[i]="underpay"

  }
  print (i)
}

#companies that tend to overpay or underpay
companies=DS311 %>% group_by(EMPLOYER_NAME) %>%
  summarise(mean_wage=mean(PAID_WAGE_PER_YEAR),median_wage=median(PAID_WAGE_PER_YEAR),count=n())
companies.use=data.frame(companies,stringsAsFactors = TRUE)
companies.use=subset(companies.use,companies.use$count>99)
companies.overpay=subset(companies.use,companies$mean_wage>overall[1])
companies.underpay=subset(companies.use,companies$median_wage<overall[1])
head(companies.overpay[order(companies.overpay$mean_wage,decreasing=TRUE),])
head(companies.underpay[order(companies.underpay$mean_wage,decreasing=FALSE),])
#companies based on prevailing wage per year
companies_eval=DS311 %>% group_by(EMPLOYER_NAME) %>%
  summarise(overpay=length(evaluate[evaluate=="overpay"]),underpay=length(evaluate[evaluate=="underpay"])
            ,overpayrate=overpay/(overpay+underpay),underpayrate=underpay/(overpay+underpay),count=n())
companies.evaluse=data.frame(companies_eval,stringsAsFactors = TRUE)
companies.evaluse=subset(companies.evaluse,companies.evaluse$count >99)
head(companies.evaluse[order(companies.evaluse$overpayrate,decreasing=TRUE),],15)
head(companies.evaluse[order(companies.evaluse$underpayrate,decreasing=TRUE),],15)
#state
state=DS311 %>% group_by(WORK_STATE_ABBREVIATION) %>%
  summarise(overpay=length(evaluate[evaluate=="overpay"]),underpay=length(evaluate[evaluate=="underpay"])
            ,overpayrate=overpay/(overpay+underpay),underpayrate=underpay/(overpay+underpay),count=n())

state
state.use=data.frame(state,stringsAsFactors = TRUE)
head(state.use[order(state.use$overpayrate,decreasing=TRUE),],7)
head(state.use[order(state.use$underpayrate,decreasing=TRUE),],8)

sum(companies.evaluse$overpay)
sum(companies.evaluse$underpay)

DS311.CA=subset(DS311,DS311$WORK_STATE=="California")
View(DS311.CA)
#city in california
CA=DS311.CA %>% group_by(WORK_CITY) %>%
  summarise(overpay=length(evaluate[evaluate=="overpay"]),underpay=length(evaluate[evaluate=="underpay"]),
            overpayrate=overpay/(overpay+underpay),underpayrate=underpay/(overpay+underpay),count=n())


CA.frame=data.frame(rbind(CA),stringsAsFactors = TRUE)
CA.framecity=subset(CA.frame,CA.frame$count>99)
head(CA.framecity[order(CA.framecity$overpayrate,decreasing=TRUE),])
head(CA.framecity[order(CA.framecity$underpayrate,decreasing=TRUE),])
#companies in california
CA.com=DS311.CA %>% group_by(EMPLOYER_NAME) %>%
  summarise(overpay=length(evaluate[evaluate=="overpay"]),underpay=length(evaluate[evaluate=="underpay"]),
            overpayrate=overpay/(overpay+underpay),underpayrate=underpay/(overpay+underpay),count=n())
View(CA)

CAcom.frame=data.frame(rbind(CA.com),stringsAsFactors = TRUE)
CA.frameccom=subset(CAcom.frame,CAcom.frame$count>99)
head(CA.frameccom[order(CA.frameccom$overpayrate,decreasing=TRUE),])
head(CA.frameccom[order(CA.frameccom$underpayrate,decreasing=TRUE),])
View(DS311)
#interesting
table1(~COUNTRY_OF_CITIZENSHIP|CASE_STATUS,data=DS311,overall="Total")



#additional pie chart
#assistant professor
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="assistant professor"&DS311$PAID_WAGE_PER_YEAR<confidence.lower[1]])
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="assistant professor"&DS311$PAID_WAGE_PER_YEAR>confidence.upper[1]])

#attorney
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="attorney"&DS311$PAID_WAGE_PER_YEAR<confidence.lower[2]])
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="attorney"&DS311$PAID_WAGE_PER_YEAR>confidence.upper[2]])
#business analyst
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="business analyst"&DS311$PAID_WAGE_PER_YEAR<confidence.lower[3]])
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="bussiness analyst"&DS311$PAID_WAGE_PER_YEAR>confidence.upper[3]])
#data analyst
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="data analyst"&DS311$PAID_WAGE_PER_YEAR<confidence.lower[4]])
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="data analyst"&DS311$PAID_WAGE_PER_YEAR>confidence.upper[4]])
#data scientist
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="data scientist"&DS311$PAID_WAGE_PER_YEAR<confidence.lower[5]])
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="data scientist"&DS311$PAID_WAGE_PER_YEAR>confidence.upper[5]])
#management consultant
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="management consultant"&DS311$PAID_WAGE_PER_YEAR<confidence.lower[6]])
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="management consultant"&DS311$PAID_WAGE_PER_YEAR>confidence.upper[6]])
#software engineer
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="software engineer"&DS311$PAID_WAGE_PER_YEAR<confidence.lower[7]])
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="software engineer"&DS311$PAID_WAGE_PER_YEAR>confidence.upper[7]])
#teacher
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="teacher"&DS311$PAID_WAGE_PER_YEAR<confidence.lower[8]])
length(DS311$PAID_WAGE_PER_YEAR[DS311$JOB_TITLE_SUBGROUP=="teacher"&DS311$PAID_WAGE_PER_YEAR>confidence.upper[8]])

unique(DS311$JOB_TITLE_SUBGROUP)



#pie chart
#job sub categories
#assitant professor
e1=length(DS311$evaluate[DS311$evaluate=="not certified"&DS311$JOB_TITLE_SUBGROUP=="assistant professor"])
e2=length(DS311$evaluate[DS311$evaluate=="overpay"&DS311$JOB_TITLE_SUBGROUP=="assistant professor"])
e3=length(DS311$evaluate[DS311$evaluate=="underpay"&DS311$JOB_TITLE_SUBGROUP=="assistant professor"])
slices <- c(e1,e2,e3)
lbls <- c("not cerfified", "overpay", "underpay")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Underpay/Overpay for Assistant Professor")
#attorney
e1=length(DS311$evaluate[DS311$evaluate=="not certified"&DS311$JOB_TITLE_SUBGROUP=="attorney"])
e2=length(DS311$evaluate[DS311$evaluate=="overpay"&DS311$JOB_TITLE_SUBGROUP=="attorney"])
e3=length(DS311$evaluate[DS311$evaluate=="underpay"&DS311$JOB_TITLE_SUBGROUP=="attorney"])
slices <- c(e1,e2,e3)
lbls <- c("not cerfified", "overpay", "underpay")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Underpay/Overpay for attorney")
#business analyst
e1=length(DS311$evaluate[DS311$evaluate=="not certified"&DS311$JOB_TITLE_SUBGROUP=="business analyst"])
e2=length(DS311$evaluate[DS311$evaluate=="overpay"&DS311$JOB_TITLE_SUBGROUP=="business analyst"])
e3=length(DS311$evaluate[DS311$evaluate=="underpay"&DS311$JOB_TITLE_SUBGROUP=="business analyst"])
slices <- c(e1,e2,e3)
lbls <- c("not cerfified", "overpay", "underpay")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Underpay/Overpay for Business Analyst")

#data analyst
e1=length(DS311$evaluate[DS311$evaluate=="not certified"&DS311$JOB_TITLE_SUBGROUP=="data analyst"])
e2=length(DS311$evaluate[DS311$evaluate=="overpay"&DS311$JOB_TITLE_SUBGROUP=="data analyst"])
e3=length(DS311$evaluate[DS311$evaluate=="underpay"&DS311$JOB_TITLE_SUBGROUP=="data analyst"])
slices <- c(e1,e2,e3)
lbls <- c("not cerfified", "overpay", "underpay")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Underpay/Overpay for Data Analyst")

#data scientist
e1=length(DS311$evaluate[DS311$evaluate=="not certified"&DS311$JOB_TITLE_SUBGROUP=="data scientist"])
e2=length(DS311$evaluate[DS311$evaluate=="overpay"&DS311$JOB_TITLE_SUBGROUP=="data scientist"])
e3=length(DS311$evaluate[DS311$evaluate=="underpay"&DS311$JOB_TITLE_SUBGROUP=="data scientist"])
slices <- c(e1,e2,e3)
lbls <- c("not cerfified", "overpay", "underpay")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Underpay/Overpay for Data Scientist")

#management consultant
e1=length(DS311$evaluate[DS311$evaluate=="not certified"&DS311$JOB_TITLE_SUBGROUP=="management consultant"])
e2=length(DS311$evaluate[DS311$evaluate=="overpay"&DS311$JOB_TITLE_SUBGROUP=="management consultant"])
e3=length(DS311$evaluate[DS311$evaluate=="underpay"&DS311$JOB_TITLE_SUBGROUP=="management consultant"])
slices <- c(e1,e2,e3)
lbls <- c("not cerfified", "overpay", "underpay")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Underpay/Overpay for Management Consultant")

#software engineer
e1=length(DS311$evaluate[DS311$evaluate=="not certified"&DS311$JOB_TITLE_SUBGROUP=="software engineer"])
e2=length(DS311$evaluate[DS311$evaluate=="overpay"&DS311$JOB_TITLE_SUBGROUP=="software engineer"])
e3=length(DS311$evaluate[DS311$evaluate=="underpay"&DS311$JOB_TITLE_SUBGROUP=="software engineer"])
slices <- c(e1,e2,e3)
lbls <- c("not cerfified", "overpay", "underpay")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Underpay/Overpay for Software Engineer")

#teacher
e1=length(DS311$evaluate[DS311$evaluate=="not certified"&DS311$JOB_TITLE_SUBGROUP=="teacher"])
e2=length(DS311$evaluate[DS311$evaluate=="overpay"&DS311$JOB_TITLE_SUBGROUP=="teacher"])
e3=length(DS311$evaluate[DS311$evaluate=="underpay"&DS311$JOB_TITLE_SUBGROUP=="teacher"])
slices <- c(e1,e2,e3)
lbls <- c("not cerfified", "overpay", "underpay")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Underpay/Overpay for Teacher")