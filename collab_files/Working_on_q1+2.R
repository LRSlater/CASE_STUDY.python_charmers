#What states (of those I am willing to move to) have the highest paying data-related salaries?
#Differences between job sub-categories?

#Loading Packages 
library(dplyr)
library(ggplot2) 

#Exploring the datafile
names(salary_data_states)



#calculate the mean for  wage California data analyst
#The mean is 80470.05 for wage and 71625 for prevailing wage which doesn't align with mine so need to check this 





ffig = data_ints[data_ints$JOB_TITLE_SUBGROUP == c("data analyst" ), ]
caig = ffig[ffig$WORK_STATE == c("California" ), ]
mydf_2 <- caig %>% group_by(WORK_STATE) %>% summarize(PAID_WAGE_PER_YEAR=mean(PAID_WAGE_PER_YEAR))


#data related jobs

data_interest = data.frame(salary_data_states[c("WORK_STATE", "JOB_TITLE_SUBGROUP", "PREVAILING_WAGE_PER_YEAR")])
data_int = na.exclude(data_interest)
inter = data_int[data_int$JOB_TITLE_SUBGROUP == c("data analyst" ), ]
cali = inter[inter$WORK_STATE == c("California" ), ]



data_interests = data.frame(salary_data_states[c("WORK_STATE", "JOB_TITLE_SUBGROUP", "PAID_WAGE_PER_YEAR")])
data_ints = na.exclude(data_interests)
da_frame = data_ints[data_ints$JOB_TITLE_SUBGROUP == c("data analyst" ), ]

da_dataframe <- da_frame %>% group_by(WORK_STATE) %>% summarize(PAID_WAGE_PER_YEAR=mean(PAID_WAGE_PER_YEAR))

fg = data_int[data_int$JOB_TITLE_SUBGROUP == c( "data scientist"), ]

fm = distinct(ff, WORK_STATE, .keep_all = TRUE)
dfg = distinct(fg, WORK_STATE, .keep_all = TRUE)

fm_sort <- fm[order(-fm$PREVAILING_WAGE_PER_YEAR),]
da_df <- head(fm_sort)


dfg_sort <- dfg[order(-dfg$PREVAILING_WAGE_PER_YEAR),]
da_dfg <- head(dfg_sort)

da_dfcnn <- da_df[da_df$WORK_STATE %in% c("California", "New York", "New Jersey") ,]
dfg_dfcnn <- da_dfg[da_dfg$WORK_STATE %in% c("California", "New York", "New Jersey") ,]


#legend_colors <- c("data analyst" = "black", "data scientist" = "red")

ggplot() +
  geom_point(data = da_dfcnn, aes(WORK_STATE,PREVAILING_WAGE_PER_YEAR), color = "black", size = 1.5) +
  geom_point(data = dfg_dfcnn , aes(WORK_STATE,PREVAILING_WAGE_PER_YEAR), color = "red" , size = 1.5) +
  ggtitle("Salary for Data Analysts (black) and Data Scientists (red) in Highest Paying States")+
  xlab("State of Employment")+ ylab("Salary ($)")

#total_head <- merge(da_df, da_dfg, by = "WORK_STATE")

#ggplot() +
  #geom_point(data = da_df, aes(WORK_STATE,PREVAILING_WAGE_PER_YEAR)) +
  #geom_point(data = da_dfg, aes(WORK_STATE,PREVAILING_WAGE_PER_YEAR), colour = 'red', size = 3) 
  

fm = distinct(ff, WORK_STATE, .keep_all = TRUE)
dfg = distinct(fg, WORK_STATE, .keep_all = TRUE)
total_dfm <- merge(fm,dfg,by="WORK_STATE")


sl1 <- fm[fm$WORK_STATE %in% c("Wyoming", "Vermont", "South Dakota", "New Hampshire", "Massachusetts") ,]

sl2 <- fg[fg$WORK_STATE %in% c("Wyoming", "Vermont", "South Dakota", "New Hampshire", "Massachusetts") ,]

total_dfm$SUM <- rowSums(total_dfm[ ,c(3,5)])
--------------------------------------------------------

ggplot(data=total_dfm, aes(x=total_dfm$WORK_STATE, y=total_dfm$PREVAILING_WAGE_PER_YEAR.x, fill=supp)) +
  geom_bar(stat="identity")

data_interest = data.frame(salary_data_states[c("WORK_STATE", "JOB_TITLE_SUBGROUP", "PREVAILING_WAGE_PER_YEAR")])
data_int = na.exclude(data_interest)
data_int$JOB <- with(data_int, ifelse(JOB_TITLE_SUBGROUP == "data analyst", 1, 0))

newdata <- data_ints[ which(data_int$JOB_TITLE_SUBGROUP== c('data analyst', 'data scientist')),]
ndf = distinct(newdata, WORK_STATE, .keep_all = TRUE)
ndff <- ndf[ndf$WORK_STATE %in% c("California", "New York", "Washington", "New Jersey", "Texas", "Massachusetts") ,]
ggplot(data=ndff, aes(x=ndf$WORK_STATE, y=ndf$PREVAILING_WAGE_PER_YEAR, fill=ndf$JOB_TITLE_SUBGROUP)) +
  geom_bar(stat="identity")
------------------------------------------------------
#Let's try again for something clearer
#I need to make new dataset with column that has data analyst and data scientist in same column and prevailing wage in same column
group <- newdata%>%group_by(WORK_STATE, JOB_TITLE_SUBGROUP)%>%summarise_all(funs(mean))
# Removing states I would not want to live in (too hot):
grouped<-  subset(group, WORK_STATE!="Florida" & WORK_STATE!="Louisiana")

#this is averaging da and ds columns together- need to separate them
da_data <- newdata %>% group_by(WORK_STATE) %>% summarize(PAID_WAGE_PER_YEAR=mean(PAID_WAGE_PER_YEAR))

#select states with highest pay in data related jobs
sorted_total <- total_dfm[order(-total_dfm$SUM),]
#Here we can see that in data reflated jobs the highest paid states are 
#California, New York, Washington, New Jersey, Texas, Massachusetts

groupedstates <- grouped[grouped$WORK_STATE %in% c("California", "New York", "Washington", "New Jersey", "Texas") ,]

ggplot(data=groupedstates, aes(x=groupedstates$WORK_STATE, y=groupedstates$PREVAILING_WAGE_PER_YEAR, fill=groupedstates$JOB_TITLE_SUBGROUP)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x="States with Highest Pay", y="Prevailing Wage Per Year") +
  ggtitle("What states have the \n Highest Paying Data-related Salaries?")+
  guides(fill=guide_legend(title="And how do the data-\nrelated jobs differ \nin average salary?"))+
  scale_fill_manual(values=c('#003366', '#6699CC'))


##Which companies have the highest salaries for those sub-types?

companies = data.frame(salary_data_states[c("EMPLOYER_NAME", "WORK_STATE", "JOB_TITLE_SUBGROUP", "PREVAILING_WAGE_PER_YEAR")])
comp = na.exclude(companies)
#subset data jobs and sort by prevailing wage
comp_sal <- comp[ which(comp$JOB_TITLE_SUBGROUP== c('data analyst', 'data scientist')),]

comp_sal_sort <- comp_sal[order(-comp_sal$PREVAILING_WAGE_PER_YEAR),]
#distinct company and group salary by wage for scientist and analyst


com_da = comp_sal_sort[comp_sal_sort$JOB_TITLE_SUBGROUP== c('data analyst'),]
com_ds = comp_sal_sort[comp_sal_sort$JOB_TITLE_SUBGROUP== c('data scientist'),]

comp_da = head(com_da)
comp_ds = head(com_ds)


ggplot(data=comp_da, aes(x=comp_da$EMPLOYER_NAME, y=comp_da$PREVAILING_WAGE_PER_YEAR)) +
  geom_bar(stat="identity") +
  labs(x="Companies with Highest Pay", y="Prevailing Wage Per Year") +
  ggtitle("What companies have the Highest Paying Salaries for data analyst jobs?")
##Will the answer change if I take standard of living into account?

