#What states (of those I am willing to move to) have the highest paying data-related salaries?
#  Differences between job sub-categories?


library(dplyr)
names(salary_data_states)



data_interest = data.frame(salary_data_states[c("WORK_STATE", "JOB_TITLE_SUBGROUP", "PREVAILING_WAGE_PER_YEAR")])
data_int = na.exclude(data_interest)

ff = data_int[data_int$JOB_TITLE_SUBGROUP == c("data analyst" ), ]
fg = data_int[data_int$JOB_TITLE_SUBGROUP == c( "data scientist"), ]



fm = distinct(ff, WORK_STATE, .keep_all = TRUE)
dfg = distinct(fg, WORK_STATE, .keep_all = TRUE)

fm_sort <- fm[order(-fm$PREVAILING_WAGE_PER_YEAR),]
da_df <- head(fm_sort)


dfg_sort <- dfg[order(-fm$PREVAILING_WAGE_PER_YEAR),]
da_dfg <- head(dfg_sort)

da_dfcnn <- da_df[da_df$WORK_STATE %in% c("California", "New York", "New Jersey") ,]
dfg_dfcnn <- da_dfg[da_dfg$WORK_STATE %in% c("California", "New York", "New Jersey") ,]


library(ggplot2) 

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
  

total_dfm <- merge(fm,dfg,by="WORK_STATE")


#I need to make new dataset with column that has data analyst and data scientist in same column and prevailing wage in same column
ggplot(data=total_dfm, aes(x=total_dfm$WORK_STATE, y=total_dfm$PREVAILING_WAGE_PER_YEAR.x, fill=supp)) +
  geom_bar(stat="identity")



##Which companies have the highest salaries for those sub-types?



##Will the answer change if I take standard of living into account?

# 5 top states for standard of living
#Wyoming	
#Vermont	
#South Dakota	
#New Hampshire	
#Massachusetts	



fm = distinct(ff, WORK_STATE, .keep_all = TRUE)
dfg = distinct(fg, WORK_STATE, .keep_all = TRUE)

sl1 <- fm[fm$WORK_STATE %in% c("Wyoming", "Vermont", "South Dakota", "New Hampshire", "Massachusetts") ,]

sl2 <- fg[fg$WORK_STATE %in% c("Wyoming", "Vermont", "South Dakota", "New Hampshire", "Massachusetts") ,]

total_dfm$SUM <- rowSums(total_dfm[ ,c(3,5)])

sorted_total <- total_dfm[order(-total_dfm$SUM),]
#Here we can see that in data related jobs the highest paid states are 
#California, New York, Washington, New Jersey, Texas, Massachusetts

ggplot(data=total_dfm, aes(x=total_dfm$WORK_STATE, y=total_dfm$PREVAILING_WAGE_PER_YEAR.x, fill=supp)) +
  geom_bar(stat="identity")



data_interest = data.frame(salary_data_states[c("WORK_STATE", "JOB_TITLE_SUBGROUP", "PREVAILING_WAGE_PER_YEAR")])
data_int = na.exclude(data_interest)
data_int$JOB <- with(data_int, ifelse(JOB_TITLE_SUBGROUP == "data analyst", 1, 0))
newdata <- data_int[ which(data_int$JOB_TITLE_SUBGROUP== c('data analyst', 'data scientist')),]
ndf = distinct(newdata, WORK_STATE, .keep_all = TRUE)

ndff <- ndf[ndf$WORK_STATE %in% c("California", "New York", "Washington", "New Jersey", "Texas", "Massachusetts") ,]


ggplot(data=ndff, aes(x=ndf$WORK_STATE, y=ndf$PREVAILING_WAGE_PER_YEAR, fill=ndf$JOB_TITLE_SUBGROUP)) +
  geom_bar(stat="identity")

library(dplyr)
grouped <- newdata%>%group_by(WORK_STATE, JOB_TITLE_SUBGROUP)%>%summarise_all(funs(mean))
groupedstates <- grouped[grouped$WORK_STATE %in% c("California", "New York", "Washington", "New Jersey", "Texas") ,]

ggplot(data=groupedstates, aes(x=groupedstates$WORK_STATE, y=groupedstates$PREVAILING_WAGE_PER_YEAR, fill=groupedstates$JOB_TITLE_SUBGROUP)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x="States with Highest Pay", y="Prevailing Wage Per Year") +
  ggtitle("What states have the \n Highest Paying Data-related Salaries?")+
  guides(fill=guide_legend(title="And how do the data-\nrelated jobs differ \nin average salary?"))

ff = data_int[data_int$JOB_TITLE_SUBGROUP == c("data analyst" ), ]
fg = data_int[data_int$JOB_TITLE_SUBGROUP == c( "data scientist"), ]


#df <- data.frame(salary_data_states[c("WORK_STATE", "JOB_TITLE_SUBGROUP == "data scientist" & "data analyst", "PREVAILING_WAGE_PER_YEAR")])


