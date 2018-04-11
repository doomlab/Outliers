library(reshape)
out.testing = subset(master, mention.outliers == "Yes")

##create reason columns
out.testing$part = 0
out.testing$part[ grep("part", out.testing$reason.code)] = 1
out.testing$stat = 0
out.testing$stat[ grep("stat", out.testing$reason.code)] = 1
out.testing$exp = 0
out.testing$exp[ grep("unus", out.testing$reason.code)] = 1
names(out.testing)
long.testing=out.testing[,c("time.pulled", "Type", "peopleor.data.points", "take.out.or.leave.in", "run.with.or.without", 
                            "part", "stat", "exp", "Basics", "ANOVA" , "Regression", "ChiSquare", "Nonparametric", 
                            "Modeling", "BayesOther" )]

long.testing.data=melt(long.testing, 
                    id = c("time.pulled", "Type", "peopleor.data.points", "take.out.or.leave.in", "run.with.or.without", 
                           "part", "stat", "exp"),
                    measured = c("Basics", "ANOVA" , "Regression", "ChiSquare", "Nonparametric", 
                                 "Modeling", "BayesOther"))
#View(choice.data)          
colnames(long.testing.data)=c("time.pulled", "Type", "peopleor.data.points", "take.out.or.leave.in", "run.with.or.without", 
                           "part", "stat", "exp", "variable", "analysis")
long.testing.data$analysis= gl(7, 457, labels = c("Basics", "ANOVA" , "Regression", "ChiSquare", "Nonparametric", 
                                           "Modeling", "BayesOther"))
View(long.testing.data)

long.reason.data=long.testing.data[,c("time.pulled", "Type", "peopleor.data.points", "take.out.or.leave.in", "run.with.or.without",  
                                       "analysis", "part", "stat", "exp")]
long.reason.data=melt(long.reason.data,
                      id=c("time.pulled", "Type", "peopleor.data.points", "take.out.or.leave.in", "run.with.or.without",  
                           "analysis"),
                      measured=c("part", "stat", "exp"))
colnames(long.reason.data)=c("time.pulled", "Type", "peopleor.data.points", "take.out.or.leave.in", "run.with.or.without",  
                             "analysis", "var", "reason.type")
long.reason.data$reason.type=gl(3, 3199, labels=c("Participant Error", "Statistical Reason", "Unusable Data"))

View(long.reason.data)
##create a data frame of the percentages
reason.table.test = table(long.reason.data$analysis, long.reason.data$reason.type)
View(reason.table.test)
reason.table.test = as.data.frame(reason.table.test)
colnames(reason.table.test) = c("analysis", "reason.type", "Freq")
library(dplyr)
View(reason.table.test)

reasonspercent = group_by(reason.table.test, reason.type ) %>%
  mutate(percent = Freq/sum(Freq)*100)
View(reasonspercent)
barplot()





