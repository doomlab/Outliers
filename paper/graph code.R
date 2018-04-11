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
                           "part", "stat", "exp", "analysis", "used")
View(long.testing.data)
long.testing.data = subset(long.testing.data, used == 1)

View(long.testing.data)

long.reason.data=long.testing.data[,c("time.pulled", "Type", "peopleor.data.points", "take.out.or.leave.in", "run.with.or.without",  
                                       "analysis", "part", "stat", "exp")]
long.reason.data=melt(long.reason.data,
                      id=c("time.pulled", "Type", "peopleor.data.points", "take.out.or.leave.in", "run.with.or.without",  
                           "analysis"),
                      measured=c("part", "stat", "exp"))
colnames(long.reason.data)=c("time.pulled", "Type", "peopleor.data.points", "take.out.or.leave.in", "run.with.or.without",  
                             "analysis", "reasion.type", "used")
View(long.reason.data)
long.reason.data = subset(long.reason.data, used == 1)
long.reason.data<-na.omit(long.reason.data)
 View(long.reason.data)

long.reason.data$reason=factor(long.reason.data$reasion.type)
reason.table.test = table( long.reason.data$reason, long.reason.data$analysis)
View(reason.table.test)

barplot(reason.table.test, col=c("purple", "blue", "green"), space=1, legend = rownames(reason.table.test))




