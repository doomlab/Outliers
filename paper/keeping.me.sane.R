
master = read.csv("/Users/kdvdnf/Documents/GitHub/Outliers/paper/outliers\ complete.csv")
##experiment size
table(master$Journal, master$time.pulled)
table(master$mention.outliers)
master$mention.outliers = factor(master$mention.outliers,
                                 levels = c("no", "yes"),
                                 labels = c("No", "Yes"))

##create a data frame of the percentages
outsummary = table(master$mention.outliers, master$time.pulled, master$Type)
outsummary = as.data.frame(outsummary)
colnames(outsummary) = c("mention.outliers", "time.pulled", "Type", "Freq")

##here we want to calculate if they mention outliers by year and type
##want to focus on the yeses 
library(dplyr)

outpercent = group_by(outsummary, time.pulled, Type) %>%
  mutate(percent = Freq/sum(Freq)*100)



library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), 
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

graphdata = subset(outpercent, mention.outliers == "Yes")

#calculate CIs to add to graph
graphdata$sample = as.data.frame(table(master$time.pulled, master$Type))$Freq
graphdata$SE = sqrt(graphdata$percent*(100-graphdata$percent)/graphdata$sample)

dotplot = ggplot(graphdata, aes(Type, percent, color = time.pulled))
finalgraph = dotplot +
  geom_pointrange(aes(ymin=percent-1.96*SE, ymax=percent+1.96*SE))+ 
  cleanup +
  xlab("Field of Journal") +
  ylab("Percent of Outlier Mentions") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_manual(name = "Year", 
                     values = c("maroon", "gray")) + 
  coord_cartesian(ylim = c(0,75))

tiff(filename = "mention_graph.tiff", res = 300, width = 8, 
     height = 6, units = 'in', compression = "lzw")
plot(finalgraph)
dev.off()



library(MOTE)
graphdata$prop = graphdata$percent/100
types = levels(graphdata$Type)
for (i in 1:length(types))
{
  saved = d.prop(graphdata$prop[graphdata$Type==types[i]][1],
                 graphdata$prop[graphdata$Type==types[i]][2],
                 graphdata$sample[ graphdata$Type==types[i]][1], 
                 graphdata$sample[ graphdata$Type==types[i]][2],
                 a = .05) 
  print(c(types[i],saved$d))
}




master$reason.code
table(master$reason.code)
reasons = subset(master, mention.outliers == "Yes")

##create reason columns
reasons$part = 0
reasons$part[ grep("part", reasons$reason.code)] = 1
reasons$stat = 0
reasons$stat[ grep("stat", reasons$reason.code)] = 1
reasons$exp = 0
reasons$exp[ grep("unus", reasons$reason.code)] = 1

##create a data frame of the percentages
reasonssummary = table(reasons$part, reasons$time.pulled, reasons$Type)
reasonssummary = as.data.frame(reasonssummary)
colnames(reasonssummary) = c("yesno", "time.pulled", "Type", "part.freq")
reasonssummary$stat.freq = as.data.frame(table(reasons$stat, reasons$time.pulled, reasons$Type))$Freq
reasonssummary$exp.freq = as.data.frame(table(reasons$exp, reasons$time.pulled, reasons$Type))$Freq

reasonspercent = group_by(reasonssummary, time.pulled, Type) %>%
  mutate(part.percent = part.freq/sum(part.freq)*100) %>%
  mutate(stat.percent = stat.freq/sum(stat.freq)*100) %>%
  mutate(exp.percent = exp.freq/sum(exp.freq)*100) 

kable(reasonspercent)


peep_data = subset(master, mention.outliers == "Yes")
table(peep_data$peopleor.data.points)

##create a data frame of the percentages
peep_datasummary = table(peep_data$peopleor.data.points, peep_data$time.pulled, peep_data$Type)
peep_datasummary = as.data.frame(peep_datasummary)
colnames(peep_datasummary) = c("code", "time.pulled", "Type", "Freq")

peep_datapercent = group_by(peep_datasummary, time.pulled, Type) %>%
  mutate(percent = Freq/sum(Freq)*100)
kable(peep_datapercent)



library(reshape)
analyses = master[ , c(3, 4, 10, 20:26)]
View(analyses)
longanalyses = melt(analyses,
                    id = c("time.pulled", "Type", "mention.outliers"))
colnames(longanalyses)[4:5] = c("analysis.type", "used")

longanalyses = subset(longanalyses, used == 1)

##create a data frame of the percentages
analysessummary = table(longanalyses$mention.outliers, longanalyses$time.pulled, longanalyses$analysis.type)
analysessummary = as.data.frame(analysessummary)
colnames(analysessummary) = c("mention.outliers", "time.pulled",  "analysis.type", "Freq")

analysespercent = group_by(analysessummary, time.pulled, analysis.type) %>%
  mutate(percent = Freq/sum(Freq)*100)

graphdata = subset(analysespercent, mention.outliers == "Yes")

#calculate CIs to add to graph
graphdata$sample = as.data.frame(table(longanalyses$time.pulled, longanalyses$analysis.type))$Freq
graphdata$SE = sqrt(graphdata$percent*(100-graphdata$percent)/graphdata$sample)

dotplot2 = ggplot(graphdata, aes(analysis.type, percent, color = time.pulled))
finalgraph = dotplot2 +
  geom_pointrange(aes(ymin=percent-1.96*SE, ymax=percent+1.96*SE))+ 
  cleanup +
  xlab("Type of Analysis") +
  ylab("Percent of Outlier Mentions") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_manual(name = "Year", 
                     values = c("maroon", "gray")) + 
  coord_cartesian(ylim = c(0,50))

tiff(filename = "analyses_graph.tiff", res = 300, width = 15, 
     height = 12, units = 'in', compression = "lzw")
plot(finalgraph)
dev.off()


table1<-table(master$time.pulled, master$mention.outliers)
chisq.test(table1)
table1

#let's try a 3-way table
# 3-Way Frequency Table 
#
mytable <- xtabs(~time.pulled+mention.outliers+Type, data=master)
model2<-loglm(~time.pulled+mention.outliers+Type, data=mytable)
model2
ftable(mytable)


#try for journal?
#mytable2 <- xtabs(~time.pulled+mention.outliers+Journal, data=master)
#loglm(~time.pulled+mention.outliers+Journal, data=mytable2)
#ftable(mytable2)
#can't talk about this becasue of data sparsity

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
chisq.test(reason.table.test) #thre a "approximation may be incorrect" error, so switched to the Fisher test.
#fisher.test(reason.table.test, simulate.p.value=TRUE,B=1e7) # same as chisq

#so probably don't need to graph it so much. 
barplot(reason.table.test, col=c("purple", "blue", "green"), space=1, legend = rownames(reason.table.test))


#rationalextime
#View(long.reason.data)
long.reason.data.na<-na.omit(long.reason.data)
length(long.reason.data.na$time.pulled)
t.r.time<-table(long.reason.data.na$time.pulled, long.reason.data.na$reason)
t.r.time
chisq.test(t.r.time)
names(t.r.time.test)
#View(t.r.time.test$observed)
#associated: more overall in 2017, but most increase in use of statistical reasoning (.59, as compared to part=.31, exp=.4).

#rationalexdiscipline
names(long.reason.data.na)
t.r.field<-table(long.reason.data.na$Type, long.reason.data.na$reason)
View(t.r.field)

#can't discuss this, too sparse of data


#rationalexanalysis
#this one done above


out.testing$peopleor.data.points.f=factor(out.testing$peopleor.data.points)
time.people<-table(out.testing$time.pulled, out.testing$peopleor.data.points)
time.people<-time.people[,c(2:5)]
time.people

chisq.test(time.people)#thre a "approximation may be incorrect" error, so switched to the Fisher test.
#fisher.test(time.people, simulate.p.value=TRUE,B=1e7) # same as chisq basically p= 0.01594
#so, there is a sign. difference, mainly driven by the fact that taking out people has doubled. 


long.reason.data.2=long.reason.data[,c(1, 3:9)]
people.reason=table(long.reason.data$peopleor.data.points, long.reason.data$reason)
people.reason
#so can't run this one b/c data sparsity

time.out=table(long.reason.data$take.out.or.leave.in, long.reason.data$time.pulled)
time.out
#can't run that

reason.out=table(long.reason.data$reason, long.reason.data$take.out.or.leave.in)
reason.out
#can't run that either

out.people=table(out.testing$take.out.or.leave.in, out.testing$peopleor.data.points)
out.people
names(out.testing)

#this data is super sparse, let's keep this part simple and just describe the data we have. 
a<-table(out.testing$peopleor.data.points)     

table(out.testing$take.out.or.leave.in)     
table(out.testing$run.with.or.without)
table(out.testing$difference.between.analyses.with.and.without)
table(out.testing$report.results.on.if.did.both)
