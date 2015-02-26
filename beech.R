#load in results
Results <- read.csv("~/GitHub/essay/Growth Phenology/Results.csv", sep=";", dec=",")

#fixed effect will be Plot and tree ID ? 

#random effect will be: Year, groups and/or position

#only select fagus sylavatica in tree species

#age trend will be noticed in discussion but not included in model ( only 20 Years )

#nested: Year then groups then position? maybe groups and position are interacting and only need one

#generalized linear mixed model 

attach(Results)
workingdata1= Results[Position =="dbh",]
detach(Results)
attach(workingdata1)
workingdata2 = workingdata1[Tree_species =="Fagus_sylvatica",]
detach(workingdata1)
attach(workingdata2)
workingdata2 = workingdata2[,-c(9,10,11,12),]


final = na.omit(workingdata2)
##alright, alles weg was man nicht brauch
##nicht plausibele ergebnisse immer noch drin.. 

#remove rows which have NAs (metadata)
final = na.omit(workingdata2)

search()
detach(workingdata2)
attach(final)

#now fill in all NAs from the plausibility check: 
          
final$End [which (final$Dendro_ID=="FaSy5"& final$Year_CE=="2013"&final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy5"& final$Year_CE=="2013"& final$Plot_abb=="SIL")] = NA

final$Begin [which (final$Dendro_ID=="FaSy6"& final$Year_CE=="2013"& final$Plot_abb=="GUE")] = NA
final$Period [which (final$Dendro_ID=="FaSy6"& final$Year_CE=="2013"& final$Plot_abb=="GUE")] = NA

final$End [which (final$Dendro_ID=="FaSy4"& final$Year_CE=="1997"& final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy4"& final$Year_CE=="1997"& final$Plot_abb=="SIL")] = NA

#Begin, Period = NA for i in final$Dendro_ID with final$Year_CE with final$Plot_abb

final$Begin [which (final$Dendro_ID=="FaSy3"& final$Year_CE=="2013"& final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy3"& final$Year_CE=="2013"& final$Plot_abb=="SIL")] = NA

final$Begin [which (final$Dendro_ID=="FaSy4"& final$Year_CE=="2013"& final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy4"& final$Year_CE=="2013"& final$Plot_abb=="SIL")] = NA

final$Begin [which (final$Dendro_ID=="FaSy5"& final$Year_CE=="2000"& final$Plot_abb=="GUE")] = NA
final$Period [which (final$Dendro_ID=="FaSy5"& final$Year_CE=="2000"& final$Plot_abb=="GUE")] = NA

final$Begin [which (final$Dendro_ID=="FaSy4"& final$Year_CE=="2000"& final$Plot_abb=="GUE")] = NA
final$Period [which (final$Dendro_ID=="FaSy4"& final$Year_CE=="2000"& final$Plot_abb=="GUE")] = NA


#remove completely:
#FaSy1, 2012,2011,2003,1997 SIL
#FaSy2, 2012, 2003, SIL
#FaSy3,2011, SIL
#FaSy5, 1997, SIL
final$End [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="2012"&final$Plot_abb=="SIL")] = NA
final$Begin [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="2012"& final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="2012"& final$Plot_abb=="SIL")] = NA

final$End [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="2011"&final$Plot_abb=="SIL")] = NA
final$Begin [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="2011"& final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="2011"& final$Plot_abb=="SIL")] = NA

final$End [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="2003"&final$Plot_abb=="SIL")] = NA
final$Begin [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="2003"& final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="2003"& final$Plot_abb=="SIL")] = NA

final$End [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="1997"&final$Plot_abb=="SIL")] = NA
final$Begin [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="1997"& final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy1"& final$Year_CE=="1997"& final$Plot_abb=="SIL")] = NA


final$End [which (final$Dendro_ID=="FaSy2"& final$Year_CE=="2012"&final$Plot_abb=="SIL")] = NA
final$Begin [which (final$Dendro_ID=="FaSy2"& final$Year_CE=="2012"& final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy2"& final$Year_CE=="2012"& final$Plot_abb=="SIL")] = NA

final$End [which (final$Dendro_ID=="FaSy2"& final$Year_CE=="2003"&final$Plot_abb=="SIL")] = NA
final$Begin [which (final$Dendro_ID=="FaSy2"& final$Year_CE=="2003"& final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy2"& final$Year_CE=="2003"& final$Plot_abb=="SIL")] = NA

final$End [which (final$Dendro_ID=="FaSy3"& final$Year_CE=="2011"&final$Plot_abb=="SIL")] = NA
final$Begin [which (final$Dendro_ID=="FaSy3"& final$Year_CE=="2011"& final$Plot_abb=="SIL")] = NA
final$Period [which (final$Dendro_ID=="FaSy3"& final$Year_CE=="2011"& final$Plot_abb=="SIL")] = NA

final$End [which (final$Dendro_ID=="FaSy5"& final$Year_CE=="1997"&final$Plot_abb=="SIL")] = NA
final$Begin [which (final$Dendro_ID=="FaSy5"& final$Year_CE=="1997"& final$Plot_abb=="GUE")] = NA
final$Period [which (final$Dendro_ID=="FaSy5"& final$Year_CE=="1997"& final$Plot_abb=="GUE")] = NA

#remove 2014 completely
final$End [which ( final$Year_CE=="2014")] = NA
final$Begin [which (final$Year_CE=="2014")] = NA
final$Period [which (final$Year_CE=="2014")] = NA

#end of plausibility check, 
#all senseless data removed from table



######################

##################

#only work with mixed stand trees

#######
#check first differences
library(ggplot2)
attach(final)
p <- ggplot(final, aes(factor(Plot_abb), Period))
b <- ggplot(final, aes(factor(Plot_abb), Begin))
e <- ggplot(final, aes(factor(Plot_abb), End))

qplot(factor(Plot_abb), Period, data = final, geom = "boxplot") +coord_flip()
qplot(factor(Plot_abb), Begin, data = final, geom = "boxplot") +coord_flip()
qplot(factor(Plot_abb), End, data = final, geom = "boxplot") +coord_flip()

#schickes ding: 
#farben:
#  http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/
#  http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf


#Period boxplot with rawdata
ggsave("periodboxplot.pdf")
p + geom_boxplot(aes(fill = factor(Plot_abb)))+ 
  scale_fill_manual(values=c( "forestgreen","indianred2", "steelblue2")) +
  labs(size= "Nitrogen",
       x = "Plots",
       y = "Growth Period [DOY]",
       title = " Growth Period")

ggsave("endboxplot.pdf")
e + geom_boxplot(aes(fill = factor(Plot_abb)))+ 
  scale_fill_manual(values=c("forestgreen","indianred2",  "steelblue2")) +
  labs(size= "Nitrogen",
       x = "Plots",
       y = "End of Growth Period [DOY]",
       title = "End of  Growth Period")

ggsave("beginboxplot.pdf")
b + geom_boxplot(aes(fill = factor(Plot_abb)))+ 
  scale_fill_manual(values=c("forestgreen", "indianred2", "steelblue2")) +
  labs(size= "Nitrogen",
       x = "Plots",
       y = "Begin of Growth Period [DOY]",
       title = "Begin of Growth Period")

#nur mixed stand:
mixed = final[Group=="Mixed_stand",]
##do again boxplots with mixed stand
pm <- ggplot(mixed, aes(factor(Plot_abb), Period))
bm <- ggplot(mixed, aes(factor(Plot_abb), Begin))
em <- ggplot(mixed, aes(factor(Plot_abb), End))

#library(plyr)
#meds <- ddply(mixed, .(factor(Plot_abb)), summarize, med = round(median(Period ,na.rm = TRUE)))
ggsave("periodmixedboxplot.pdf")
pm + geom_boxplot(aes(fill = factor(Plot_abb)))+ 
  scale_fill_manual(values=c("forestgreen", "indianred2", "steelblue2")) +
  labs(size= "Nitrogen",
       x = "Plots",
       y = "Growth Period [DOY]",
       title = "Growth Period in Mixed Stands")
#+  geom_text(data=meds, aes( y=med, label="med", size = 3, vjust = +1))  

ggsave("endmixedboxplot.pdf")
em + geom_boxplot(aes(fill = factor(Plot_abb)))+ 
  scale_fill_manual(values=c("forestgreen", "indianred2", "steelblue2")) +
  labs(size= "Nitrogen",
       x = "Plots",
       y = "End of Growth Period [DOY]",
       title = "Cessation of Growth Period in Mixed Stands")

ggsave("beginmixedboxplot.pdf")
bm + geom_boxplot(aes(fill = factor(Plot_abb)))+ 
  scale_fill_manual(values=c("forestgreen", "indianred2", "steelblue2")) +
  labs(size= "Nitrogen",
       x = "Plots",
       y = "Begin of Growth Period [DOY]",
       title = "Begin of Growth Period in Mixed Stands")

###
#check other possible differences: 

#1. on position: 
p + geom_boxplot(aes(fill = factor(Position)))

#2. on grouping:
ggsave("buntegroupings.pdf")
p + geom_boxplot(aes(fill = interaction(Group, Plot_abb))) + 
  scale_fill_manual(values=c("forestgreen", "indianred2", "steelblue2","tan1", "violetred2" )) +
  labs(size= "Nitrogen",
       x = "Plots",
       y = "Growth Period [DOY]",
       title = "Growth Period with different groupings")

ggsave("palegroupings.pdf")
p + geom_boxplot(aes(fill = interaction(Group, Plot_abb))) + 
  scale_fill_manual(values=c("palegreen2", "palegreen3", "palegreen4","tan1", "violetred2" )) +
  labs(size= "Nitrogen",
       x = "Plots",
       y = "Growth Period [DOY]",
       title = "Growth Period with different groupings")

##focused in SIL: 
ggsave("buntegroupings.pdf")
SIL = final[final$Plot_abb =="SIL",]
pSIL <- ggplot(SIL, aes(factor(Group), Period))

ggsave("SILgrouping_othercolour.pdf")
pSIL + geom_boxplot(aes(fill = (Group))) + 
  scale_fill_manual(values=c("forestgreen", "tan1",  "violetred2" )) +
  labs(size= "Nitrogen",
       x = "Plots",
       y = "Growth Period [DOY]",
       title = "Growth Period with different groupings")


#3. Exposition: 
ggsave("exposition.pdf")
p + geom_boxplot(aes(fill = Exposition))+ 
  scale_fill_manual(values=c("steelblue2", "indianred2","palegreen2")) +
  labs(size= "Nitrogen",
       x = "Plots",
       y = "Growth Period [DOY]",
       title = "Growth Period along different Expositions")


#####################

#Data checking for assupmtions

#normal distribution, shapiro p smaller 0.05, dann reject NULL = keine Normalverteilung
attach(final)
write.csv(final, file="final.csv")

detach(final)
attach(mixed)
par(mfrow=c(1,3))
qqnorm(Period, main="Period")
qqline(Period)
shapiro.test(Period) 

qqnorm(Begin, main="Begin")
qqline(Begin)
shapiro.test(Begin)
#not normally distributed! log transform does not help

qqnorm(log(mixed$Begin))
qqline(log(mixed$Begin))
shapiro.test(log(mixed$Begin))

qqnorm(End, main="End")
qqline(End)
shapiro.test(End)
par(mfrow=c(1,1))
mtext("Mixed Stand Values", side=3, line= 3)
######################

#Checking first statistics
#overall means
mean_begin = mean(Begin)
mean_end = mean(End)
mean_period = mean(Period)

#means with different altitudes
mean_3_period <- tapply(final$Period, final$Plot_abb, mean, na.rm=TRUE)
mean_3_period

mean_3_beg<- tapply(final$Begin, final$Plot_abb, mean, na.rm=TRUE)
mean_3_beg

mean_3_end <- tapply(final$End, final$Plot_abb, mean, na.rm=TRUE)
mean_3_end

######################

#bootstrapping

#predict and simulate data

#check model

#do posthoc test

#####################

# group trees in to 3 plots 
test = lmer (Period ~ Plot_abb + (1|Year_CE) + (1|Dendro_ID))
plot(test)


#eigentlich brauche ich ja 3 modelle?? 
#predict anfang, ende und länge? 
#dann daten plotten 

test1 <- lme(Period ~ Plot_abb, random=~1|Year_CE, data = final)
plot(test1)

##################################

#look for outliers: done by visualizing and makign a table "outlier"


