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

final$End [which (final$Dendro_ID=="FaSy6"& final$Year_CE=="2013"& final$Plot_abb=="GUE")] = NA
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

qplot(factor(Plot_abb), Period, data = final, geom = "boxplot") +
  coord_flip()

qplot(factor(Plot_abb), Begin, data = final, geom = "boxplot") +
  coord_flip()

qplot(factor(Plot_abb), End, data = final, geom = "boxplot") +
  coord_flip()

#schickes ding: 
#Period boxplot with rawdata
p + geom_boxplot(aes(fill = factor(Plot_abb)))
e + geom_boxplot(aes(fill = factor(Plot_abb)))
b + geom_boxplot(aes(fill = factor(Plot_abb)))

#nur mixed stand:
mixed = final[Group=="Mixed_stand",]
##do again boxplots with mixed stand
pm <- ggplot(mixed, aes(factor(Plot_abb), Period))
bm <- ggplot(mixed, aes(factor(Plot_abb), Begin))
em <- ggplot(mixed, aes(factor(Plot_abb), End))

#library(plyr)
#meds <- ddply(mixed, .(factor(Plot_abb)), summarize, med = round(median(Period ,na.rm = TRUE)))

pm + geom_boxplot(aes(fill = factor(Plot_abb)))
#+  geom_text(data=meds, aes( y=med, label="med", size = 3, vjust = +1))                                                           
em + geom_boxplot(aes(fill = factor(Plot_abb)))
bm + geom_boxplot(aes(fill = factor(Plot_abb)))
###
#check other possible differences: 

#1. on position: 
p + geom_boxplot(aes(fill = factor(Position)))

#2. on grouping:
p + geom_boxplot(aes(fill = factor(Group)))

#3. Exposition: 
p + geom_boxplot(aes(fill = Exposition))
#####################

#Data checking for assupmtions

#normal distribution, shapiro p smaller 0.05, dann reject NULL = keine Normalverteilung
attach(final)
write.csv(final, file="final.csv")
par(mfrow=c(1,3))
qqnorm(Period, main="Period")
qqline(Period)
shapiro.test(Period)

qqnorm(Begin, main="Begin")
qqline(Begin)
shapiro.test(Begin)

qqnorm(End, main="End")
qqline(End)
shapiro.test(End)
par(mfrow=c(1,1))
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
test = lmer (Period ~ Plot_abb + (1|Year_CE)+ (1|Dendro_ID))
plot(test)


#eigentlich brauche ich ja 3 modelle?? 
#predict anfang, ende und länge? 
#dann daten plotten 

test1 <- lme(Period ~ Plot_abb, random=~1|Year_CE, data = final)
plot(test1)

##################################

#look for outliers: 

