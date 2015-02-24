#load in results
Results <- read.csv("~/GitHub/essay/Growth Phenology/Results.csv", sep=";", dec=",")

attach(Results)

#fixed effect will be Plot and tree ID ? 

#random effect will be: year, groups and/or position

#only select fagus sylavatica in tree species

#age trend will be noticed in discussion but not included in model ( only 20 years )

#nested: year then groups then position? maybe groups and position are interacting and only need one

#generalized linear mixed model 

detach(Results)
workingdata1= Results[Position =="dbh",]
attach(workingdata1)
workingdata2 = workingdata1[Tree_species =="Fagus_sylvatica",]
detach(workingdata1)
attach(workingdata2)
workingdata2 = workingdata2[,-c(9,10,11,12),]


final = na.omit(workingdata2)
##alright, alles weg was man nicht brauch
##nicht plausibele ergebnisse immer noch drin.. 

search()
detach(workingdata2)
attach(final)

##################

#only work with mixed stand trees
final = final[Group=="Mixed_stand",]

#######
#check first differences

p <- ggplot(final, aes(factor(Plot_abb), Period))

qplot(factor(Plot_abb), Period, data = final, geom = "boxplot") +
  coord_flip()

qplot(factor(Plot_abb), Begin, data = final, geom = "boxplot") +
  coord_flip()

qplot(factor(Plot_abb), End, data = final, geom = "boxplot") +
  coord_flip()

p + geom_boxplot(aes(fill = factor(Plot_abb)))

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

qqnorm(Period)
qqline(Period)
shapiro.test(Period)

qqnorm(Begin)
qqline(Begin)
shapiro.test(Begin)

qqnorm(End)
qqline(End)
shapiro.test(End)

######################

#Checking first statistics
#overall means
mean_begin = mean(Begin)
mean_end = mean(End)
mean_period = mean(Period)

#means with different altitudes
mean_3_period <- tapply(final$Period, final$Plot_abb, mean)
mean_3_period

mean_3_beg<- tapply(final$Begin, final$Plot_abb, mean)
mean_3_beg

mean_3_end <- tapply(final$End, final$Plot_abb, mean)
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

