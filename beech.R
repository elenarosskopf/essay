
install.packages("AER")
library("AER")
install.packages("effects")
library("effects")
install.packages("caret")
library("caret")
install.packages("nlme")
library("nlme")
install.packages("lme4")
library("lme4")
install.packages("ggplot2")
library("ggplot2")
install.packages("forecast")
library("forecast")
install.packages("multcomp")
library("multcomp")
library("languageR")
library("LMERConvenienceFunctions")
library("coda")
library("pbkrtest")
library("lmerTest")

#load in results


Results <- read.csv("~/GitHub/essay/Growth Phenology/Results.csv", sep=";", dec=",")
attach(Results)

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
workingdata2 = workingdata2[,-c(1,4,9,10,11,12),]


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

#remove 2014 data
final$End [which (final$Year_CE=="2014")] = NA
final$Begin [which (final$Year_CE=="2014")] = NA
final$Period [which (final$Year_CE=="2014")] = NA

# OR do this: 
#remove rows of 2014 completely to remove NAs
final = final [!(final$Year_CE=="2014"),]


#end of plausibility check, 
#all senseless data removed from table

######################
detach(final)
attach(final)
##################
#add factors to the dataset: factor year
final$fyear = factor(final$Year_CE)
final$fgroup = factor(final$Group)

detach(final)
attach(final)
#only work with mixed stand trees
#nur mixed stand:
mixed = final[Group=="Mixed_stand",]

period = final[,-c(4,8,9)]
period= na.omit(period)
period = period[c(1:7,11,12)]
period_mixed = period[Group=="Mixed_stand",]
period_mixed = na.omit(period_mixed)

#normal distribution, shapiro p smaller 0.05, dann reject NULL = keine Normalverteilung
write.csv(final, file="final.csv")
attach(final)
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

##start with simple model
#DOY as response Y variable (begin, end) and period as day (count)
#B and E: is it either gaussain data 
#Period: count data = poisson ? or gauss? 

b1 = lm(Begin ~ Plot_abb)
#try to include year as a random effect: 
p1 = lme (Period ~ Plot_abb, random = ~1|Year_CE, data = final,na.action = na.exclude)
summary(p1)

p2 = lme (Period ~ Plot_abb, random = ~1|fyear, data = final,na.action = na.exclude)
summary(p2)
fittedp2 = fitted(p2, level=0)
fittedp2_1 = fitted(p2, level=1) 
#level 0 or level 1 depend wether i want the fitted values being obtained from the model or from the within-plot values

#include the interaction term 
p3 = lme( Period ~ Plot_abb, random = ~1 + Plot_abb | fyear, data = final, na.action=na.omit)
#possible not ! data sucks! iteration limit reached , no df left? 
#try without ~1 intercept
p3 = lme( Period ~ Plot_abb, random = ~ Plot_abb | fyear, data = final, na.action=na.exclude)
#not possible to try this.. no df left

#random effects model: 
p4 = lme(Period ~1 , random =~1 | fyear, data=final, na.action=na.omit)
summary(p4)
#marginal model, gls: 
p5 = lme(Period ~ Plot_abb, random = ~1|fyear, method="REML", data=final, na.action=na.omit)
summary(p5)
#try gls with correltation structure
p6 = gls( Period ~ Plot_abb, method="REML", correlation = corCompSymm(form = ~1| fyear), data=final, na.action= na.exclude)
summary(p6)
#add the dendro_ID as a random effect 
p7 = lmer (Period ~ Plot_abb + (1|Year_CE)+ (1|Dendro_ID))
summary(p7)
#make it not REML but normal max likelihood
p8 = lmer (Period ~ Plot_abb + (1|Year_CE)+ (1|Dendro_ID), REML=0)
summary (p8)


detach(final)
attach(final)

p11 = lmer(Period ~ Plot_abb + (1| Year_CE) + (1| Plot_abb/Dendro_ID),  data=period) #needed to remove plot in the random effect structure
AIC(p11)

p12 = lmer(Period ~ Plot_abb + (1| Year_CE) + (1| Plot_abb/Dendro_ID) + (1|Plot_abb/fgroup),  data=final) #nested design with trees within plots and groups within plots.. model failed to converge
AIC(p12)

p13 = lmer(Period ~ Plot_abb + (1| Year_CE) + (1| Plot_abb/fgroup/Dendro_ID),  data=final)
#nested design with trees within the groups within the plots .. need to rescale.. what? all variables? 
AIC(p13)

################################# focus on mixed stand ( so factor "group" gets unused) and only focus on period ( I could do a loop for begin and end ), so use the subset period_mixed instead of final 
p14 = lmer(Period ~ Plot_abb + (Plot_abb|fyear) + (1| Plot_abb/Dendro_ID),  data=period_mixed) 
#bestmodel
AIC(p14)

p14a = lmer(Period ~ Plot_abb +(fyear|Plot_abb) + (1|fyear) +(1|Plot_abb/Dendro_ID),  data=period_mixed) 
AIC(p14a)
AIC(p14)
anova(p14a,p14)
#(1| Plot_abb/Group/Dendro_ID) random interscept

#(Plot_abb| Year_CE) better fit than (Year_CE |Plot_abb):
# the years differ in importnace to the plot from plot to plot ( e.g. 2003 is more important on the lowest plot site due to drought stress effects even if I have no climate data present in my dataset) ? or is it that the years are increasing or decreasing from plot to plot? this would then make no sense.. 

#so normally I should group the years also to get a clearer picture.. 
### random slope for effect of plot 
#p14 significant besser


fullModelGLMM = lmer(Period ~ Plot_abb + Year_CE + I(Year_CE^2)+  (1|fyear) + (1|Plot_abb/Dendro_ID), data = period_mixed, na.action="na.fail")
AIC(fullModelGLMM)
#p14 immer noch besser

submodelsGLMM <- dredge(fullModelGLMM)
print(submodels[1:10])


bestGLMM <- get.models(submodelsGLMM, subset = 1)[[1]]
summary(bestGLMM)
r.squaredGLMM(bestGLMM)
#so this is pretty nice, so my marginal R2 with just the fixed effect can only explain 30% of my variance but the conditional R2 can explain nearly 90% of the variance by incluing the random effects 
bestGLMM

period_mixed_scaled <- period_mixed
period_mixed_scaled[,numcols] <- scale(period_mixed_scaled[,numcols])
bestGLMM_scaled <- update(bestGLMM,data=period_mixed_scaled)

#try with gls due to possible temporal autocorrelation 
p15 = gls(Period ~ Plot_abb + Year_CE + Plot_abb*Year_CE, data = period_mixed, correlation = corAR1(form = ~ Year_CE | Plot_abb/Dendro_ID))
AIC(p15)
fullModelGLS = gls(Period ~ Plot_abb + Year_CE + I(Year_CE^2) +Plot_abb*Year_CE, data = period_mixed, correlation = corAR1(form = ~ Year_CE|Plot_abb/Dendro_ID))
submodelsGLS <- dredge(fullModelGLS)
print(submodels[1:10])
bestGLS <- get.models(submodelsGLS, subset = 1)[[1]]
summary(bestGLS)
r.squaredLR(bestGLS)
#only around 50% of the variance can be explained by the model .. but other structure ! this model includes Year and the interaction in the model as a fixed effect and includes a correlation structure ARMA ( moving average and the autocorrelation) ARMA(1,0)

#ok I will use the bestGLMM: 

#check for temporal correlation:
par(mfrow=c(1,1))
plot(period_mixed$Year_CE, residuals(bestGLMM))
plot(period_mixed$Year_CE, residuals(bestGLS))

acf(residuals(bestGLMM))
pacf(residuals(bestGLMM))
#ok should be alright
plot(bestGLMM)
hist(residuals(bestGLMM))
#hm rechtsschief.. 
#rescale variables? 

shapiro.test(residuals(bestGLMM))
#nice =) normality check
--> parametric post hoc test



mcp.fnc(bestGLMM)  

#remove outliers
rm.outliers <- romr.fnc(bestGLMM, period_mixed, trim=2.5)
period_mixed <- rm.outliers$data
bestGLMM <- update(bestGLMM)

# re-check model assumptions
mcp.fnc(bestGLMM)

# compare model to null model
bestGLMMnull <- lmer(Period ~ 1 + (1 | fyear) + (1 | Plot_abb/Dendro_ID), data=period_mixed)
region.krtest <- KRmodcomp(bestGLMM, bestGLMMnull)

# output lmer summary
bestGLMsummary <- summary(bestGLMM)

# run post hoc tests
anova(bestGLMM)
var = list(ph1 = c("Plot_abb"))
mcposthoc.fnc(bestGLMM, var, two.tailed = TRUE, mcmc = FALSE, nsim = 10000, ndigits = 4, mc.cores = 1)


##############
well as I understood now: 
  we have significant differences between SIL and HBR and SIL and GUE but no siginificant difference between GUE and HBR






se <- sqrt(diag(vcov(bestGLMM)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(bestGLMM), LL = fixef(bestGLMM) - 1.96 * se, UL = fixef(bestGLMM) + 1.96 *
                se))
# creating the prediction frame
newdata <- with(period_mixed, expand.grid(Plot_abb=unique(Plot_abb),
                                          fyear=unique(fyear),
                                   Dendro_ID=unique(Dendro_ID)
                                    ))
# calculating predictions
newdata$pred <- predict(bestGLMM, newdata, allow.new.levels =T)
boxplot(newdata$pred ~ newdata$Plot_abb)





#########################################

#eigentlich brauche ich ja 3 modelle?? 
#predict anfang, ende und länge? 
#dann daten plotten 


#same for begin
begin = final[,-c(4,9,10)]
begin= na.omit(begin)
begin = begin[c(1:7,11,12)]
begin_mixed = begin[Group=="Mixed_stand",]
begin_mixed = na.omit(begin_mixed)


################################# focus on mixed stand ( so factor "group" gets unused) and only focus on period ( I could do a loop for begin and end ), so use the subset period_mixed instead of final 
p14 = lmer(Begin ~ Plot_abb + (Plot_abb|fyear) + (1| Plot_abb/Dendro_ID),  data=begin_mixed) 
#bestmodel
AIC(p14)

p14a = lmer(Begin ~ Plot_abb +(fyear|Plot_abb) + (1|fyear) +(1|Plot_abb/Dendro_ID),  data=begin_mixed) 
AIC(p14a)
AIC(p14)

fullModelGLMM_begin = lmer(Begin ~ Plot_abb + Year_CE + I(Year_CE^2)+  (1|fyear) + (1|Plot_abb/Dendro_ID), data = begin_mixed, na.action="na.fail")
AIC(fullModelGLMM)
#p14 immer noch besser

submodelsGLMM_begin <- dredge(fullModelGLMM_begin)
print(submodels[1:10])

bestGLM_begin <- get.models(submodelsGLMM_begin, subset = 1)[[1]]
summary(bestGLM_begin)
r.squaredGLMM(bestGLM_begin)
#so this is pretty nice, so my marginal R2 with just the fixed effect can only explain 30% of my variance but the conditional R2 can explain nearly 90% of the variance by incluing the random effects 
bestGLM_begin

begin_mixed_scaled <- begin_mixed
begin_mixed_scaled[,numcols] <- scale(begin_mixed_scaled[,numcols])
bestGLM_begin_scaled <- update(bestGLM_begin,data=begin_mixed_scaled)

#try with gls due to possible temporal autocorrelation 
p15 = gls(Period ~ Plot_abb + Year_CE + Plot_abb*Year_CE, data = period_mixed, correlation = corAR1(form = ~ Year_CE | Plot_abb/Dendro_ID))
AIC(p15)
fullModelGLS = gls(Period ~ Plot_abb + Year_CE + I(Year_CE^2) +Plot_abb*Year_CE, data = period_mixed, correlation = corAR1(form = ~ Year_CE|Plot_abb/Dendro_ID))
submodelsGLS <- dredge(fullModelGLS)
print(submodels[1:10])
bestGLS <- get.models(submodelsGLS, subset = 1)[[1]]
summary(bestGLS)
r.squaredLR(bestGLS)
#only around 50% of the variance can be explained by the model .. but other structure ! this model includes Year and the interaction in the model as a fixed effect and includes a correlation structure ARMA ( moving average and the autocorrelation) ARMA(1,0)

#ok I will use the bestGLM_begin: 

#check for temporal correlation:
par(mfrow=c(1,1))
plot(begin_mixed$Year_CE, residuals(bestGLM_begin))
plot(begin_mixed$Year_CE, residuals(bestGLS))

acf(residuals(bestGLM_begin))
pacf(residuals(bestGLM_begin))
#ok should be alright
plot(bestGLM_begin)
hist(residuals(bestGLM_begin))
#hm rechtsschief.. 
#rescale variables? 

shapiro.test(residuals(bestGLM_begin))
#nice =) normality check
--> parametric post hoc test

mcp.fnc(bestGLM_begin)  

#remove outliers
rm.outliers <- romr.fnc(bestGLM_begin, begin_mixed, trim=2.5)
begin_mixed <- rm.outliers$data
bestGLM_begin <- update(bestGLM_begin)

# re-check model assumptions
mcp.fnc(bestGLM_begin)

# compare model to null model
bestGLM_beginnull <- lmer(Period ~ 1 + (1 | fyear) + (1 | Plot_abb/Dendro_ID), data=begin_mixed)
region.krtest <- KRmodcomp(bestGLM_begin, bestGLM_beginnull)

# output lmer summary
bestGLMsummary <- summary(bestGLM_begin)

# run post hoc tests
anova(bestGLM_begin)
var = list(ph1 = c("Plot_abb"))
mcposthoc.fnc(bestGLM_begin, var, two.tailed = TRUE, mcmc = FALSE, nsim = 10000, ndigits = 4, mc.cores = 1)


##############
well as I understood now: 
  we have significant differences between SIL and HBR and SIL and GUE but no siginificant difference between GUE and HBR

se <- sqrt(diag(vcov(bestGLM_begin)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(bestGLM_begin), LL = fixef(bestGLM_begin) - 1.96 * se, UL = fixef(bestGLM_begin) + 1.96 *
                se))
# creating the prediction frame
newdata_begin <- with(begin_mixed, expand.grid(Plot_abb=unique(Plot_abb),
                                          fyear=unique(fyear),
                                          Dendro_ID=unique(Dendro_ID)
))
# calculating predictions
newdata_begin$pred <- predict(bestGLM_begin, newdata_begin, allow.new.levels =T)
boxplot(newdata_begin$pred ~ newdata_begin$Plot_abb)

