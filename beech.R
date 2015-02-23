#load in results
Results <- read.csv("~/GitHub/essay/Growth Phenology/Results.csv", sep=";", dec=",")

attach(Results)

#fixed effect will be Plot and tree ID ? 

#random effect will be: year, groups and/or position

#only select fagus sylavatica in tree species

#age trend will be noticed in discussion but not included in model ( only 20 years )

#nested: year then groups then position? maybe groups and position are interacting and only need one

#generalized linear mixed model 
