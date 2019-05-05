# R-Codes
These codes are valid data analyses in R version 3.4.1
*******************import data set********************************************************
library(haven)
IAIR74FL <- read_dta("C:/Users/DELL/Desktop/tobacco_nfhs-4 materials/IAIR74FL.DTA")
View(IAIR74FL)

data.frame(v012)

**********************generatlinbg new variables******************************************
attach(IAIR74FL)

IAIR74FL$sum<-News_Mag+Radio+TV
IAIR74FL$sum<-(News_Mag+Radio+TV)/3
detach(IAIR74FL)

IAIR74FL<-transform(IAIR74FL)
sum(y1+y2)
mean(y1+y2)/2

********************************recoding into new varible**************************************
####v012 in 7 categories
attach(IAIR74FL)
IAIR74FL$agecat[v012<=19]<-"15-19"
IAIR74FL$agecat[v012>19 & v012<=24]<-"20-24"
IAIR74FL$agecat[v012>24 & v012<=29]<-"25-29"
IAIR74FL$agecat[v012>29 & v012<=34]<-"30-34"
IAIR74FL$agecat[v012>34 & v012<=39]<-"35-39"
IAIR74FL$agecat[v012>39 & v012<=44]<-"40-44"
IAIR74FL$agecat[v012>44 & v012<=49]<-"45-49"
detach(IAIR74FL)

*********************************renaming variables*********************************************
library(reshape)
IAIR74FL<rename(IAIR74FL, c(v012="age"))
names(IAIR74FL)<-c("v012","age","v013","ageg")

**********************************2-way cross-tab************************************************************
 t<-table(v106,v157)
 t
 margin.table(t,1)
 margin.table(t,2)
 prop.table(t)---------------cell proportion/percent
 prop.table(t,1)............row proportion/percent
 prop.table(t,2)............column proportion/percent
 
 **********************************3-way tab*****************************************************************
 t<-table(v106,v157,v158)
 ftable(t)
 
 t<-xtabs(~v106+v157+v158, data=IAIR74FL).....................(function xtab() is used in programmitic sense)
 ftable(t) 
 
 
CrossTable() function in gmodels
library(gmodels)
CrossTable(v106, v157, prop.r=FALSE,prop.c=FALSE, prop.t=TRUE,prop.chisq=TRUE)
CrossTable(v106, v157, prop.r=FALSE,prop.c=FALSE, prop.t=TRUE,prop.chisq=FALSE)

CrossTable(v106, v157, prop.r=TRUE,prop.c=FALSE, prop.t=FALSE,prop.chisq=FALSE)

******************************test of independence Chi-square****************************************************
summary(t)
chisq.test(t)
fisher.test(t) -exact test of independence

*****************************measures of association*************************************************************
library(vcd)
assocstats(t)
kappa(t)

**************************data visualiztion*********************************************************************
plot(v106,v157)
library(vcd)
mosaic(v106)

require(ggplot2)
hist(IAIR74FL$ageg, main="Age",xlab="age-group")
hist(IAIR74FL$ageg, main="Age",xlab="age-group", col="green")
plot(v106~v157, data=IAIR74FL)
or  plot(IAIR74FL$v106, IAIR74FL$v157)
boxplot(IAIR74FL$v106)

**************************predictive modeling********************************************************************
 ######Linear regression#############

head(final_dataset)

head(final_dataset,30)

require(UsingR)

require(ggplot2)
head(final_dataset)
ggplot(final_dataset, aes(x=v517, y=v518)) +geom_point()+
+ geom_smooth(method="lm")+labs(x="news", y="radio")

LM<-lm(v518~v517, data=final_dataset)
LM

summary(LM)
confint(LM)





 
