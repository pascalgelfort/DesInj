load("dsreg16052022.Rda")
load("dsjena17062022.Rda")
#####################################
####Packages#########################
#####################################
library(plyr)
library(dplyr)
library(tidyr)
library(gdata)
library(readxl)
library(psych)
library(lmerTest)
library(performance)
library(multilevel)
library(DescTools)
library(cluster)
library(data.table)
#####################################
Targets<-read_xlsx("Targets.xlsx")
which(colnames(dsjena)=="RW01_01")
jena<-dsjena[,c(9,10,11,13,15,16,18,20:80,81:141,142:202,203:263,289:349,
                350:410,411:413,264:279,280:288,445)]
regensburg<-dsreg[,c(9,11,12,14,16,17,19,21:81,82:142,143:203,204:264,290:350,
                     351:411,412:414,265:280,281:289,442)]
a<-rbind(jena,regensburg)
######Datenaufbereitung####

######Entfernen Datens?tze####

######Zählen NAs pro Bedingung#####
#1 = Injunktiv Maskulin
#2 = Injunktiv Gender
#3 = Vorurteil Maskulin
#4 = Vorurteil Gender
#5 = Deskriptiv Maskulin
#6 = Deskriptiv Gender
a$IM<-ifelse(a$FU03==1,a$IM<-apply(a[252:312], MARGIN = 1, function(x) sum(is.na(x))),NA)
a$IG<-ifelse(a$FU03==2,a$IG<-apply(a[313:373], MARGIN = 1, function(x) sum(is.na(x))),NA)
a$PM<-ifelse(a$FU03==3,a$PM<-apply(a[130:190], MARGIN = 1, function(x) sum(is.na(x))),NA)
a$PG<-ifelse(a$FU03==4,a$PG<-apply(a[191:251], MARGIN = 1, function(x) sum(is.na(x))),NA)
a$DM<-ifelse(a$FU03==5,a$DM<-apply(a[8:68], MARGIN = 1, function(x) sum(is.na(x))),NA)
a$DG<-ifelse(a$FU03==6,a$DG<-apply(a[69:129], MARGIN = 1, function(x) sum(is.na(x))),NA)
a$NAs <- as.numeric(paste(a$NAs, coalesce(a$IM,a$IG,a$PM,a$PG,a$DM,a$DG)))
a<-a[,-c(403:408)]
table(a$NAs)

####NAs und RSI aussortieren####
a<-subset(a,a$NAs==0)
a<-subset(a,a$TIME_RSI<2)

####Skalenmittelwerte####
a$RW01_01<-ifelse(a$RW01_01==-9,NA,a$RW01_01)
a$RW01_02<-ifelse(a$RW01_02==-9,NA,a$RW01_02)
a$RW01_03<-ifelse(a$RW01_03==-9,NA,a$RW01_03)
a$RW01_04<-ifelse(a$RW01_04==-9,NA,a$RW01_04)
a$RW01_05<-ifelse(a$RW01_05==-9,NA,a$RW01_05)
a$RW01_06<-ifelse(a$RW01_06==-9,NA,a$RW01_06)
a$RW01_07<-ifelse(a$RW01_07==-9,NA,a$RW01_07)
a$RW01_08<-ifelse(a$RW01_08==-9,NA,a$RW01_08)
a$RW01_09<-ifelse(a$RW01_09==-9,NA,a$RW01_09)

a$SD01_01<-ifelse(a$SD01_01==-9,NA,a$SD01_01)
a$SD01_02<-ifelse(a$SD01_02==-9,NA,a$SD01_02)
a$SD01_03<-ifelse(a$SD01_03==-9,NA,a$SD01_03)
a$SD01_04<-ifelse(a$SD01_04==-9,NA,a$SD01_04)
a$SD01_05<-ifelse(a$SD01_05==-9,NA,a$SD01_05)
a$SD01_06<-ifelse(a$SD01_06==-9,NA,a$SD01_06)
a$SD01_07<-ifelse(a$SD01_07==-9,NA,a$SD01_07)
a$SD01_08<-ifelse(a$SD01_08==-9,NA,a$SD01_08)
a$SD01_09<-ifelse(a$SD01_09==-9,NA,a$SD01_09)
a$SD01_10<-ifelse(a$SD01_10==-9,NA,a$SD01_10)
a$SD01_11<-ifelse(a$SD01_11==-9,NA,a$SD01_11)
a$SD01_12<-ifelse(a$SD01_12==-9,NA,a$SD01_12)
a$SD01_13<-ifelse(a$SD01_13==-9,NA,a$SD01_13)
a$SD01_14<-ifelse(a$SD01_14==-9,NA,a$SD01_14)
a$SD01_15<-ifelse(a$SD01_15==-9,NA,a$SD01_15)
a$SD01_16<-ifelse(a$SD01_16==-9,NA,a$SD01_16)

####RWA
which(colnames(a) == "SD01_16")
a$authaggression<-((a$RW01_01+a$RW01_02+a$RW01_03)/3)
a$authaggression<-ifelse(a$authaggression==-9,NA,a$authaggression)
a$authsubmission<-((a$RW01_04+a$RW01_05+a$RW01_06)/3)
a$authsubmission<-ifelse(a$authsubmission==-9,NA,a$authsubmission)
a$conventionalism<-((a$RW01_07+a$RW01_08+a$RW01_09)/3)
a$conventionalism<-ifelse(a$conventionalism==-9,NA,a$conventionalism)
a$RWA<-((a$authaggression+a$authsubmission+a$conventionalism)/3)
a$RWA<-ifelse(a$RWA==-9,NA,a$RWA)
hist(a$RWA)
describe(a$RWA)
alpha(a[,393:395])
alpha(a[,396:398])
alpha(a[,399:401])

alpha(a[,393:401])

####SDO
#rekodieren
a$SD01_09<-7-a$SD01_09
a$SD01_10<-7-a$SD01_10
a$SD01_11<-7-a$SD01_11
a$SD01_12<-7-a$SD01_12
a$SD01_13<-7-a$SD01_13
a$SD01_14<-7-a$SD01_14
a$SD01_15<-7-a$SD01_15
a$SD01_16<-7-a$SD01_16

a$SDO<-rowMeans(a[,377:392])
hist(a$SDO)
describe(a$SDO)
alpha(a[,377:392])

####Identifikation
a$Ident<-rowMeans(a[,374:376])
describe(a$Ident)
hist(a$Ident)
alpha(a[,374:376])
#1 = Injunktiv Maskulin
#2 = Injunktiv Gender
#3 = Vorurteil Maskulin
#4 = Vorurteil Gender
#5 = Deskriptiv Maskulin
#6 = Deskriptiv Gender
ainjmask<-subset(a,a$FU03==1)
ainjgen<-subset(a,a$FU03==2)
aprejmask<-subset(a,a$FU03==3)
aprejgen<-subset(a,a$FU03==4)
adeskmask<-subset(a,a$FU03==5)
adeskgen<-subset(a,a$FU03==6)
ainjmask<-ainjmask[,-c(8:251,313:373,374:403)]
colnames(ainjmask)<-c("cond","age","gender","edu","ideology","occupation","party",Targets$Targets,
                      "authagg","authsub","conv","RWA","SDO","Ident")
ainjgen<-ainjgen[,-c(8:312,374:403)]
colnames(ainjgen)<-c("cond","age","gender","edu","ideology","occupation","party",Targets$Targets,
                     "authagg","authsub","conv","RWA","SDO","Ident")
aprejmask<-aprejmask[,-c(8:129,191:373,374:403)]
colnames(aprejmask)<-c("cond","age","gender","edu","ideology","occupation","party",Targets$Targets,
                      "authagg","authsub","conv","RWA","SDO","Ident")
aprejgen<-aprejgen[,-c(8:190,252:373,374:403)]
colnames(aprejgen)<-c("cond","age","gender","edu","ideology","occupation","party",Targets$Targets,
                      "authagg","authsub","conv","RWA","SDO","Ident")
adeskmask<-adeskmask[,-c(69:373,374:403)]
colnames(adeskmask)<-c("cond","age","gender","edu","ideology","occupation","party",Targets$Targets,
                       "authagg","authsub","conv","RWA","SDO","Ident")
adeskgen<-adeskgen[,-c(8:68,130:373,374:403)]
colnames(adeskgen)<-c("cond","age","gender","edu","ideology","occupation","party",Targets$Targets,
                      "authagg","authsub","conv","RWA","SDO","Ident")
d<-rbind.data.frame(ainjmask,ainjgen,adeskmask,adeskgen,aprejmask,aprejgen)
d$ID<-seq(1:nrow(d))
d<-d[,c(75,1:74)]
d$condition<-ifelse(d$cond==1 | d$cond==2,"injunctive",
                    ifelse(d$cond==3|d$cond==4,"prejudice",
                           ifelse(d$cond==5|d$cond==6,"descriptive",NA)))
d$grammar<-ifelse(d$cond==1|d$cond==3|d$cond==5,"masculine","gender")
dlong<-pivot_longer(d,cols = 9:69,names_to = "Targets",values_to = "Rating")
