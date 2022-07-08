###########################################
###########Analysen########################
###########################################

####Korrelation grammar cond####
meansgrammar<-by(dlong$Rating,list(dlong$Targets,dlong$grammar),mean)
corr.test(meansgrammar)
#.99

####Mittelwerte pro Target und Bedingung####
means<-by(dlong$Rating,list(dlong$Targets,dlong$condition),mean)
#Korrelation Vorurteile und Normen 
corr.test(means)

####Korrelation SDO RWA####
#Vorurteile
dprej<-subset(d,d$condition=="prejudice")
RWAPrej<-corr.test(d[,c(73,9:69)])
RWAPrej$r[,1]
SDOPrej<-corr.test(d[,c(74,9:69)])
SDOPrej$r[,1]
#injunktive Normen
dinj<-subset(d,d$condition=="injunctive")
RWAInj<-corr.test(d[,c(73,9:69)])
RWAInj$r[,1]
SDOInj<-corr.test(d[,c(74,9:69)])
SDOInj$r[,1]
#deskriptive Normen
ddesc<-subset(d,d$condition=="descriptive")
RWADesc<-corr.test(d[,c(73,9:69)])
RWADesc$r[,1]
SDODesc<-corr.test(d[,c(74,9:69)])
SDODesc$r[,1]

####MultiLevel Versuche####
#ICC als m?glicher Indikator f?r ?hnlichkeit ??????
longprej<-subset(dlong,dlong$condition=="prejudice")
longinj<-subset(dlong,dlong$condition=="injunctive")
longdesc<-subset(dlong,dlong$condition=="descriptive")

m1<-lmerTest::lmer(Rating~+(1|Targets),data = longprej)
summary(m1)
icc(m1)

m1b<-lmerTest::lmer(Rating~1+(1|Targets),data = longprej)
summary(m1b)
icc(m1b)
anova(m1b,m1)

m2<-lmerTest::lmer(Rating~1+(1|Targets),data = longinj)
summary(m2)
icc(m2)

m3<-lmerTest::lmer(Rating~1+(1|Targets),data = longdesc)
summary(m3)
icc(m3)

####Person Population Correlation####
ppcprej<-cbind.data.frame(longprej$ID,longprej$Rating,longprej$Targets)
ppcprej<-pivot_wider(ppcprej,id_cols = "longprej$Targets",names_from="longprej$ID",values_from = "longprej$Rating")
ppcprej<-ppcprej[,-1]
ppcprej<-ppcprej[ , colSums(is.na(ppcprej)) == 0]
ppcprejITC<-item.total(ppcprej)
ppcprejITC$Item.Total<-fisherz(ppcprejITC$Item.Total)
FisherZInv(mean(ppcprejITC$Item.Total))


ppcinj<-cbind.data.frame(longinj$ID,longinj$Rating,longinj$Targets)
ppcinj<-pivot_wider(ppcinj,id_cols = "longinj$Targets",names_from="longinj$ID",values_from = "longinj$Rating")
ppcinj<-ppcinj[,-1]
ppcinj<-ppcinj[ , colSums(is.na(ppcinj)) == 0]
ppcinjITC<-item.total(ppcinj)
ppcinjITC$Item.Total<-fisherz(ppcinjITC$Item.Total)
FisherZInv(mean(ppcinjITC$Item.Total))

ppcdesc<-cbind.data.frame(longdesc$ID,longdesc$Rating,longdesc$Targets)
ppcdesc<-pivot_wider(ppcdesc,id_cols = "longdesc$Targets",names_from="longdesc$ID",values_from = "longdesc$Rating")
ppcdesc<-ppcdesc[,-1]
ppcdesc<-ppcdesc[ , colSums(is.na(ppcdesc)) == 0]
ppcdescITC<-item.total(ppcdesc)
ppcdescITC$Item.Total<-fisherz(ppcdescITC$Item.Total)
FisherZInv(mean(ppcdescITC$Item.Total))

####Correlation Norm Prejudice per Target ???####


