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
#injunktiv Vorurteil 0.90
#deskriptiv Vorurteil 0.47
#deskriptiv injunktiv 0.72

cor.test(d$RWA,d$SDO)
#.44
cor.test(d$RWA,d$ideology)
#.32
cor.test(d$SDO,d$ideology)
#.37

####Korrelation SDO RWA####

#Vorurteile
dprej<-subset(d,d$condition=="prejudice")
RWAPrej<-corr.test(dprej[,c(73,9:69)])
RWAPrej$r[,1]
#mean -.05
SDOPrej<-corr.test(dprej[,c(74,9:69)])
SDOPrej$r[,1]
#mean -.10

#injunktive Normen
dinj<-subset(d,d$condition=="injunctive")
RWAInj<-corr.test(dinj[,c(73,9:69)])
RWAInj$r[,1]
#mean .07
SDOInj<-corr.test(dinj[,c(74,9:69)])
SDOInj$r[,1]
#mean -.04
#deskriptive Normen
ddesc<-subset(d,d$condition=="descriptive")
RWADesc<-corr.test(ddesc[,c(73,9:69)])
RWADesc$r[,1]
#mean .05
SDODesc<-corr.test(ddesc[,c(74,9:69)])
SDODesc$r[,1]
#mean .01
#korrelierte Korrelationen
correlations<-cbind.data.frame(RWAPrej$r[,1],SDOPrej$r[,1],
                    RWAInj$r[,1],SDOInj$r[,1],
                    RWADesc$r[,1],SDODesc$r[,1])
corr.test(correlations[-1,1:6])
#RWA und SDO sagen Vorurteile und Injunktive Normen relativ (.59 - .80) ähnlich vorher
#Vorhersage von Vorurteilen durch RWA und SDO ganz anders als von Deskriptiven Normen
#vgl. geringe Korrelation Vorurteile/Deskriptive Normen

####Person Population Correlation####
longprej<-subset(dlong,dlong$condition=="prejudice")
longinj<-subset(dlong,dlong$condition=="injunctive")
longdesc<-subset(dlong,dlong$condition=="descriptive")
#Datenstruktur ändern
ppcprej<-cbind.data.frame(longprej$ID,longprej$Rating,longprej$Targets)
ppcprej<-pivot_wider(ppcprej,id_cols = "longprej$Targets",names_from="longprej$ID",values_from = "longprej$Rating")
ppcprej<-ppcprej[,-1]
ppcprej<-ppcprej[ , colSums(is.na(ppcprej)) == 0]
ppcprejITC<-item.total(ppcprej)
ppcprejITC$Item.Total<-fisherz(ppcprejITC$Item.Total)
FisherZInv(mean(ppcprejITC$Item.Total))

#PPC Prej .69
ppcinj<-cbind.data.frame(longinj$ID,longinj$Rating,longinj$Targets)
ppcinj<-pivot_wider(ppcinj,id_cols = "longinj$Targets",names_from="longinj$ID",values_from = "longinj$Rating")
ppcinj<-ppcinj[,-1]
ppcinj<-ppcinj[ , colSums(is.na(ppcinj)) == 0]
ppcinjITC<-item.total(ppcinj)
ppcinjITC$Item.Total<-fisherz(ppcinjITC$Item.Total)
FisherZInv(mean(ppcinjITC$Item.Total))

#PPC Inj .70
ppcdesc<-cbind.data.frame(longdesc$ID,longdesc$Rating,longdesc$Targets)
ppcdesc<-pivot_wider(ppcdesc,id_cols = "longdesc$Targets",names_from="longdesc$ID",values_from = "longdesc$Rating")
ppcdesc<-ppcdesc[,-1]
ppcdesc<-ppcdesc[ , colSums(is.na(ppcdesc)) == 0]
ppcdescITC<-item.total(ppcdesc)
ppcdescITC$Item.Total<-fisherz(ppcdescITC$Item.Total)
FisherZInv(mean(ppcdescITC$Item.Total))

#PPC Desc .70
####Person Population Correlation korrelieren####
ppcallcond<-rbind(ppcprejITC,ppcinjITC,ppcdescITC)
ppcallcond<-ppcallcond[,1:2]
colnames(ppcallcond)<-c("ID","ppc")
d<-merge(d,ppcallcond,by="ID")

corr.test(d$ppc,d$Ident)
corr.test(d$ppc,d$age)
corr.test(d$ppc,d$RWA)
corr.test(d$ppc,d$SDO)
corr.test(d$ppc,d$ideology)
corr.test(d$Ident,d$RWA)

#negative Korrelationen von PPC mit SDO, RWA, Ideology, Alter aber nicht Identifikation

####Correlation Norm Prejudice per Target ???####
dprejtargets<-transpose(dprej[,9:69])
dinjtargets<-transpose(dinj[,9:69])
ddesctargets<-transpose(ddesc[,9:69])
dcorr<-rbind.fill(dprejtargets,dinjtargets,ddesctargets)
dcorr<-transpose(dcorr)
colnames(dcorr)<-c(Targets$Prej...2,Targets$Inj...3,Targets$Des...4)
cortable<-corr.test(dcorr)
cortable_r<-cortable$r
prejinj<-cortable_r[62:122,1:61]
prejinj<-diag(prejinj)

prejdesc<-cortable_r[123:183,1:61]
prejdesc<-diag(prejdesc)


descinj<-cortable_r[62:122,123:183]
descinj<-diag(descinj)

proTargetNormPrej<-round(cbind(prejinj,prejdesc,descinj),2)
rownames(proTargetNormPrej)<-Targets$Targets
#eher geringe Korrelationen

####PreTest####
load("TargetFeatures.Rda")
pretesttargets<-read_xlsx("targetnames.xlsx")
targetfeat$Target<-pretesttargets$target
means_new<-cbind.data.frame(means[,1],means[,2],means[,3])
colnames(means_new)<-c("Prej","Des","Inj")
means_new$Target<-Targets$Targets
master_newstudy<-merge(targetfeat,means_new,by ="Target")
master_newstudy<-master_newstudy[,-c(6:10)]
corr.test(master_newstudy[,2:8])

#Korrelation mit Daten aus MA
#Korrelationen von Stereotypen und Vorurteilen unterscheiden sich von denen mit Normen
#bspw. Prej Belief -.09, Prej Des/Inf -.51,-.65

####MultiLevel Versuche####
#ICC als m?glicher Indikator f?r ?hnlichkeit ??????
#Mittelwerte als weiterer Prädiktor

longprej$meansprej<-rep(means_new$Prej,nrow(longprej)/61)
longprej$meansinj<-rep(means_new$Inj,nrow(longprej)/61)
longprej$meansdesc<-rep(means_new$Des,nrow(longprej)/61)


mdesc<-lmerTest::lmer(Rating~RWA+meansdesc+(RWA|Targets),data = longprej)
summary(mdesc)
icc(mdesc)

minj<-lmerTest::lmer(Rating~RWA+meansinj+(RWA|Targets),data = longprej)
summary(minj)
icc(minj)


lm_mdesc<-lm(Rating~RWA+meansdesc,data = longprej)
summary(lm_mdesc)

lm_minj<-lm(Rating~RWA+meansinj,data = longprej)
summary(lm_minj)

m1a<-lmerTest::lmer(Rating~1+(1|Targets),data = longprej)
summary(m1a)
icc(m1a)

m1b<-lmerTest::lmer(Rating~RWA+(1|Targets),data = longprej)
summary(m1b)
icc(m1b)

m1c<-lmerTest::lmer(Rating~1+(RWA|Targets),data = longprej)
summary(m1c)
icc(m1c)

m1d<-lmerTest::lmer(Rating~RWA+(RWA|Targets),data = longprej)
summary(m1d)
icc(m1d)
compare_performance(m1a,m1b,m1c,m1d)
check_model(m1d)
####bestes Modell m1c, Vorhersage durch random RWA Effekt pro Target

m2<-lmerTest::lmer(Rating~1+(1|Targets),data = longinj)
summary(m2)
icc(m2)

m3<-lmerTest::lmer(Rating~1+(1|Targets),data = longdesc)
summary(m3)
icc(m3)
#ICC wird weitaus größer, wenn RWA als Random-Factor in das Modell aufgenommen wird, wenn RWA Fixed-Factor ist, kaum höheres ICC, gleiches gilt für r2
#Spricht dafür, dass RWA nicht konsistent Vorurteile vorhersagt, sondern spezifische Vorurteile gegen spezifische Targets



####Cluster####
####für Vorurteile 
dprejclust<-transpose(dprej[,9:69])
rownames(dprejclust)<-Targets$Targets
## Ward-Methode
ward_prejclust <- agnes(dprejclust, metric = "euclidean", stand = TRUE, method = "ward")
# desto besser lassen sich Cluster unterscheiden, > 0.5 gut)
ward_prejclust$ac
## Merge-Matrix
# negative Werte beschreiben Ursprungsperson
# positive Werte beschreiben Schritt, indem das Cluster entstanden ist
ward_prejclust$merge

## Dendrogramm
pltree(ward_prejclust, main = "Dendrogramm Ward-Methode")

##### Distanz
options(digits = 4)

## Ward
ward_prejclust_QS <- sort(ward_prejclust$height)
ward_prejclust_QS

#Plot
plot(ward_prejclust_QS, type = "b", xlab = "Number of Clusters (Ward)", ylab = "Distanz")

## Clusterzuordnung 
cluster_ward_prejclust<-cutree(ward_prejclust, 4)

prejclusterfinal<-cbind(cluster_ward_prejclust,means[,1])
by(prejclusterfinal[,2],prejclusterfinal[,1],describe)

####für injunktive Normen 
dinjclust<-transpose(dinj[,9:69])
rownames(dinjclust)<-Targets$Targets
## Ward-Methode
ward_injclust <- agnes(dinjclust, metric = "euclidean", stand = TRUE, method = "ward")
# desto besser lassen sich Cluster unterscheiden, > 0.5 gut)
ward_injclust$ac
## Merge-Matrix
# negative Werte beschreiben Ursprungsperson
# positive Werte beschreiben Schritt, indem das Cluster entstanden ist
ward_injclust$merge

## Dendrogramm
pltree(ward_injclust, main = "Dendrogramm Ward-Methode")

##### Distanz
options(digits = 4)

## Ward
ward_injclust_QS <- sort(ward_injclust$height)
ward_injclust_QS

#Plot
plot(ward_injclust_QS, type = "b", xlab = "Number of Clusters (Ward)", ylab = "Distanz")

## Clusterzuordnung 
cluster_ward_injclust<-cutree(ward_injclust, 4)

injclusterfinal<-cbind(cluster_ward_injclust,means[,1])
by(injclusterfinal[,2],injclusterfinal[,1],describe)

####für deskriptive Normen 
ddescclust<-transpose(ddesc[,9:69])
rownames(ddescclust)<-Targets$Targets
## Ward-Methode
ward_descclust <- agnes(ddescclust, metric = "euclidean", stand = TRUE, method = "ward")
# desto besser lassen sich Cluster unterscheiden, > 0.5 gut)
ward_descclust$ac
## Merge-Matrix
# negative Werte beschreiben Ursprungsperson
# positive Werte beschreiben Schritt, indem das Cluster entstanden ist
ward_descclust$merge

## Dendrogramm
pltree(ward_descclust, main = "Dendrogramm Ward-Methode")

##### Distanz
options(digits = 4)

## Ward
ward_descclust_QS <- sort(ward_descclust$height)
ward_descclust_QS

#Plot
plot(ward_descclust_QS, type = "b", xlab = "Number of Clusters (Ward)", ylab = "Distanz")

## Clusterzuordnung 
cluster_ward_descclust<-cutree(ward_descclust, 4)

descclusterfinal<-cbind(cluster_ward_descclust,means[,1])
by(descclusterfinal[,2],descclusterfinal[,1],describe)

####für Mittelwerte der Bedingungen
## Ward-Methode
ward_meansclust <- agnes(means, metric = "euclidean", stand = TRUE, method = "ward")
# desto besser lassen sich Cluster unterscheiden, > 0.5 gut)
ward_meansclust$ac
## Merge-Matrix
# negative Werte beschreiben Ursprungsperson
# positive Werte beschreiben Schritt, indem das Cluster entstanden ist
ward_meansclust$merge

## Dendrogramm
pltree(ward_meansclust, main = "Dendrogramm Ward-Methode")

##### Distanz
options(digits = 4)

## Ward
ward_meansclust_QS <- sort(ward_meansclust$height)
ward_meansclust_QS

#Plot
plot(ward_meansclust_QS, type = "b", xlab = "Number of Clusters (Ward)", ylab = "Distanz")

## Clusterzuordnung 
cluster_ward_meansclust<-cutree(ward_meansclust, 4)

meansclusterfinal<-cbind(cluster_ward_meansclust,means[,1])
by(meansclusterfinal[,2],meansclusterfinal[,1],describe)

####Vergleich Lösungen####
allcluster<-cbind(cluster_ward_prejclust,cluster_ward_injclust,cluster_ward_descclust,cluster_ward_meansclust,means)
table(cluster_ward_prejclust,cluster_ward_meansclust)
table(cluster_ward_prejclust,cluster_ward_injclust)
table(cluster_ward_prejclust,cluster_ward_descclust)
table(cluster_ward_injclust,cluster_ward_descclust)

ward_prejclust$ac
ward_injclust$ac
ward_descclust$ac
ward_meansclust$ac

corr.test((allcluster[,1:4]))
#AC bei MW-Clusterlösung am besten (Artefakt von kleinerer Datengrundlage?)
#Vorurteilsbedingung produziert die unterschiedlichsten Cluster im Vergleich zu Des, Inj, MW

####Cluster von Personen####
## Ward-Methode
dprejpers<-transpose(dprejclust)
colnames(dprejpers)<-Targets$Targets
ward_persclust <- agnes(dprejpers, metric = "euclidean", stand = TRUE, method = "ward")
# desto besser lassen sich Cluster unterscheiden, > 0.5 gut)
ward_persclust$ac
## Merge-Matrix
# negative Werte beschreiben Ursprungsperson
# positive Werte beschreiben Schritt, indem das Cluster entstanden ist
ward_persclust$merge

## Dendrogramm
pltree(ward_persclust, main = "Dendrogramm Ward-Methode")

##### Distanz
options(digits = 4)

## Ward
ward_persclust_QS <- sort(ward_persclust$height)
ward_persclust_QS

#Plot
plot(ward_persclust_QS, type = "b", xlab = "Number of Clusters (Ward)", ylab = "Distanz")

## Clusterzuordnung 
cluster_ward_persclust<-cutree(ward_persclust, 4)

cluster_ward_persclust<-cbind(cluster_ward_persclust,dprej$RWA,dprej$SDO,dprej$Soldaten)
by(cluster_ward_persclust[,5],cluster_ward_persclust[,1],describe)


####Faktorenanalyse####
#Prejudice
factcor1<-cortable_r[1:61,1:61]
PCA1 <- princomp(covmat = factcor)
summary(PCA1, loadings = TRUE)
plot(PCA1,type="lines")
PCA1a_varimax <- principal(factcor, nfactors = 4, rotate = "varimax")
PCA1a_varimax$loadings
#Des
factcor2<-cortable_r[62:122,62:122]
PCA2 <- princomp(covmat = factcor2)
summary(PCA2, loadings = TRUE)
plot(PCA2,type="lines")
PCA2a_varimax <- principal(factcor2, nfactors = 4, rotate = "varimax")
PCA2a_varimax$loadings
#Des
factcor3<-cortable_r[123:183,123:183]
PCA3 <- princomp(covmat = factcor3)
summary(PCA3, loadings = TRUE)
plot(PCA3,type="lines")
PCA3a_varimax <- principal(factcor3, nfactors = 4, rotate = "varimax")
PCA3a_varimax$loadings

cor(PCA3a_varimax$loadings[,1],PCA1a_varimax$loadings[,1])
cor(PCA3a_varimax$loadings[,2],PCA1a_varimax$loadings[,2])
cor(PCA3a_varimax$loadings[,3],PCA1a_varimax$loadings[,3])
cor(PCA3a_varimax$loadings[,4],PCA1a_varimax$loadings[,4])

cor(PCA3a_varimax$loadings[,1],PCA2a_varimax$loadings[,1])
cor(PCA3a_varimax$loadings[,2],PCA2a_varimax$loadings[,2])
cor(PCA3a_varimax$loadings[,3],PCA2a_varimax$loadings[,3])
cor(PCA3a_varimax$loadings[,4],PCA2a_varimax$loadings[,4])

cor(PCA2a_varimax$loadings[,1],PCA1a_varimax$loadings[,1])
cor(PCA2a_varimax$loadings[,2],PCA1a_varimax$loadings[,2])
cor(PCA2a_varimax$loadings[,3],PCA1a_varimax$loadings[,3])
cor(PCA2a_varimax$loadings[,4],PCA1a_varimax$loadings[,4])



