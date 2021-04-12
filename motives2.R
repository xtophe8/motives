##analyse lausanne sur les lyc?ens  donn?es motives
setwd("C:/Users/ladmin.schnitzler/Dropbox/science/article en ecriture/motives/Motives/article_persistence/lausanne 2020")

#import the data
library(readr)
motives <- read_csv("motives_clean.csv")
View(motives)

hist(motives$TOTAL_IPAQ)
motives2<-motives[,-c(1:3,9:11,17)]

motives2$lycee<-as.factor(motives2$lycee)
motives2$sexe<-as.factor(motives2$sexe)
motives2$Filiere<-as.factor(motives2$Filiere)
motives2$`Job Mere`<-as.factor(motives2$`Job Mere`)
motives2$`Job Pere`<-as.factor(motives2$`Job Pere`)
motives2$Implication_Sport<-as.factor(motives2$Implication_Sport)
motives2$OutofSchool_sport<-as.factor(motives2$OutofSchool_sport)
motives2$Implication_Frequence<-as.factor(motives2$Implication_Frequence)
motives2$Favorite_sport_PE<-as.factor(motives2$Favorite_sport_PE)

#visual exploration of the histogram only reveals problem on the column 22 heures_min_assis


#clustering


library(Factoshiny)
res<-PCAshiny(motives2) #prendre 3 dimensions puis faire la PCA avec distance manhattan. Mettre support pair parents en var supp
#lignes de code donnent un r?sultat un peu diff?rent, garder interpr?tation en haut
res.PCA<-imputePCA(motives2,ncp=3,quali.sup=c(1,2,3,4,5,7,8,9,10),quanti.sup=c(11,12,22),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=FALSE,graph=FALSE,metric='manhattan')
plot.HCPC(res.HCPC,choice='tree',title='Arbre hi?rarchique')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Plan factoriel')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Arbre hi?rarchique sur le plan factoriel')

#cluster
motives3<-res.HCPC$data.clust
#contenu des clusters
library(prettyR)
describe(motives3$clust)

