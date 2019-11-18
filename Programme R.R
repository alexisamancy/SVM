#####################################################################################
#####################                                           #####################
################            PROJET - SUPPORT VECTOR MACHINE          ################
################         Amancy Alexis    -    Mazloum Sabrina       ################
#####################                                           #####################
#####################################################################################

setwd('C:/Users/Desktop/MASTER 2/SVM/PROJET DEDEK/shiny/svm/svmproject')

rm(list = ls())

memory.limit()
memory.limit(size=1000000000)


#PARTIE 1: découverte de la table et rééchantillonnage

#Packages et librairies nécessaires: 

install.packages("devtools")
install.packages("ff")
install.packages("bit")
install.packages("e1071")
install.packages("tidyverse")
install.packages("dyplr")
library(tidyverse)
library(dyplr)
library(bit)
library(ff)
library(devtools)
library(treespace)
#library(readr)

#On importe les données 
setwd('C:/Users/Laurence/Desktop/MASTER 2/SVM/PROJET DEDEK/shiny/svm/svmproject')
table<-read.csv("creditcard.csv",sep=',')[1:10,]


#Pour avoir plus d'informations sur 'table'
attach(table)
dim(table)         #31 variables et 284807 observations
table(table$Class) # Il y a 284315 "1" et 492 "0"
head(table)
summary(table)


# Fréquence de la variable 'Class' illustrée par le graphique suivant:

barplot((table(table$Class)),ylim=c(0,300000),main = "Fréquence de 0 et de 1 dans la base de données",
        xlab = "Variable Class",
        ylab = "Fréquence", col= "tan1", border="tan1")


# On étudie maintenant les correlations: 

library(corrplot)

mcor<-cor(table)
symnum(mcor,abbr.colnames = FALSE)
corrplot(mcor,type="upper",order="hclust", bg="pink", tl.col="black", tl.srt=45)



#On procède maintenant au rééchantillonnage de  "table".

install.packages("DMwR")
library(DMwR)

#On remplace le "0" par "sain" et le "1" par "defaut"
#sinon la procedure SMOTE ne marche pas
#grace a mutate, on peut modifier les observations de la variable "Class" dans
# la table "table_bis". 
#avec ifelse on lui dit: si class="0" alors on remplace par "sain" sinon on écrit "defaut"
table_bis<-mutate(table,Class=ifelse(Class=="0","sain","defaut"))

#Pour faire la procedure SMOTE, il faut transformer les observations de la
#colonne "Class" en facteur 
#et le presenter sous forme de tableau grace à "as.data.frame"
Class=as.data.frame(as.factor(table_bis$Class))

#on remplace la variable "Class" de "table_bis" (31 eme colonne) par le nouvel objet 
#"Class"
table_bis<-replace(table_bis,31,Class)


#On peut maintenant proceder au rééchantillonnage avec la procedure SMOTE
#on essaye plusieurs  valeurs pour perc.over et perc.under, 
#voici les differents resulats:

newtable <- SMOTE(Class ~ ., table_bis, perc.over =600 ,perc.under =250 )
table(newtable$Class)

#frequence de base: 284315 pour les "defauts" et 492 pour les "sains"
#on remplace les valeurs de perc.over et perc.under, voici les resulats:


# SMOTE va prendre la classe minoritaire de "table_bis" pour le calcul 
#des nouvelles frequences de sains et de defauts

# par exemple si perc.over=600 et perc.under=250, les calculs effectu?s sont:
#492*6+492  =3444>> defaut  1
#492*6*2.5  =7380>> sain    0
#7380+3444=10824 en tout 


# On peut procéder à la formation de l'échantillon test et d'apprentissage
# > 70 % apprentissage et 30 % test 

set.seed(123)

#création d'une liste de 1  à 10824
obs<-1:nrow(newtable)


long_obs<-length(obs)  #=10824

#on forme l'échantillon d'apprentissage en prenant 70 % des chiffres de la liste 'obs'
ech<-sample(obs,0.7*long_obs,replace=FALSE)


train<-newtable[ech,]  # 7576 obs
test<-newtable[-ech,]  # 3248 obs

#On regarde la fréquence des 'sain' et des 'défaut' dans chaque échantillons
barplot((table(train$Class)),ylim=c(0,6000),main = "Fréquence de 'sain' et 'défaut' dans l'échantillon d'apprentissage",
        xlab = "Variable Class",
        ylab = "Fréquence", col= "tan1", border="tan1")


barplot((table(test$Class)),ylim=c(0,6000),main = "Fréquence de 'sain' et 'défaut' dans l'échantillon test",
        xlab = "Variable Class",
        ylab = "Fréquence", col= "tan1", border="tan1")



# Y a t-il des valeurs manquantes ? 
anyNA(train)
anyNA(test)




#PARTIE 2: Machine à vecteur de support 

#librairies utiles pour le SVM:
library(e1071)
library(rpart)

#On recharche les meilleurs parametres 'Cost' et 'Gamma' avec la fonction "tune":  
set.seed(123)
  
svm_tune<-tune(svm, Class ~.,data=train,ranges=
                 list(epsilon=c(0.01,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1),  cost=c(50,100,150,200,250,300,350,400,450,500)))


#On veut stocker ces résultats dans une table qu'on appelle  'resultat_bis'
summary(svm_tune)
resultat=svm_tune$performances


gamma<-as.data.frame(rep(c(0.01,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1),10))
names(gamma)='gamma'
cost<-as.data.frame(rep(c(50,100,150,200,250,300,350,400,450,500),each=9))
names(cost)='cost' 

error<- as.data.frame(rep(c(0.01108894,0.01161734,0.01227767,0.01148524,0.01135331,
                         0.01082509, 0.01122139,  0.01188154,0.01174962,0.01161769),each=9)) 
names(error)='error'


resultat_bis<-bind_cols(gamma,cost) %>% 
  bind_cols(error)




#On fait un graphique 3d pour illustrer ces résultats:


remotes::install_github("tylermorganwall/rayshader")
install.packages("rayshader")
install.packages("viridis")
install.packages("plot3D")
install.packages('rayshader')
install.packages('sf')

library(rayshader)
library(ggplot2)
library(rgl)

graph_tune<-ggplot(resultat_bis) +
  geom_point(aes(x=cost,y=gamma,color=1-error)) +
  scale_x_continuous(limits=c(50,500)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_colour_gradientn(colours=c('gold','coral1','violetred4'), limits=c(0.987,0.99)) +
    theme(
    # Modifier la couleur de fond de la l?gende
    legend.background = element_rect(fill = "grey98"),
    # Modifier la taille et la largeur des signes de la légende
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(1, "cm"))+
  theme(panel.background = element_rect(fill='wheat', colour='white'), axis.title.x = element_text(colour = "deeppink4", size=rel(1)),
        axis.title.y = element_text(colour = "deeppink4",size=rel(1)),
        plot.background = element_rect(fill="grey98"))
   

plot_gg(graph_tune, height=3, width=3.5, multicore=TRUE, pointcontract  = 0.7, soliddepth=-200)



#Autre graphique pour représenter la meme chose: 
plot(svm_tune, 
     transform.z = function(x) 1 - x,       
     swapxy = TRUE,                          
     color.palette = cm.colors,        
     xlab = 'cost', ylab = 'gamma',
     main = "Taux d'exactitude du SVM selon le gamma et le cost")



#On applique maintenant avec nos meilleurs paramètres le svm sur l'echantillon d'apprentissage 
svm.reg<-svm(Class~., data=train, gamma=0.01 ,cost=300,probability=TRUE,scale=FALSE) 

# >nombre de vecteurs supports : 482 (environ)

summary(svm.reg)

#On fait nos predictions sur l'échantillon test
pred<-predict(svm.reg,test[,-31], decision.values = TRUE, probability = TRUE)  #on retire la colonne class de test


#On en déduit la matrice de confusion: 

mc<-as.data.frame(table(pred,test$Class))
names(mc)=c('Predicted','Actual','Value')

Value<-select(mc,"Value")

#Voici le taux d'erreur du svm et le taux d'exactitude
erreur_svm<-round((sum(Value[1,],Value[4,])/sum(Value))*100,2)
exact_svm<-round((1-(erreur_svm/100))*100,2)


# On represente la matrice de confusion 

# seulement si le ggplot d'apres ne marche pas. réinstaller le package:
dev.off() 
install.packages("ggplot2")
library(ggplot2)


ggplot(data = mc,
       mapping = aes(x = Predicted,
                     y = Actual)) +
  geom_tile(aes(fill = Value)) +
  geom_text(aes(label = sprintf("%1.0f", Value)), vjust = 1) +
  scale_fill_gradient(low = "bisque",
                      high = "indianred3",
                      trans = "log") +
  theme(
    # Modifier la couleur de fond de la légende
    legend.background = element_rect(fill = "peachpuff2"),
    # Modifier la taille et la largeur des signes de la légende
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(1, "cm"))+
  theme(panel.background = element_rect( colour='peachpuff2'), axis.title.x = element_text(colour = "lightsalmon4", size=rel(1)),
        axis.title.y = element_text(colour = "lightsalmon4",size=rel(1)),
        plot.background = element_rect(fill="peachpuff2"))




#Production de la roc curve sur l'échantillon test: 

install.packages("pROC")
library(pROC)

dv_svm<-attributes(pred)$decision.values
svm_roc<-plot.roc(as.numeric(test$Class), dv_svm,col="mediumvioletred", xlab = "1 - Spécificité",ylab="Sensitivite",
         print.auc=TRUE)

#Extraction de l'AUC: 
auc_svm=as.numeric(svm_roc$auc)


#Production de la pr curve sur l'échantillon test: 

library(PRROC)

score1_svm=pred[test$Class=="defaut"]
score0_svm= pred[test$Class=="sain"]

pr_svm= pr.curve(score0_svm, score1_svm, curve = T)

par(bg="snow2")
svm_pr<-plot(pr_svm,  col="mediumvioletred", lwd=3, main="Courbe PR")
text(0.5,0.5,paste("AUC = ",format(pr_svm$auc.integral, digits=5, scientific=FALSE)))

#Extraction de l'AUC
auc_svm_pr<-as.numeric(pr_svm$auc.integral)




#PARTIE 3: Application du RANDOM FOREST

#On regarde les caractéristiques des variables
str(table)


install.packages("randomForest")
library(randomForest)
set.seed(123)

#On determine le 'ntree' optimal
#voir boucle pour le ntree et mtry dans fichier 'RF2'
fit_50<-randomForest(Class~.,data=train, ntree=50, na.action=na.omit)  # oob=1.6
fit_50

set.seed(123)
fit_100<-randomForest(Class~.,data=train, ntree=100, na.action=na.omit)  # oob=1.61
fit_100


fit_300<-randomForest(Class~.,data=train, ntree=300, na.action=na.omit)  # oob=1.68
fit_300

fit_500<-randomForest(Class~.,data=train, ntree=500, na.action=na.omit)  # oob=1.69
fit_500

fit_800<-randomForest(Class~.,data=train, ntree=800, na.action=na.omit)  # oob=1.67
fit_800


#Si on veut le nombre de fois ou l'individu a été laissé out of bag:
hist(fit_100$oob.times,main="Histogramme du nombre de fois ou l'individu est laissé OOB" )

#Extraction de l' oob de la sortie du 'fit_100':
y=fit_100$predicted
y2=train$Class 
oob<-mean(y2 != y)*100



#Quel est l'importance des variables explicatives pour les defauts et les sains?
varImpPlot(fit_100)
#sous forme de tableau:
fit_100$importance[order(fit_100$importance[, 1], decreasing = TRUE), ]



#On peut maintenant faire nos predictions sur l'échantillon test : 

predictions <- as.data.frame(predict(fit_100, test, type = "prob"))
prediction_rf<-predict(fit_100 ,test)

#Voici la matrice de confusion: 
mc_rf<-data.frame(table(prediction_rf, test$Class))

#Avec le taux d'erreur et d'exactitude
Freq<-select(mc_rf,"Freq")
erreur_rf<-(sum(Freq[1,],Freq[4,])/sum(Freq))*100
exact_rf<-(1-(erreur_rf/100))*100

mc_rf<-table(prediction_rf, test$Class)
erreur_rf<-(mc_rf[1,2]+mc_rf[2,1])/sum(mc_rf)
exact_rf<-(1-(erreur_rf/100))*100


#Représentation matrice de confusion 
library(ggplot2)

names(mc_rf)=c('Predicted','Actual','Value')

ggplot(data = mc_rf,
       mapping = aes(x = Predicted,
                     y = Actual)) +
  geom_tile(aes(fill = Value)) +
  geom_text(aes(label = sprintf("%1.0f", Value)), vjust = 1) +
  scale_fill_gradient(low = "bisque",
                      high = "indianred3",
                      trans = "log") +
  theme(
    # Modifier la couleur de fond de la l?gende
    legend.background = element_rect(fill = "peachpuff2"),
    # Modifier la taille et la largeur des signes de la l?gende
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(1, "cm"))+
  theme(panel.background = element_rect( colour='peachpuff2'), axis.title.x = element_text(colour = "lightsalmon4", size=rel(1)),
        axis.title.y = element_text(colour = "lightsalmon4",size=rel(1)),
        plot.background = element_rect(fill="peachpuff2"))



#Production de la  roc curve avec l'échantillon test

library(pROC)


ROC_rf<- roc(test$Class, predictions[,2])
ROC_rf_auc <- auc(ROC_rf)


rf_roc<-plot(ROC_rf, col = "mediumvioletred", main = "Courbe ROC ")
text(0.5,0.5,paste("AUC = ",format(ROC_rf_auc, digits=5, scientific=FALSE)))

#Extraction de l'AUC
auc_rf_roc<-as.numeric(rf_roc$auc)


#Production de la pr curve avec l'échantillon test: 
score1_rf=prediction_rf[test$Class=="defaut"]
score0_rf= prediction_rf[test$Class=="sain"]


pr_rf= pr.curve(score0_rf, score1_rf, curve = T)

par(bg="snow2")
rf_pr<-plot(pr_rf,  col="mediumvioletred", lwd=3, main="Courbe PR")
text(0.5,0.5,paste("AUC = ",format(pr_rf$auc.integral, digits=5, scientific=FALSE)))

#Extraction de l'AUC: 
auc_rf_pr<-as.numeric(pr_rf$auc.integral)



#PARTIE 4: Application de la régression logistique

#Installation des packages et des librairies nécessaires
install.packages('questionr')
install.packages('ggplot')
install.packages("DMwR")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("broom")
install.packages("forestmodel")

library(questionr)
library(ggplot2)
library(ggplot)
library(MASS)
library(DMwR)
library(corrplot)
library(Hmisc)
library(broom)
library(ROCR)
library(forestmodel)

#Nous allons appliquer la fonction 'relevel': 
newtable$Class<-relevel(newtable$Class,"sain")

str(newtable)
newtable$Class=factor(newtable$Class)

#On vérifie la présence de valeur manquante:
na=function(x){any(is.na(x))}
check.na=apply(newtable,2,na)
check.na

#On peut maintenant faire notre régression logistique:
reg<-glm(Class ~., data=train, family=binomial(logit))
reg0<-glm(Class~1,data=train,family=binomial(logit))

#On essaye ici les 3 méthodes de séléctions: 

#FORWARD:
mod_forward<-stepAIC(reg0,Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15
                     +V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount+Time
                     , data=train, trace=T,direction=c("forward"))

#BACKWARD:
mod_backward<-stepAIC(reg,~.,data=train, trace=T,direction=c("backward"))

#STEPWISE:
mod_stepwise<-stepAIC(reg,~.,data=train, trace=T,direction=c("both"))

summary(mod_stepwise)
summary(mod_forward) 
summary(mod_backward)

#Régression logistique:
set.seed(123)
bestreg<-glm(Class~Time + V1 + V2 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14
             + V15 + V16 + V20 + V22 + V23 + V26 + Amount, data=train, family = binomial(logit))
summary(bestreg)

fisher=as.numeric(bestreg$iter)

#graphique illustratif de nos résultats:
tmp<-tidy(bestreg, conf.int=T, exponentiate=T)
str(tmp)
ggplot(tmp)+aes(x=estimate,y=term,xmin=conf.low,xmax=conf.high)+
  geom_vline(xintercept = 1) + geom_errorbarh() + geom_point()+
  scale_x_log10()

forest_model(bestreg)


#Prédiction sur l'échantillon test et matrice de confusion:
glm.probs<-predict(bestreg,type="response",newdata=test)
glm.pred<-rep(1,nrow(test))
glm.pred[glm.probs>.5]=0

mc_log<-table(test$Class,glm.pred)

#Taux d'erreur et d'exactitude:
erreur_log<-round((sum(mc_log[1,2],mc_log[2,1])/sum(mc_log))*100,2)
exact_log<-round((1-(erreur_log/100))*100,2)


#Représentation graphique de la matrice de confusion
names(mc_log)<-c("Predicted","Actual","Value")

ggplot(data = mc_log,
       mapping = aes(x = Predicted,
                     y = Actual)) +
  geom_tile(aes(fill = Value)) +
  geom_text(aes(label = sprintf("%1.0f", Value)), vjust = 1) +
  scale_fill_gradient(low = "bisque",
                      high = "indianred3",
                      trans = "log") +
  theme(
    # Modifier la couleur de fond de la l?gende
    legend.background = element_rect(fill = "peachpuff2"),
    # Modifier la taille et la largeur des signes de la l?gende
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(1, "cm"))+
  theme(panel.background = element_rect( colour='peachpuff2'), axis.title.x = element_text(colour = "lightsalmon4", size=rel(1)),
        axis.title.y = element_text(colour = "lightsalmon4",size=rel(1)),
        plot.background = element_rect(fill="peachpuff2"))


#Production de la roc curve avec l'echantillon test


library(pROC)
library(ROCR)

p<-prediction(glm.probs,test$Class)

auc <- performance(p, measure = "auc")
auc <- auc@y.values[[1]]


lm_roc<-plot(roc(test$Class, glm.probs, direction="<"),
     col="mediumvioletred", lwd=3, main="Courbe ROC")
text(0.5,0.5,paste("AUC = ",format(auc, digits=2, scientific=FALSE)))

p <- prediction(glm.probs,test$Class)
auc <- performance(p, measure = "auc")
auc <- auc@y.values[[1]]


par(bg = "snow2")
plot(roc(test$Class, glm.probs, direction="<"),
     col="mediumvioletred", lwd=3, main="Courbe ROC", xlab="1 - Spécificité", ylab="Sensitivité",
     print.auc=TRUE)

#Extraction de l'AUC:
auc_log_roc<-a.numeric(1-auc)


#Production de la pr curve avec l'échantillon test:


install.packages("PRROC")
library(PRROC)

score1_lm= glm.probs[test$Class=="sain"]
score0_lm= glm.probs[test$Class=="defaut"]


pr_lm= pr.curve(score0_lm, score1_lm, curve = T)

par(bg="snow2")
lm_pr<-plot(pr_lm,  col="mediumvioletred", lwd=3, main="Courbe PR")
text(0.5,0.5,paste("AUC = ",format(pr_lm$auc.integral, digits=5, scientific=FALSE)))

#Extraction de l'AUC:
auc_log_pr<-round(as.numeric(pr_lm$auc.integral)*100,2)




#Comparaisons des 3 roc curves trouvées:

plot(svm_roc,col="darkred")
plot(rf_roc,col="darkgreen",add=TRUE)
plot(lm_roc,col="orange",add=TRUE)
legend(0.6,0.45, c('SVM','RF','LOGISTIC'),lty=c(1,1),
       lwd=c(2,2),col=c('darkred','darkgreen','orange'))


#Comparaison des 3 pr curves trouvées:

plot(pr_svm,col="darkred")
plot(pr_rf,col="darkgreen",add=TRUE)
plot(pr_lm,col="orange",add=TRUE)


#FIN 
