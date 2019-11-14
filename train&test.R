library(tidyverse)
#Importation des données
table<-read.csv("C:/Users/Amancy/Documents/GitHub/SVM/creditcard.csv")
attach(table)

#Rééchantillonnage et partionnement
table_bis<-mutate(table,Class=ifelse(Class=="0","sain","defaut"))
Class=as.data.frame(as.factor(table_bis$Class))
table_bis<-replace(table_bis,31,Class)
newtable <- SMOTE(Class ~ ., table_bis, perc.over =600 ,perc.under =250 )
set.seed(123)  
obs<-1:nrow(newtable)
long_obs<-length(obs)
ech<-sample(obs,0.7*long_obs,replace=FALSE)  
train<-newtable[ech,]
test<-newtable[-ech,] 


write.csv(train,"C:/Users/Amancy/Documents/GitHub/SVM/creditcard_train.csv", row.names = FALSE)
write.csv(test,"C:/Users/Amancy/Documents/GitHub/SVM/creditcard_test.csv", row.names = FALSE)
