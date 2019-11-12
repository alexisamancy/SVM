#Packages utilisés
library(PRROC)
library(pROC)
library(ROCR)
library(DMwR)
library(tidyverse)
library(e1071)
library(rpart)
library(shiny)
library(plot3D)
library(Hmisc)
library(questionr)
library(broom)
library(forestmodel)
library(MASS)
library(randomForest)
library(rayshader)
library(ggplot2)
options(rgl.useNULL = TRUE)
library(rgl)

#Importation des données
table<-read.csv("C:/Users/Amancy/Documents/M2/Semestre 1/Support Vector Machine/projet/demo/hey/creditcard.csv")
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

#Préparation pour la comparaion des courbes ROC

  #RandomForest
    set.seed(123)
    fit_100<-randomForest(Class~.,data=train, ntree=100, na.action=na.omit)
    predictions <- as.data.frame(predict(fit_100, test, type = "prob"))
    
    #ROC
    set.seed(123)
    ROC_rf<- roc(test$Class, predictions[,2])
    ROC_rf_auc <- auc(ROC_rf)
    rf_roc<-plot(ROC_rf, col = "mediumvioletred", lwd=3,xlab="1 - Spécificité", ylab="Sensitivité", main = "Courbe ROC ")

    #PR
    set.seed(123)
    prediction_rf<-predict(fit_100 ,test)
    score1_rf=prediction_rf[test$Class=="defaut"]
    score0_rf= prediction_rf[test$Class=="sain"]
    pr_rf= pr.curve(score0_rf, score1_rf, curve = T)
    rf_pr<-plot(pr_rf,  col="mediumvioletred", lwd=3, main="Courbe PR", auc.main=FALSE)
    
  #Régression Logistique
    set.seed(123)
    bestglm <- glm(Class ~ V14 + V4 + V12 + V10 + V13 + V8 + V1 + V11 + V26 + V16 + 
                     V5 + V17 + V7 + V6 + Time + V21 + V22 + V15 + V28 + V27 + 
                     V9 + Amount + V20 + V19 + V24 + V2, data=train, family=binomial)
    
    glm.probs<-predict(bestglm,type="response",newdata=test)
    glm.pred<-rep(1,nrow(test))
    glm.pred[glm.probs>.5]=0
    
    #ROC
    p<-prediction(glm.probs,test$Class)
    auc <- performance(p, measure = "auc")
    auc <- auc@y.values[[1]]
    lm_roc<-plot(roc(test$Class, glm.probs, direction="<"),
                 col="mediumvioletred", lwd=2, xlim=c(1,0), ylim=c(0,1), xlab="1 - Spécificité", ylab="Sensitivité", main="Courbe ROC")

    #PR
    score1_lm= glm.probs[test$Class=="defaut"]
    score0_lm= glm.probs[test$Class=="sain"]
    pr_lm= pr.curve(score0_lm, score1_lm, curve = T)
    lm_pr<-plot(pr_lm,  col="mediumvioletred", lwd=3, main="Courbe PR", auc.main=FALSE)
    
  #SVM
    set.seed(123)
    svm.reg<-svm(Class~., data=train, gamma=0.01 ,cost=150) 
    pred<-predict(svm.reg,test[,-31], decision.values = TRUE)
    
    #ROC
    dv_svm<-attributes(pred)$decision.values
    svm_roc<-plot.roc(as.numeric(test$Class), lwd=3, xlim=c(1,0), ylim=c(0,1),dv_svm,col="mediumvioletred", xlab = "1 - Spécificité",ylab="Sensitivité")
  
    #PR
    score1_svm=pred[test$Class=="defaut"]
    score0_svm= pred[test$Class=="sain"]
    pr_svm= pr.curve(score0_svm, score1_svm, curve = T)
    svm_pr<-plot(pr_svm,  col="mediumvioletred", lwd=3, main="Courbe PR", auc.main=FALSE)

################################################################
################################################################

shinyServer(function(input, output) {

  output$notice <- downloadHandler(
    filename = "notice_.pdf",
    content = function(file) {
      file.copy("www/notice_.pdf", file)
    }
  )
  
  
  svm_ <- reactive({
    
    set.seed(123)
    if (input$kernel=="polynomial"){
      
      svm.reg <- svm(as.factor(Class)~., 
                     data=train,
                     kernel=input$kernel,
                     degree=input$deg,
                     cost=input$pena)
      
    } 
    
    else if (input$kernel=="radial"){
      
      svm.reg <- svm(as.factor(Class)~., 
                     data=train,
                     kernel=input$kernel,
                     gamma=input$gamma,
                     cost=input$pena) 
      
    } 
    
    else if (input$kernel=="linear"){
        
        svm.reg <- svm(as.factor(Class)~., 
                       data=train,
                       kernel=input$kernel,
                       cost=input$pena)
    }       
    
    else if (input$kernel=="sigmoid"){
    
    svm.reg <- svm(as.factor(Class)~., 
                   data=train,
                   kernel=input$kernel,
                   cost=input$pena)
  }       
      
    
 })
  
  output$sum_svm <- renderPrint ({
    summary(svm_())
  })
  
  output$conf_svm <- renderPlot({
    
    set.seed(123)
    pred<-predict(svm_(),test[,-31], decision.values = TRUE) 
    mc<-as.data.frame(table(pred,test$Class))
    names(mc)=c('Predicted','Actual','Value')
    
    ggplot(data = mc,
           mapping = aes(x = Predicted,
                         y = Actual)) +
          geom_tile(aes(fill = Value)) +
          geom_text(aes(label = sprintf("%1.0f", Value)), vjust = 1) +
          scale_fill_gradient(low = "bisque",
                              high = "indianred3",
                              trans = "log") +
      theme(
        legend.background = element_rect(fill = "peachpuff2"),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
      theme(
        panel.background = element_rect( colour='peachpuff2'), 
                           axis.title.x = element_text(colour = "lightsalmon4", size=rel(1)),
        axis.title.y = element_text(colour = "lightsalmon4",size=rel(1)),
        plot.background = element_rect(fill="peachpuff2"))
    
  })

  output$roc_svm <- renderPlot({
    
    set.seed(123)
    pred<-predict(svm_(),test[,-31], decision.values = TRUE)
    dv_svm<-attributes(pred)$decision.values
    par(bg="snow2")
    plot.roc(as.numeric(test$Class), dv_svm ,col="mediumvioletred", xlab = "1 - Spécificité", ylab="Sensitivité",
             print.auc=TRUE, main="Courbe ROC")
    
  })
  
  output$pr_svm <- renderPlot({
    
    set.seed(123)
    pred <- predict(svm_(),test[,-31], decision.values = TRUE)
    score1_svm= pred[test$Class=="défaut"]
    score0_svm = pred[test$Class=="sain"]
    pr_svm = pr.curve(score0_svm,score1_svm, curve=T)
    par(bg="snow2")
    plot(pr_svm, col="mediumvioletred", lwd=3, main="Courbe PR")
    text(0.5,0.5,paste("AUC = ",format(pr_svm$auc.integral, digits=5, scientific=FALSE)))
    
  })
  
  output$compROC <- renderPlot ({
    
    par(bg="snow2")
    plot(svm_roc,col="#3729FC",main="Comparaison des courbes ROC", lwd=2, xlim=c(1,0), ylim=c(0,1), xlab="1 - Spécificité", ylab="Sensitivité")
    plot(rf_roc,col="#4FBC3F",add=TRUE,lwd=2, xlim=c(1,0), ylim=c(0,1), xlab="1 - Spécificité", ylab="Sensitivité")
    plot(lm_roc,col="#BC3131",add=TRUE,lwd=2, xlim=c(1,0), ylim=c(0,1), xlab="1 - Spécificité", ylab="Sensitivité")
    legend("bottomright",legend=c("SVM : 0.994", "RandomForest : 0.999","Logistic : 0.990"),lty=c(1,1,1),
           lwd=c(1,1,1),col=c('#3729FC','#4FBC3F','#BC3131'), cex=1.2,
           title="Modèles", text.font=4, bg='#FCD7FB')
    
  },execOnResize=TRUE)
  
  output$compPR <- renderPlot ({
    
    par(bg="snow2")
    plot(pr_svm,col="#3729FC",main="Comparaison des courbes PR", lwd=2, xlim=c(0,1), ylim=c(0,1), xlab="Recall", ylab="Précision", auc.main=FALSE)
    plot(pr_rf,col="#4FBC3F",add=TRUE,lwd=2, xlim=c(1,0), ylim=c(0,1), xlab="Recall", ylab="Précision", auc.main=FALSE)
    plot(pr_lm,col="#BC3131",add=TRUE,lwd=2, xlim=c(1,0), ylim=c(0,1), xlab="Recall", ylab="Précision", auc.main=FALSE)
    legend("bottomright",legend=c("SVM : 0.979", "RandomForest : 0.977","Logistic : 0.994"),lty=c(1,1,1),
           lwd=c(1,1,1),col=c('#3729FC','#4FBC3F','#BC3131'), cex=1.2,
           title="Modèles", text.font=4, bg='#FCD7FB')
    
  },execOnResize=TRUE)

  
})
