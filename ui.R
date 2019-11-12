library(shiny)
library(markdown)
library(rmarkdown)
library(knitr)

fluidPage(

          titlePanel(
            h2("Implémentation des SVM dans un contexte de détection de fraude",
                  style = "background-color: rgb(178,132,190);
                           color: rgba(4,4,4,0.66);
                           font-size:350%;
                           font-family:arial;
                           text-shadow: 1px 2px 2px rgba(147,64,39,0.48);
                           text-align:center")),
          
          navlistPanel(
            
            tabPanel(title= span(tagList(icon("toggle-on"), "Introduction")),

                     downloadLink("notice",h6("Téléchargez la notice",
                                              style="color: #FFEEF2;
                                               text-align:center;
                                               font-size: 12px; 
                                               line-height: 12px; 
                                               padding: 6px; 
                                               border-radius: 0px; 
                                               font-family: Georgia, serif; 
                                               background-image: linear-gradient(to right, #A43249 0%, #AD3890 0%, #752727 100%); 
                                               border: 2px solid #752727; 
                                               display: inline-block;")),
                     
                     includeHTML("Intro.html")),
          
            
            tabPanel(title= span(tagList(icon("user-secret"), "Notre analyse")),
                     
                     includeHTML("svm.html"),
                     includeHTML("svm2.html")),
            
            tabPanel(

              title= span(tagList(icon("brain"), "SVM modulable")),
                     
                  
                     sidebarPanel(
                       selectInput("kernel", h6("Choisissez un type de kernel:",
                                                style = "color: #d1454e;
                                                font-size:110%;
                                                text-align:center
                                                font-family: Arial, Helvetica, sans-serif;
                                                font-weight: 800;"),
                                   choice=c('linear','radial',
                                            'sigmoid','polynomial'),
                                   selected="Linéaire"),
                  
                       
                       conditionalPanel("input.kernel=='polynomial'",
                                   radioButtons("deg", label = (h6("Choisissez le nombre de degrés:",
                                                                           style = "color: #d1454e;
                                                                           font-size:110%;
                                                                           text-align:center
                                                                           font-family: Arial, Helvetica, sans-serif;
                                                                           font-weight: 800;")), 
                                                      choices = c(3,4,5),
                                                      selected = 3)),
                       
                       conditionalPanel("input.kernel=='radial'",
                                    sliderInput("gamma", label = (h6("Choisissez le gamma:",
                                                                     style = "color: #d1454e;
                                                                           font-size:110%;
                                                                           text-align:center
                                                                           font-family: Arial, Helvetica, sans-serif;
                                                                           font-weight: 800;")),
                                                      min = 0.1, max = 1, value = 0.1)),
                       

                       hr( style="border: none;
                           border-top: 3px double #333;
                           color: #333;
                             overflow: visible;
                           text-align: center;
                           height: 5px;"),
                       
                       sliderInput("pena",
                                   h6("Paramètre de pénalisation",
                                      style = "color: #d1454e;
                                      font-size:110%;
                                      text-align:center
                                      font-family: Arial, Helvetica, sans-serif;
                                      font-weight: 800;"),
                                    min = 1,  max = 100, value = 30)),
                     
                     mainPanel(navbarPage("",
                                          
                                          tabPanel(h6("Summary",
                                                      style = "color: #d1454e;
                                                      font-size:90%;
                                                      text-align:center
                                                      font-family: Arial, Helvetica, sans-serif;
                                                      font-weight: 800;
                                                      "),
                                                   
                                                   verbatimTextOutput("sum_svm")),
                                          
                                          tabPanel(h6("Matrice de confusion",
                                                      style = "color: #d1454e;
                                                      font-size:90%;
                                                      text-align:center
                                                      font-family: Arial, Helvetica, sans-serif;
                                                      font-weight: 800;
                                                      "),
                                                   
                                                   plotOutput("conf_svm")),
                                                   
                                          tabPanel(h6("Courbe ROC",
                                                      style = "color: #d1454e;
                                                      font-size:90%;
                                                      text-align:center
                                                      font-family: Arial, Helvetica, sans-serif;
                                                      font-weight: 800;"),
                                                   
                                                   plotOutput("roc_svm")),
                                          
                                          tabPanel(h6("Courbe Precision-Recall",
                                                      style = "color: #d1454e;
                                                      font-size:90%;
                                                      text-align:center
                                                      font-family: Arial, Helvetica, sans-serif;
                                                      font-weight: 800;"),
                                                   
                                                   plotOutput("pr_svm"))
                                          )
                               )
                     ),
            
            
            tabPanel(title= span(tagList(icon("th-list"), "Comparaison")),
                     navbarPage("",
                                                                          
                                tabPanel(h6("Régression Logistique",
                                         style = "color: #d1454e;
                                         font-size:110%;
                                         text-align:center
                                         font-family: Arial, Helvetica, sans-serif;
                                         font-weight: 800;"),
                                         
                                         includeHTML("logistic_regression.html")),
                                
                                tabPanel(h6("RandomForest",
                                            style = "color: #d1454e;
                                            font-size:110%;
                                            text-align:center
                                            font-family: Arial, Helvetica, sans-serif;
                                            font-weight: 800;"),
                                         
                                         includeHTML("RandomForest.html")),
                                
                                tabPanel(h6("Comparaison aux SVM",
                                            style = "color: #d1454e;
                                            font-size:110%;
                                            text-align:center
                                            font-family: Arial, Helvetica, sans-serif;
                                            font-weight: 800;"),
                                
                                          plotOutput("compROC"),
                                          plotOutput("compPR"))
                                
                                )
                     ),
            
            well=FALSE, widths =c(3,9))
          ) 

