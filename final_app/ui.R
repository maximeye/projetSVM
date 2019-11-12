library(e1071)
library(smotefamily)
library(ggplot2)
library(rgl)
library(misc3d)
library(ROCR)
library(leaps)
library(caTools)
library(MASS)
library(ROCR)
library(mlr)
library(FSelector)
library(rpart)
library(gbm)
library(xgboost)
library(ggplot2)
library(ineq)
set.seed(12345)



shinyUI(fluidPage(
  (navbarPage(title="Demonstrator : SVM performances",
              
              tabPanel(p(icon("info-circle"),"Informations"),
                       uiOutput("doc")
              ),
              tabPanel(p(icon("book-open"),"Some definitions"),
                       uiOutput("doc_to_display")
              ),
              
              
              
              tabPanel(p(icon("database"),"Data presentation"),
                       
                     
                         
                       
                           textOutput('txt'),
                           verbatimTextOutput("dim"),
                           textOutput('txt1'),
                           plotOutput("name")
                           
                           
                       
              ),
              
              tabPanel(p(icon("project-diagram"),"Comparing SVM with other models"),
                         
                         sidebarLayout(
                           
                           
                           
                           
                           
                           sidebarPanel(
                             fluidRow(
                               column(6, radioButtons("law", 
                                                      label = "Model to compare with SVM", 
                                                      choices = list("Logistic Regression"='logit',"Decision Tree" ='tree', 'Random Forest'='rf','Gradient Boosting'='gb', 'XGBoost'='xgb' ),
                                                      selected = 'logit')),
                               column(6,'Parameters','',
                                      uiOutput("minsplit"),
                                      uiOutput("minbucket"),
                                      uiOutput("cp"),
                                      uiOutput("ntree"),
                                      uiOutput("nodesize"),
                                      uiOutput("mtry"),
                                      uiOutput("n.trees"),
                                      uiOutput("interaction_d"),
                                      uiOutput("minobsinnode"),
                                      uiOutput("schrink"),
                                      uiOutput("nround"),
                                      uiOutput("maxdepth"),
                                      uiOutput("lambda"),
                                      uiOutput("eta"),
                                      uiOutput("subsample"),
                                      uiOutput("minweight"),
                                      uiOutput("coltree")
                               )
                               
                             ),
                             
                             

                             
                             selectInput("kernel",
                                         "SVM Kernel",
                                         choices = list("Linear"='linear',"Radial Basis"="radial","Polynomial"='polynomial',"Sigmoid"='sigmoid')),
                             sliderInput(inputId='n', label='Sample size (More the sample size is large, longer the process time will be)',min=1000,max=30000,value=1000,step=100),
                             sliderInput(inputId='cost', label='C',min=1,max=100,value=25.8,step=0.1),
                             #sliderInput(inputId='gamma', label='Gamma',min=0,max=1,value=0.573,step=0.001)

                             
                             
                           ),
                           
                           

                           mainPanel(
                             
                             textOutput("t1"),
                             verbatimTextOutput("i1"),
                             textOutput("t2"),
                             verbatimTextOutput("i2"),
                             textOutput("t3"),
                             verbatimTextOutput("i3"),
                             
                             textOutput("t4"),
                             verbatimTextOutput("i4"),
                             textOutput("t5"),
                             verbatimTextOutput("i5"),
                             textOutput("t6"),
                             verbatimTextOutput("i6"),
                             textOutput("t7"),
                             
                             plotOutput("roc1")
                           )
                         )
                         
                         
                         
                         
                         
                
                
              ),
              
              tabPanel(p(icon("chart-bar"),"Multiple ROC Curve comparison"),
                
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(inputId='nn', label='Sample size (More the sample size is large, longer the process time will be)',min=1000,max=30000,value=1000,step=100)
                    
                  ),
                  mainPanel(
                    plotOutput("roc2")
                  )
                  
                  
                
              )
  ),
  tabPanel(p(icon("comment"),"Conclusion"),
           uiOutput("doc_to_display2")
  )
  ))))

