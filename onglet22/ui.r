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
set.seed(12345)

shinyUI(fluidPage(
  (navbarPage(title="Demonstrator : SVM performances",

              
              ####Onglet 2 - Comparaison
              tabPanel(
  titlePanel("Comparison of SVM and another model"),
  
  sidebarLayout(
    
    
    
    
    
    sidebarPanel(
      ################ model
      fluidRow(
        column(6, radioButtons("law", 
                               label = "Model to compare with SVM", 
                               choices = list("Logistic Regression"='logit',"Decision Tree" ='tree', 'Random Forest'='rf','Gradient Boosting'='gb', 'XGBoost'='xgb' ),
                               selected = 'logit')),
        ###model's parameters
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
      
      
      
      ################ sample size
      
      selectInput("kernel",
                  "SVM Kernel",
                  choices = list("Linear"='linear',"Radial Basis"="radial","Polynomial"='polynomial',"Sigmoid"='sigmoid'))
      
      

 
      
      
    ),
    
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      splitLayout(
        verticalLayout (
          textOutput("t1"),
          tableOutput("i1"),
          textOutput("t2"),
          tableOutput("i2"),
          textOutput("t3"),
          tableOutput("i3")
                        ),
        verticalLayout    (
          textOutput("t4"),
          tableOutput("i4"),
          textOutput("t5"),
          tableOutput("i5"),
          textOutput("t6"),
          tableOutput("i6")
                          )
               )
      
    )
  
  ),
  
  mainPanel(
    plotOutput("roc")
  )
  )
  
    
  
  
  ))))