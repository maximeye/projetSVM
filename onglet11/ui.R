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
                
                # 1er onglet : Data presentation
                tabPanel("Data presentation",
                         
                         sidebarLayout(
                             sidebarPanel(
                             ),
                             
                             mainPanel(
                                 textOutput('txt'),
                                 tableOutput("dim"),
                                 textOutput('txt1'),
                                 plotOutput("name")
                                 
                                 
                                 
                             )
                         )
                )
    ))))
                
                