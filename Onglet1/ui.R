library(shiny)
library(e1071)
library(smotefamily)
library(ggplot2)
library(rgl)
library(misc3d)
library(ROCR)
library(leaps)
library(caTools)
library(httr)
#source("Projet R.R")


ui <- fluidPage(
    
    #Titre Programme tout à gauche
    (navbarPage(title="Machine Learning using SVM",
                
                # 1er onglet :
                tabPanel("Data presentation",
                         
                         sidebarLayout(
                             sidebarPanel(
                                 #actionButton(inputId="click",label="MANUAL",helpText='Please open the manual to understand how the application works')
                             ),
                             
                             mainPanel(
                                 textOutput('txt'),
                                 tableOutput("dim"),
                                 textOutput('txt1'),
                                 plotOutput("name")
                                 
                              
                                 
                             )
                         )
                )
    )))
                
                
                