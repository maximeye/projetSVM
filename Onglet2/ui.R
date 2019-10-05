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
                
                tabPanel("Description of SVM",
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput(inputId='n', label='Sample size',min=2,max=568199,value=10000,step=50),
                                 selectInput(inputId="kernel",label="Choose a kernel",choices=c('Linear','Polynomial','Radial Basis','Sigmoid'),multiple = F,selected = 'Linear'),
                                 sliderInput(inputId='cost', label='C',min=1,max=500,value=10,step=1),
                                 sliderInput(inputId='gamma', label='Gamma',min=0,max=1,value=0.01,step=0.01),
                                 actionButton("submit" ,"Refresh", icon("refresh"))
                             ),
                             mainPanel(
                                 plotOutput('plot1'),
                                 textOutput('explication1')
                             )
                             
                         )
                )
                
                
                
                
                

                
                
                
    )))