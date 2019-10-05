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



shinyUI(fluidPage(

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="kernel",label = 'Select the kernel', choices=c('Linear','Radial Basis', 'Polynomial','Sigmoid',multiple=FALSE, selected='Linear'),
            sliderInput("n","Sample size",min = 2,max = 568199, value=10000),
            sliderInput("cost", "C", min = 0, max = 500,value = 1, step=1),
            sliderInput("gamma","gamma",min =0 ,max =1 ,value =0.01,step=0.01 )

        ),
        mainPanel(
            rglwidgetOutput("plot",  width = 800, height = 600),
            textOutput('text1'),
            textOutput("predsvm"),
        )
    )
)
))


