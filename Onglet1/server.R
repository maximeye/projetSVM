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



server <- function(input, output) {
    
    notice="https://raw.githubusercontent.com/maximeye/projetSVM/master/notice.Rmd"
    observeEvent(input$click,{file.show(url(notice))})
    
    file="https://raw.githubusercontent.com/maximeye/projetSVM/master/newdat.csv"
    data=read.csv(file=url(file),header=T,sep=",")
    data=data[,-1]
    attach(data)
    set.seed(12345)
    
    data$class=as.factor(data$class)
    index=(1:nrow(data))
    
    
    
    
    #p1
    
    output$presentation <- renderText({
        
    })
    output$dim <- renderTable({
        dim(data)
    })
    output$name <- renderTable({
        names(data)
    })
    output$sum <- renderDataTable({summary(data)
        
    })
    
}
    
    
    
    
    
    
    
    