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



output$plot1 <- renderPlot({
    
    
    taille_ech=input$n
    index=(1:nrow(data))
    trainindex=sample(index,round(taille_ech*0.7))
    train=data[trainindex,]
    itest=sample(index,round(taille_ech*0.3))
    test=data[itest,]
    attach(train)
    
    
    if (input$kernel=='Linear'){
        model=svm(class~.,data=train,kernel="linear",scale=F,cost=input$cost)
        plot(model,train,col=c("bisque","lightblue"))
    }
    else if (input$kernel=='Polynomial'){
        model=svm(class~., data=train, kernel="polynomial",gamma =input$gamma, cost =input$cost) 
        plot(model,train,col=c("bisque","lightblue"))
    }
    else if (input$kernel=='Radial Basis'){
        model=svm(class~., data=train, kernel="radial",gamma = input$gamma, cost =input$cost) 
        plot(model,train,col=c("bisque","lightblue"))
    }
    else if (input$kernel=='Sigmoid'){
        model=svm(class~., data=train, kernel="sigmoid",gamma = input$gamma, cost =input$cost) 
        plot(model,train,col=c("bisque","lightblue"))
    }
})
output$explication1 <- renderText({
    'We can observe that the SVM create a line who separate data in two parts, we have above points where realisations are 1 (fraud) and below realisations wich are 0 (no fraud). 
    The rate of good classification is :' 
    Y=predict(model,newdata = test)
    table(test$class,Y)
    mean(test$class==Y)
    
})




















