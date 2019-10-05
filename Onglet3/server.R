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


shinyServer(function(input, output) {

    
    data=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/newdat.csv",header=T,sep=",")
    data$class=as.factor(data$class)
    set.seed(12345)
    data=data[,-1]
    
    values<- reactiveValues(kernel='Linear',n=10000,cost=10,gamma=0.01)
    
    observeEvent({input$kernel
        input$n
        input$cost
        input$gamma}
        
        {values$n=input$n
        if (input$kernel=='Linear') {values$kernel='linear'}
        if (input$kernel=='Polynomial') {values$kernel='polynomial'}
        if (input$kernel=='Radial Basis') {values$kernel='radial'}
        if (input$kernel=='Sigmoid') {values$kernel='sigmoid'}
        values$cost=input$cost
        values$gamma=input$gamma
        }
        )
    
    output$plot <- renderRglwidget({
        
        taille_ech=values$n
        index=1:nrow(data)
        trainindex=sample(index,round(taille_ech*0.7))
        train=data[trainindex,]
        itest=sample(index,round(taille_ech*0.3))
        test=data[itest,]
        attach(train)
        
        
        model=svm(class~., data=train, kernel=quote(values$kernel),gamma = 0.01, cost = 10) 
        
        
        colors =c("blue","red")
        p3d<- plot3d(train$V12, train$V14, train$V17, xlab="V12", ylab="V14",
                     zlab="V17",type="s",radius =0.3,
                     col=as.integer(train$class) ,
                     box=FALSE, size=5)
        p3d
        
        nnew = 100
        newdat.list = lapply(data[,-4], function(x) seq(min(x), max(x), len=nnew))
        newdat      = expand.grid(newdat.list)
        newdat.pred = predict(model, newdata=newdat, decision.values=T)
        newdat.dv   = attr(newdat.pred, 'decision.values')
        newdat.dv   = array(newdat.dv, dim=rep(nnew, 3))
        
        contour3d(newdat.dv, level=0, x=newdat.list$V17, y=newdat.list$V14, z=newdat.list$V12, add=T)
        
        
    })
        output$text1 <-  renderText({
            "Le taux de bonne classification sur l'échantillon COMPLET est de:"
        })
        output$predsvm<- renderText({
            Y=predict(model,newdata = data)
            table(test$class,Y)
            mean(test$class==Y)
            
        })
    

})