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
    
    #file="https://raw.githubusercontent.com/maximeye/projetSVM/master/newdat.csv"
    #data=read.csv(file=url(file),header=T,sep=",")
    data=read.csv("/Users/Maxime/Documents/Cours/Master/M2/M2S1/SVM/Docs Projet/new.csv",header=T,sep=",")
    
    data=data[,-1]
    attach(data)
    set.seed(12345)
    
    data$class=as.factor(data$class)
    index=(1:nrow(data))
    
    
    
    
    
    output$txt <- renderText({
        "Dimensions of the dataset we used : 568 199 observations and 31 variables"
        
    })
    output$dim <- renderTable({
        dim(data)
    })
    
    output$txt1 <- renderText({
        "Plot - Significance of each variables in the dataset : "
        
    })
    output$name <- renderPlot({
        taille_ech=10000
        index=1:nrow(data)
        trainindex=sample(index,round(taille_ech*0.7))
        train=data[trainindex,]
        itest=sample(index,round(taille_ech*0.3))
        test=data[itest,]
        attach(train)
        
        
        # create a task
        trainTask = makeClassifTask(data = train,target = "class")
        testTask = makeClassifTask(data = test, target = "class")
        
        # Let's consider the positive class as 1
        trainTask = makeClassifTask(data = train,target = "class", positive = "1")
        
        # Let's normalize the variables
        trainTask = normalizeFeatures(trainTask,method = "standardize")
        testTask = normalizeFeatures(testTask,method = "standardize")
        
        
        # Feature importance
        imp_feature=generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
        plotFilterValues(imp_feature,n.show = 20)
    })
  
    
}
    
    
    
    
    
    
    
    