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

shinyServer(function(input, output) {
    
    data=readRDS("/Users/Maxime/Documents/Cours/Master/M2/M2S1/SVM/projetSVM/new.rds")
    data$class=as.factor(data$class)
    set.seed(12345)
    taille_ech=5000
    index=1:nrow(data)
    trainindex=sample(index,round(taille_ech*0.7))
    train=data[trainindex,]
    itest=sample(index,round(taille_ech*0.3))
    test=data[itest,]
    attach(train)
    trainTask=makeClassifTask(data=train, target="class")
    testTask=makeClassifTask(data=test, target="class")
    trainTask=makeClassifTask(data=train,target="class", positive="1")
    trainTask=normalizeFeatures(trainTask,method="standardize")
    testTask=normalizeFeatures(testTask,method="standardize")
    
    
    
    
    ###Onglet 1
    output$txt <- renderText({
        "Dimensions of the dataset we used : 568 199 observations and 26 variables"
        
    })
    output$dim <- renderTable({
        dim(data)
    })
    
    output$txt1 <- renderText({
        "Plot - Significance of each variables in the dataset : "
        
    })
    
    output$name <- renderPlot({       
        imp_feature=generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
        plotFilterValues(imp_feature,n.show = 20)
    })
    
})