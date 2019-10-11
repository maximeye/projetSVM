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
  
  
  #data=readRDS("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/projetSVM/new.rds")
  data=readRDS("/Users/Maxime/Documents/Cours/Master/M2/M2S1/SVM/projetSVM/new.rds")
  
  data$class=as.factor(data$class)
  set.seed(12345)
  taille_ech=1000
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
  
  
  
  
  output$minsplit = renderUI(
    {
      if (input$law == "tree")
      {
        sliderInput("minsplit",
                    "Min Split",
                    value = 6,
                    min = 1,
                    max = 50)
      }
    })
  
  
  
  output$minbucket = renderUI(
    {
      if (input$law == "tree" )
      {
        sliderInput("minbucket",
                    "Min Bucket",
                    value = 1,
                    min = 0,
                    max = 50)
      }
    })
  
  
  output$cp = renderUI(
    {
      if (input$law == "tree")
      {
        sliderInput("cp",
                    "CP",
                    value = 0.001,
                    min = 0.001,
                    max = 0.5,
                    step=0.001)
      }
    })
  
  
  output$ntree = renderUI(
    {
      if (input$law=='rf')
      {
        sliderInput("ntree",
                    "Number of trees",
                    value = 346,
                    min = 50,
                    max = 500)
      }
    })
  
  
  
  
  output$nodesize = renderUI(
    {
      if (input$law=='rf')
      {
        sliderInput("nodesize",
                    "Node Size",
                    value = 4,
                    min = 1,
                    max = 30)
      }
    })
  output$mtry= renderUI(
    {
      if (input$law=='rf')
      {
        sliderInput("mtry",
                    "Mtry",
                    value = 1,
                    min = 1,
                    max = 30)
      }
    })
  
  
  
  output$n.trees = renderUI(
    {
      if (input$law=='gb')
      {
        sliderInput("n.trees",
                    "N Trees",
                    value = 640,
                    min = 100,
                    max = 1000)
      }
    })
  
  output$minobsinnode = renderUI(
    {
      if ( input$law=='gb')
      {
        sliderInput("minobsinnode",
                    "Min obs in node",
                    value = 15,
                    min = 10,
                    max = 80)
      }
    })
  
  output$schrink = renderUI(
    {
      if (input$law=='gb')
      {
        sliderInput("schrink",
                    "Schrinkage",
                    value = 0.341,
                    min = 0.01,
                    max = 1)
      }
    })
  
  output$nround = renderUI(
    {
      if (input$law=='xgb')
      {
        sliderInput("nround",
                    "Nround",
                    value = 299,
                    min = 200,
                    max = 600)
      }
      
    })
  
  output$maxdepth = renderUI(
    {
      if (input$law=='xgb')
      {
        sliderInput("Max depth",
                    "maxdepth",
                    value = 18,
                    min = 3,
                    max = 20)
      }
      
    })
  
  output$lambda = renderUI(
    {
      if (input$law=='xgb')
      {
        sliderInput("lambda",
                    "Lambda",
                    value = 0.564,
                    min = 0.55,
                    max = 0.60)
      }
    })
  
  output$eta = renderUI(
    {
      if (input$law=='xgb')
      {
        sliderInput("eta",
                    "Eta",
                    value = 0.104,
                    min = 0.001,
                    max = 0.5)
      }
    })
  
  
  
  output$subsample = renderUI(
    {
      if (input$law=='xgb')
      {
        sliderInput("subsample",
                    "Sub sample",
                    value = 0.656,
                    min = 0.1,
                    max = 0.8)
      }
    })
  
  output$minweight = renderUI(
    {
      if (input$law=='xgb')
      {
        sliderInput("minweight",
                    "Min child weight",
                    value = 1.13,
                    min = 1,
                    max = 5)
      }
    })
  
  output$coltree = renderUI(
    {
      if (input$law=='xgb')
      {
        sliderInput("coltree",
                    "Col sample by tree",
                    value = 0.572,
                    min = 0.2,
                    max = 0.8)
      }
    })
  
  
  
  
  
  
  
})
