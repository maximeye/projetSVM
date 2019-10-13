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
library(ineq)
set.seed(12345)

shinyServer(function(input, output) {
  
  
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
                    value = 5,
                    min = 5,
                    max = 50)
      }
    })
  
  
  
  output$minbucket = renderUI(
    {
      if (input$law == "tree" )
      {
        sliderInput("minbucket",
                    "Min Bucket",
                    value = 15,
                    min = 5,
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
                    value = 157,
                    min = 50,
                    max = 200)
      }
    })
  
  
  
  
  output$nodesize = renderUI(
    {
      if (input$law=='rf')
      {
        sliderInput("nodesize",
                    "Node Size",
                    value = 12,
                    min = 10,
                    max = 26)
      }
    })
  output$mtry= renderUI(
    {
      if (input$law=='rf')
      {
        sliderInput("mtry",
                    "Mtry",
                    value = 9,
                    min = 5,
                    max = 20)
      }
    })
  
  
  
  output$n.trees = renderUI(
    {
      if (input$law=='gb')
      {
        sliderInput("n.trees",
                    "N Trees",
                    value = 256,
                    min = 100,
                    max = 500)
      }
    })
  
  output$n.trees = renderUI(
    {
      if (input$law=='gb')
      {
        sliderInput("interaction_d",
                    "Interaction depth",
                    value = 5,
                    min = 2,
                    max = 10)
      }
    })
  
  output$minobsinnode = renderUI(
    {
      if ( input$law=='gb')
      {
        sliderInput("minobsinnode",
                    "Min obs in node",
                    value = 33,
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
                    value = 0.244,
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
                    value = 256,
                    min = 200,
                    max = 500)
      }
      
    })
  
  output$maxdepth = renderUI(
    {
      if (input$law=='xgb')
      {
        sliderInput("maxdepth",
                    "Max depth",
                    value = 20,
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
                    value = 0.56,
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
                    value = 0.278,
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
                    value = 0.56,
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
                    value = 3.84,
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
                    value = 0.683,
                    min = 0.2,
                    max = 0.8)
      }
    })
  
  
  
  
  output$t1 = renderText({
    "SVM: Gini coefficient"
    
  })
  
  output$i1=renderPrint({
    
    kernel=input$kernel
    cost=input$cost
    gamma=input$gamma
    
    gini.svm<- function(kernel,cost,gamma){
      
      if (input$kernel=="linear"){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="linear", cost=cost, gamma=gamma, shrinking=TRUE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        Gini=Gini(predict.svm$data$response)
      }
      else if (input$kernel=='radial'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="radial", cost=cost, gamma=gamma, shrinking=TRUE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        Gini=Gini(predict.svm$data$response)
      }
      else if (input$kernel=='polynomial'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="polynomial", cost=cost, gamma=gamma, shrinking=TRUE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        Gini=Gini(predict.svm$data$response)
      }
      else if (input$kernel=='sigmoid'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="sigmoid", cost=cost, gamma=gamma, shrinking=TRUE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        Gini=Gini(predict.svm$data$response)
      }
      return(Gini)
    }
    
    gini.svm(kernel,cost,gamma)
    
    
    
    
  })
  
  
  output$t2 = renderText({
    "SVM: Confusion matrix"
    
  })
  
  output$i2=renderPrint({
    kernel=input$kernel
    cost=input$cost
    gamma=input$gamma
    
    table.svm=function(kernel,cost,gamma){
      if (input$kernel=='linear'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="linear", cost=cost, gamma=gamma, shrinking=FALSE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        submit5=data.frame(class=test$class, class_status=predict.svm$data$response)
        matrix=table(submit5$class,submit5$class_status)
      }
      if (input$kernel=='radial'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="radial", cost=cost, gamma=gamma, shrinking=FALSE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        submit5=data.frame(class=test$class, class_status=predict.svm$data$response)
        matrix=table(submit5$class,submit5$class_status)
      }
      if (input$kernel=='polynomial'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="polynomial", cost=cost, gamma=gamma, shrinking=FALSE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        submit5=data.frame(class=test$class, class_status=predict.svm$data$response)
        matrix=table(submit5$class,submit5$class_status)
      }
      if (input$kernel=='sigmoid'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="sigmoid", cost=cost, gamma=gamma, shrinking=FALSE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        submit5=data.frame(class=test$class, class_status=predict.svm$data$response)
        matrix=table(submit5$class,submit5$class_status)
      }
      return(matrix)
    }
    
    table.svm(kernel,cost,gamma)
  })
  
  output$t3 = renderText({
    "SVM: Good classification rate"
    
  })
  
  output$i3=renderPrint({
    kernel=input$kernel
    cost=input$cost
    gamma=input$gamma
    mean.svm=function(kernel,cost,gamma){
      if (input$kernel=='linear'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="linear", cost=cost, gamma=gamma, shrinking=FALSE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        submit5=data.frame(class=test$class, class_status=predict.svm$data$response)
        matrix=table(submit5$class,submit5$class_status)
        classif=mean(submit5$class==submit5$class_status)
      }
      if (input$kernel=='radial'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="radial", cost=cost, gamma=gamma, shrinking=FALSE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        submit5=data.frame(class=test$class, class_status=predict.svm$data$response)
        matrix=table(submit5$class,submit5$class_status)
        classif=mean(submit5$class==submit5$class_status)
      }
      if (input$kernel=='polynomial'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="polynomial", cost=cost, gamma=gamma, shrinking=FALSE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        submit5=data.frame(class=test$class, class_status=predict.svm$data$response)
        matrix=table(submit5$class,submit5$class_status)
        classif=mean(submit5$class==submit5$class_status)
      }
      if (input$kernel=='sigmoid'){
        getParamSet("classif.svm")
        learner=makeLearner("classif.svm", predict.type="prob")
        cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)
        ctrl=makeTuneControlRandom(maxit=3)
        param.svm=makeParamSet(
          makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
          makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
          makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
          makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
          makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
          makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
          makeLogicalLearnerParam(id="shrinking"))
        final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="sigmoid", cost=cost, gamma=gamma, shrinking=FALSE))
        svm.model=train(final_svm, trainTask)
        predict.svm=predict(svm.model, testTask)
        submit5=data.frame(class=test$class, class_status=predict.svm$data$response)
        matrix=table(submit5$class,submit5$class_status)
        classif=mean(submit5$class==submit5$class_status)
      }
      return(classif)
    }
    mean.svm(kernel,cost,gamma)
    
  })
  
  output$t4 = renderText({
    "Selected model: Gini coefficient"
    
  })
  
  output$i4=renderPrint({
    law=input$law
    gini.model=function(law){
      
      if (input$law=='logit'){
        set.seed(12345)
        logistic=makeLearner("classif.logreg", predict.type="response")
        model=train(logistic, trainTask)
        pred=predict(model, testTask)
        performance(pred, measures=acc)
        gini=Gini(pred$data$response)
        submit2=data.frame(class=test$class, class_Status=pred$data$response)
        tab=table(submit2$class,submit2$class_Status)
        clas= mean(submit2$class==submit2$class_Status)
      }
      if (input$law=='tree'){
        set.seed(12345)
        minsplit=input$minsplit
        minbucket=input$minbucket
        cp=input$cp
        
        getParamSet("classif.rpart")
        tree=makeLearner("classif.rpart", predict.type="response")
        set_cv=makeResampleDesc("CV", iters=3)
        dtparam=makeParamSet(
          makeIntegerParam("minsplit", lower=5, upper=50),
          makeIntegerParam("minbucket", lower=5, upper=50),
          makeNumericParam("cp", lower=0.001, upper=0.5))
        gridsearchcontrol=makeTuneControlGrid()
        tun.tree=setHyperPars(tree, par.vals=list(minsplit=minsplit,minbucket=minbucket,cp=cp))
        tun.rpart=train(tun.tree, trainTask)
        treetestpred=predict(tun.rpart, testTask)
        gini=Gini(treetestpred$data$response)
        submit3=data.frame(class=test$class, class_Status=treetestpred$data$response)
        tab=table(submit3$class,submit3$class_Status)
        clas=mean(submit3$class==submit3$class_Status)
        
      }
      if (input$law=='rf'){
        set.seed(12345)
        ntree=input$ntree
        nodesize=input$nodesize
        mtry=input$mtry
        
        getParamSet("classif.randomForest")
        rf=makeLearner("classif.randomForest", predict.type="response", par.vals=list(ntree=200, mtry=3))
        rf$par.vals=list(importance=TRUE)
        rf_param=makeParamSet(
          makeIntegerParam("ntree",lower=50,upper=200),
          makeIntegerParam("mtry",lower=5,upper=20),
          makeIntegerParam("nodesize", lower=10, upper=26))
        rancontrol=makeTuneControlRandom(maxit=10)
        set_cv=makeResampleDesc("CV", iters=3)
        rf.tree=setHyperPars(rf, par.vals=list(ntree=ntree,mtry=mtry,nodesize=nodesize))
        rforest=train(rf.tree, trainTask) 
        rfmodel=predict(rforest, testTask)
        gini=Gini(rfmodel$data$response)
        submit4=data.frame(class = test$class, class_Status=rfmodel$data$response)
        tab=table(submit4$class,submit4$class_Status)
        clas= mean(submit4$class==submit4$class_Status)
        
      }
      if (input$law=='gb'){
        set.seed(12345)
        n.trees=input$n.trees
        interaction=input$interaction_d
        minobsinnode=input$minobsinnode
        schrinkage=input$schrink
        
        getParamSet("classif.gbm")
        g.gbm=makeLearner("classif.gbm", predict.type="response")
        rancontrol=makeTuneControlRandom(maxit=5)
        set_cv=makeResampleDesc("CV",iters=3)
        gbm_par=makeParamSet(
          makeDiscreteParam("distribution", values="bernoulli"),
          makeIntegerParam("n.trees", lower=100, upper=500),
          makeIntegerParam("interaction.depth", lower = 2, upper=10),
          makeIntegerParam("n.minobsinnode", lower=10, upper=80),
          makeNumericParam("shrinkage",lower=0.01, upper=1))
        final_gbm=setHyperPars(learner=g.gbm,
                               par.vals=list(distribution="bernoulli", n.trees=n.trees,
                                             interaction.depth=interaction, n.minobsinnode=minobsinnode,
                                             shrinkage=schrinkage))
        to.gbm=train(final_gbm, trainTask)
        pr.gbm=predict(to.gbm, testTask)
        gini=Gini(pr.gbm$data$response)
        submit6=data.frame(class = test$class, class_Status = pr.gbm$data$response)
        tab=table(submit6$class,submit6$class_Status)
        clas= mean(submit6$class==submit6$class_Status)
        
        
      }
      if (input$law=='xgb'){
        set.seed(12345)
        nround=input$nround
        maxdepth=input$maxdepth
        lambda=input$lambda
        eta=input$eta
        subsample=input$subsample
        minchildweight=input$minweight
        colsamplebytree=input$coltree
        
        
        getParamSet("classif.xgboost")
        xg_set=makeLearner("classif.xgboost", predict.type = "response")
        xg_set$par.vals=list(objective = "binary:logistic",
                             eval_metric = "error",
                             nrounds = 250)
        xg_ps=makeParamSet(
          makeIntegerParam("nrounds",lower=200,upper=500),
          makeIntegerParam("max_depth",lower=3,upper=20),
          makeNumericParam("lambda",lower=0.55,upper=0.60),
          makeNumericParam("eta", lower = 0.001, upper = 0.5),
          makeNumericParam("subsample", lower = 0.10, upper = 0.80),
          makeNumericParam("min_child_weight",lower=1,upper=5),
          makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8))
        rancontrol=makeTuneControlRandom(maxit=5)
        set_cv=makeResampleDesc("CV",iters=3)
        xg_new=setHyperPars(learner=xg_set, par.vals=list(nrounds=nround,max_depth=maxdepth,lambda=lambda,eta=eta,subsample=subsample,min_child_weight=minchildweight,colsample_bytree=colsamplebytree))
        xgmodel=train(xg_new, trainTask)
        predict.xg=predict(xgmodel, task=testTask)
        gini=Gini(predict.xg$data$response)
        submit7=data.frame(class = test$class, class_Status = predict.xg$data$response)
        tab=table(submit7$class,submit7$class_Status)
        clas=mean(submit7$class==submit7$class_Status)
      }
      return(gini)
      
    }
    
    gini.model(law)
    
  })
  
  output$t5 = renderText({
    "Selected model: Confusion matrix"
    
  })
  
  output$i5=renderPrint({
    law=input$law
    matrix.model=function(law){
      
      if (input$law=='logit'){
        set.seed(12345)
        logistic=makeLearner("classif.logreg", predict.type="response")
        model=train(logistic, trainTask)
        pred=predict(model, testTask)
        performance(pred, measures=acc)
        gini=Gini(pred$data$response)
        submit2=data.frame(class=test$class, class_Status=pred$data$response)
        tab=table(submit2$class,submit2$class_Status)
        clas= mean(submit2$class==submit2$class_Status)
      }
      if (input$law=='tree'){
        set.seed(12345)
        minsplit=input$minsplit
        minbucket=input$minbucket
        cp=input$cp
        
        getParamSet("classif.rpart")
        tree=makeLearner("classif.rpart", predict.type="response")
        set_cv=makeResampleDesc("CV", iters=3)
        dtparam=makeParamSet(
          makeIntegerParam("minsplit", lower=5, upper=50),
          makeIntegerParam("minbucket", lower=5, upper=50),
          makeNumericParam("cp", lower=0.001, upper=0.5))
        gridsearchcontrol=makeTuneControlGrid()
        tun.tree=setHyperPars(tree, par.vals=list(minsplit=minsplit,minbucket=minbucket,cp=cp))
        tun.rpart=train(tun.tree, trainTask)
        treetestpred=predict(tun.rpart, testTask)
        gini=Gini(treetestpred$data$response)
        submit3=data.frame(class=test$class, class_Status=treetestpred$data$response)
        tab=table(submit3$class,submit3$class_Status)
        clas=mean(submit3$class==submit3$class_Status)
        
      }
      if (input$law=='rf'){
        set.seed(12345)
        ntree=input$ntree
        nodesize=input$nodesize
        mtry=input$mtry
        
        getParamSet("classif.randomForest")
        rf=makeLearner("classif.randomForest", predict.type="response", par.vals=list(ntree=200, mtry=3))
        rf$par.vals=list(importance=TRUE)
        rf_param=makeParamSet(
          makeIntegerParam("ntree",lower=50,upper=200),
          makeIntegerParam("mtry",lower=5,upper=20),
          makeIntegerParam("nodesize", lower=10, upper=26))
        rancontrol=makeTuneControlRandom(maxit=10)
        set_cv=makeResampleDesc("CV", iters=3)
        rf.tree=setHyperPars(rf, par.vals=list(ntree=ntree,mtry=mtry,nodesize=nodesize))
        rforest=train(rf.tree, trainTask) 
        rfmodel=predict(rforest, testTask)
        gini=Gini(rfmodel$data$response)
        submit4=data.frame(class = test$class, class_Status=rfmodel$data$response)
        tab=table(submit4$class,submit4$class_Status)
        clas= mean(submit4$class==submit4$class_Status)
        
      }
      if (input$law=='gb'){
        set.seed(12345)
        ntrees=input$n.trees
        interaction=input$interaction_d
        minobsinnode=input$minobsinnode
        schrinkage=input$schrink
        
        getParamSet("classif.gbm")
        g.gbm=makeLearner("classif.gbm", predict.type="response")
        rancontrol=makeTuneControlRandom(maxit=5)
        set_cv=makeResampleDesc("CV",iters=3)
        gbm_par=makeParamSet(
          makeDiscreteParam("distribution", values="bernoulli"),
          makeIntegerParam("n.trees", lower=100, upper=500),
          makeIntegerParam("interaction.depth", lower = 2, upper=10),
          makeIntegerParam("n.minobsinnode", lower=10, upper=80),
          makeNumericParam("shrinkage",lower=0.01, upper=1))
        final_gbm=setHyperPars(learner=g.gbm,
                               par.vals=list(distribution="bernoulli", n.trees=ntrees,
                                             interaction.depth=interaction, n.minobsinnode=minobsinnode,
                                             shrinkage=schrinkage))
        to.gbm=train(final_gbm, trainTask)
        pr.gbm=predict(to.gbm, testTask)
        gini=Gini(pr.gbm$data$response)
        submit6=data.frame(class = test$class, class_Status = pr.gbm$data$response)
        tab=table(submit6$class,submit6$class_Status)
        clas= mean(submit6$class==submit6$class_Status)
        
        
        
      }
      if (input$law=='xgb'){
        set.seed(12345)
        nround=input$nround
        maxdepth=input$maxdepth
        lambda=input$lambda
        eta=input$eta
        subsample=input$subsample
        minchildweight=input$minweight
        colsamplebytree=input$coltree
        
        getParamSet("classif.xgboost")
        xg_set=makeLearner("classif.xgboost", predict.type = "response")
        xg_set$par.vals=list(objective = "binary:logistic",
                             eval_metric = "error",
                             nrounds = 250)
        xg_ps=makeParamSet(
          makeIntegerParam("nrounds",lower=200,upper=500),
          makeIntegerParam("max_depth",lower=3,upper=20),
          makeNumericParam("lambda",lower=0.55,upper=0.60),
          makeNumericParam("eta", lower = 0.001, upper = 0.5),
          makeNumericParam("subsample", lower = 0.10, upper = 0.80),
          makeNumericParam("min_child_weight",lower=1,upper=5),
          makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8))
        rancontrol=makeTuneControlRandom(maxit=5)
        set_cv=makeResampleDesc("CV",iters=3)
        xg_new=setHyperPars(learner=xg_set, par.vals=list(nrounds=nround,max_depth=maxdepth,lambda=lambda,eta=eta,subsample=subsample,min_child_weight=minchildweight,colsample_bytree=colsamplebytree))
        xgmodel=train(xg_new, trainTask)
        predict.xg=predict(xgmodel, task=testTask)
        gini=Gini(predict.xg$data$response)
        submit7=data.frame(class = test$class, class_Status = predict.xg$data$response)
        tab=table(submit7$class,submit7$class_Status)
        clas=mean(submit7$class==submit7$class_Status)
      }
      return(tab)
    }
    
    matrix.model(law)
  })
  
  output$t6 = renderText({
    "Selected model: Good classification rate"
    
  })
  
  output$i6=renderPrint({
    law=input$law
    classif.model=function(law){
      
      if (input$law=='logit'){
        set.seed(12345)
        logistic=makeLearner("classif.logreg", predict.type="response")
        model=train(logistic, trainTask)
        pred=predict(model, testTask)
        performance(pred, measures=acc)
        gini=Gini(pred$data$response)
        submit2=data.frame(class=test$class, class_Status=pred$data$response)
        tab=table(submit2$class,submit2$class_Status)
        clas= mean(submit2$class==submit2$class_Status)
      }
      if (input$law=='tree'){
        set.seed(12345)
        minsplit=input$minsplit
        minbucket=input$minbucket
        cp=input$cp
        
        getParamSet("classif.rpart")
        tree=makeLearner("classif.rpart", predict.type="response")
        set_cv=makeResampleDesc("CV", iters=3)
        dtparam=makeParamSet(
          makeIntegerParam("minsplit", lower=5, upper=50),
          makeIntegerParam("minbucket", lower=5, upper=50),
          makeNumericParam("cp", lower=0.001, upper=0.5))
        gridsearchcontrol=makeTuneControlGrid()
        tun.tree=setHyperPars(tree, par.vals=list(minsplit=minsplit,minbucket=minbucket,cp=cp))
        tun.rpart=train(tun.tree, trainTask)
        treetestpred=predict(tun.rpart, testTask)
        gini=Gini(treetestpred$data$response)
        submit3=data.frame(class=test$class, class_Status=treetestpred$data$response)
        tab=table(submit3$class,submit3$class_Status)
        clas=mean(submit3$class==submit3$class_Status)
        
      }
      if (input$law=='rf'){
        set.seed(12345)
        ntree=input$ntree
        nodesize=input$nodesize
        mtry=input$mtry
        
        getParamSet("classif.randomForest")
        rf=makeLearner("classif.randomForest", predict.type="response", par.vals=list(ntree=200, mtry=3))
        rf$par.vals=list(importance=TRUE)
        rf_param=makeParamSet(
          makeIntegerParam("ntree",lower=50,upper=200),
          makeIntegerParam("mtry",lower=5,upper=20),
          makeIntegerParam("nodesize", lower=10, upper=26))
        rancontrol=makeTuneControlRandom(maxit=10)
        set_cv=makeResampleDesc("CV", iters=3)
        rf.tree=setHyperPars(rf, par.vals=list(ntree=ntree,mtry=mtry,nodesize=nodesize))
        rforest=train(rf.tree, trainTask) 
        rfmodel=predict(rforest, testTask)
        gini=Gini(rfmodel$data$response)
        submit4=data.frame(class = test$class, class_Status=rfmodel$data$response)
        tab=table(submit4$class,submit4$class_Status)
        clas= mean(submit4$class==submit4$class_Status)
        
      }
      if (input$law=='gb'){
        set.seed(12345)
        ntrees=input$n.trees
        interaction=input$interaction_d
        minobsinnode=input$minobsinnode
        schrinkage=input$schrink
        
        getParamSet("classif.gbm")
        g.gbm=makeLearner("classif.gbm", predict.type="response")
        rancontrol=makeTuneControlRandom(maxit=5)
        set_cv=makeResampleDesc("CV",iters=3)
        gbm_par=makeParamSet(
          makeDiscreteParam("distribution", values="bernoulli"),
          makeIntegerParam("n.trees", lower=100, upper=500),
          makeIntegerParam("interaction.depth", lower = 2, upper=10),
          makeIntegerParam("n.minobsinnode", lower=10, upper=80),
          makeNumericParam("shrinkage",lower=0.01, upper=1))
        final_gbm=setHyperPars(learner=g.gbm,
                               par.vals=list(distribution="bernoulli", n.trees=ntrees,
                                             interaction.depth=interaction, n.minobsinnode=minobsinnode,
                                             shrinkage=schrinkage))
        to.gbm=train(final_gbm, trainTask)
        pr.gbm=predict(to.gbm, testTask)
        gini=Gini(pr.gbm$data$response)
        submit6=data.frame(class = test$class, class_Status = pr.gbm$data$response)
        tab=table(submit6$class,submit6$class_Status)
        clas= mean(submit6$class==submit6$class_Status)
        
        
        
      }
      if (input$law=='xgb'){
        set.seed(12345)
        nround=input$nround
        maxdepth=input$maxdepth
        lambda=input$lambda
        eta=input$eta
        subsample=input$subsample
        minchildweight=input$minweight
        colsamplebytree=input$coltree
        
        getParamSet("classif.xgboost")
        xg_set=makeLearner("classif.xgboost", predict.type = "response")
        xg_set$par.vals=list(objective = "binary:logistic",
                             eval_metric = "error",
                             nrounds = 250)
        xg_ps=makeParamSet(
          makeIntegerParam("nrounds",lower=200,upper=500),
          makeIntegerParam("max_depth",lower=3,upper=20),
          makeNumericParam("lambda",lower=0.55,upper=0.60),
          makeNumericParam("eta", lower = 0.001, upper = 0.5),
          makeNumericParam("subsample", lower = 0.10, upper = 0.80),
          makeNumericParam("min_child_weight",lower=1,upper=5),
          makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8))
        rancontrol=makeTuneControlRandom(maxit=5)
        set_cv=makeResampleDesc("CV",iters=3)
        xg_new=setHyperPars(learner=xg_set, par.vals=list(nrounds=nround,max_depth=maxdepth,lambda=lambda,eta=eta,subsample=subsample,min_child_weight=minchildweight,colsample_bytree=colsamplebytree))
        xgmodel=train(xg_new, trainTask)
        predict.xg=predict(xgmodel, task=testTask)
        gini=Gini(predict.xg$data$response)
        submit7=data.frame(class = test$class, class_Status = predict.xg$data$response)
        tab=table(submit7$class,submit7$class_Status)
        clas=mean(submit7$class==submit7$class_Status)
        
      }
      return(clas)
      
    }
    
    classif.model(law)
  })
 
  
  
  
  
  
  
  
  
})
