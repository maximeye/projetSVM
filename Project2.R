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

library(data.table)
library(rJava)



set.seed(12345)
#Chargement de la table de donnees :

#dat=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/creditcard.csv",header=T,sep=",")
dat=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/creditcard.csv",header=T,sep=",")
# On change le type de la variable de reponse "Class" (integer -> factor)
dat$Class=as.factor(dat$Class)


attach(dat)

# On change le type de la variable de reponse "Class" (integer -> factor)
dat$Class=as.factor(dat$Class)


# Reechantillonnage afin d'obtenir 50% de class = 0, 50% de class=1
new=SMOTE(dat[,-31],dat[,31],K=3,dup_size = 0)

#Transformation du type de variable de class en factor (auparavant character)
new$data$class=as.factor(new$data$class)


#Exportation de la nouvelle table en CSV (optionnelle), utile pour ne pas avoir a refaire l'etape de reechantillonnage chaque fois

#write.csv(new$data,"/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/newdat.csv")
write.csv(new$data,"C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/projetSVM/new.csv")
table(new$data$class) # 284315 Class=0
                      # 283884 Class=1



########################### DEBUT ###########################

# Chargement de la table reechantillonnee

data=read.csv("/Users/Maxime/Documents/Cours/Master/M2/M2S1/SVM/Docs Projet/new.csv",header=T,sep=",")
data=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/projetSVM/new.csv",header=T,sep=",")

data$class=as.factor(data$class)
set.seed(12345)
data=data[,-1]





#### Debut de partitionnage Apprentissage / Test ####


#Creation d'un echantillon d'apprentissage (70%) et test (30%) :
################NE PAS EXECUTER CE QUI SUIT  ############

#index=1:nrow(data)
#trainindex=sample(index,397740)
#train=data[trainindex,]
#test=data[-trainindex,]
#attach(train)


## Echantillon apprentissage pour faire tourner le svm rapidement

taille_ech=10000
index=1:nrow(data)
trainindex=sample(index,round(taille_ech*0.7))
train=data[trainindex,]
itest=sample(index,round(taille_ech*0.3))
test=data[itest,]
attach(train)


#create a task
trainTask = makeClassifTask(data = train,target = "class")
testTask = makeClassifTask(data = test, target = "class")

# Let's consider the positive class as 1
trainTask = makeClassifTask(data = train,target = "class", positive = "1")

# Let's normalize the variables
trainTask = normalizeFeatures(trainTask,method = "standardize")
testTask = normalizeFeatures(testTask,method = "standardize")


# Feature importance
im_feat=generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
plotFilterValues(im_feat,n.show = 20)



# Quadratic Discriminant Analysis (QDA)



##############################################################################
##########################   Loading QDA   ###################################
##############################################################################


qda=makeLearner("classif.qda", predict.type = "response")

# Train model
qmodel=train(qda, trainTask)

# Predict on test data
qpredict=predict(qmodel, testTask)

# Create submission file
submit1=data.frame(class=test$class,class_status=qpredict$data$response)
write.csv(submit1, "C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/submit1.csv",row.names = F)
table(submit1$class,submit1$class_status)
mean(submit1$class==submit1$class_status)




##############################################################################
##########################   Logistic Regression   ###################################
##############################################################################



# Logistic regression
logistic=makeLearner("classif.logreg",predict.type = "response")

# Cross validation (cv) accuracy
cv.logistic=crossval(learner=logistic,task=trainTask,iters = 3,stratify = TRUE,measures = acc,show.info = F)

# Cross validation accuracy
cv.logistic$aggr


# Train model
fmodel=train(logistic,trainTask)
getLearnerModel(fmodel)

# Predict on test data
fpmodel=predict(fmodel, testTask)

# Create submission file
submit2=data.frame(class = test$class, class_Status = fpmodel$data$response)
write.csv(submit2, "C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/submit2.csv",row.names = F)

table(submit2$class,submit2$class_Status)
mean(submit2$class==submit2$class_Status)




##############################################################################
##########################   Decision Tree   ###################################
##############################################################################


getParamSet("classif.rpart")

# Making the decision tree
tree=makeLearner("classif.rpart", predict.type = "response")

# Set 3 fold cross validation
set_cv=makeResampleDesc("CV",iters = 3L)


# Searching for some hyperparameters
gs=makeParamSet(
                makeIntegerParam("minsplit",lower = 10, upper = 50),
                makeIntegerParam("minbucket", lower = 5, upper = 50),
                makeNumericParam("cp", lower = 0.001, upper = 0.2))

# Minsplit represents the minimum number of observation in a node for a split to take place.
# Minbucket says the minimum number of observation I should keep in terminal nodes.
# Cp is the complexity parameter.
# The lesser it is, the tree will learn more specific relations in the data which might result in overfitting.


# Grid search
gscontrol=makeTuneControlGrid()

# Hypertune the parameters
stune=tuneParams(learner=tree, resampling = set_cv, task = trainTask, par.set = gs, control = gscontrol, measures = acc)
# Check best parameter
stune$x

# Cross validation result
stune$y


# Using hyperparameters for modeling
t.tree=setHyperPars(tree, par.vals = stune$x)

# Train the model
t.rpart=train(t.tree, trainTask)
getLearnerModel(t.rpart)

# Make predictions
tpmodel=predict(t.rpart, testTask)

# Create a submission file
submit3=data.frame(class= test$class, class_Status = tpmodel$data$response)
write.csv(submit3, "C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/submit3.csv",row.names = F)

table(submit3$class,submit3$class_Status)
mean(submit3$class==submit3$class_Status)





##############################################################################
##########################   Random Forest   ###################################
##############################################################################

getParamSet("classif.randomForest")

# Create a learner
rf=makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
rf$par.vals=list(importance = TRUE)



# set tunable parameters
# Grid search to find hyperparameters
rf_param=makeParamSet(
                      makeIntegerParam("ntree",lower = 50, upper = 500),
                      makeIntegerParam("mtry", lower = 3, upper = 10),
                      makeIntegerParam("nodesize", lower = 10, upper = 50))

# Let's do random search for 50 iterations
rancontrol=makeTuneControlRandom(maxit = 50L)


# Set 3 fold cross validation
set_cv=makeResampleDesc("CV",iters = 3L)

# Hypertuning
rf_tune=tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = acc)

# cv accuracy
rf_tune$y

# The best parameters
rf_tune$x


# Building the RF model now & checking its accuracy

# Using hyperparameters for modeling
rf.tree=setHyperPars(rf, par.vals = rf_tune$x)

# Train a model
rforest=train(rf.tree, trainTask)
getLearnerModel(t.rpart)

# Making some predictions
rfmodel=predict(rforest, testTask)

# Submission file
submit4=data.frame(class = test$class, class_Status = rfmodel$data$response)
write.csv(submit4, "C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/submit4.csv",row.names = F)

table(submit4$class,submit4$class_Status)
mean(submit4$class==submit4$class_Status)




##############################################################################
################################   SVM   #####################################
##############################################################################


getParamSet("classif.ksvm") # do install kernlab package 
ksvm=makeLearner("classif.ksvm", predict.type = "response")



# Set parameters
pssvm=makeParamSet(
  makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #  Cost parameters
  makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4))) # RBF Kernel Parameter

pssvm=


# Specify search function
ctrl=makeTuneControlGrid()

# Tune model
res=tuneParams(ksvm, task = trainTask, resampling=set_cv, par.set = pssvm, control = ctrl,measures = acc)
# CV accuracy
#[Tune] Result: C=1; sigma=0.0625 : acc.test.mean=0.9841423
res$y


# Set the model with best params
t.svm=setHyperPars(ksvm, par.vals=res$x)

# Train
par.svm=train(ksvm,trainTask)
# Test
predict.svm=predict(par.svm, testTask)

# Submission file
submit5=data.frame(class = test$class, class_status = predict.svm$data$response)
write.csv(submit5, "C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/submit5.csv",row.names = F)

table(submit5$class,submit5$class_status)
mean(submit5$class==submit5$class_status)









##############################################################################
##########################   Gradient Boosting Machines   ###################################
##############################################################################


# Loading GBM
getParamSet("classif.gbm")
g.gbm=makeLearner("classif.gbm", predict.type = "response")

# Specify the tuning method
rancontrol=makeTuneControlRandom(maxit = 50L)

# 3 fold CV
set_cv=makeResampleDesc("CV",iters = 3L)


# Set tunable parameters
gbm_par=makeParamSet(
                  makeDiscreteParam("distribution", values = "bernoulli"),
                  makeIntegerParam("n.trees", lower = 100, upper = 1000),
                  makeIntegerParam("interaction.depth", lower = 2, upper = 10),
                  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
                  makeNumericParam("shrinkage",lower = 0.01, upper = 1))

# n.minobsinnode refers to the minimum number of observations in a tree node
# shrinkage is the regulation parameter which dictates how fast / slow the algorithm should move

# Tuning parameters
tune_gbm=tuneParams(learner = g.gbm, task = trainTask,resampling = set_cv,
                    measures = acc,par.set = gbm_par,control = rancontrol)


# Checking CV accuracy
tune_gbm$y

# Setting parameters
final_gbm=setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)

# Train
to.gbm=train(final_gbm, trainTask)

# Test
pr.gbm=predict(to.gbm, testTask)

# Submission file
submit6=data.frame(class = test$class, class_Status = pr.gbm$data$response)
write.csv(submit6, "C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/submit6.csv",row.names = F)

table(submit6$class,submit6$class_Status)
mean(submit6$class==submit6$class_Status)




##############################################################################
##########################   XGBoost   #######################################
##############################################################################


# XGBoost is a decision-tree-based ensemble Machine Learning algorithm
# that uses a gradient boosting framework.


# Loading XGBoost
set.seed(12345)
getParamSet("classif.xgboost")

# Make learner with inital parameters
xg_set=makeLearner("classif.xgboost", predict.type = "response")
xg_set$par.vals=list(objective = "binary:logistic",
                     eval_metric = "error",
                     nrounds = 250)

# Defining parameters for tuning
xg_ps=makeParamSet(
          makeIntegerParam("nrounds",lower=200,upper=600),
          makeIntegerParam("max_depth",lower=3,upper=20),
          makeNumericParam("lambda",lower=0.55,upper=0.60),
          makeNumericParam("eta", lower = 0.001, upper = 0.5),
          makeNumericParam("subsample", lower = 0.10, upper = 0.80),
          makeNumericParam("min_child_weight",lower=1,upper=5),
          makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8))


# Defining search function
rancontrol=makeTuneControlRandom(maxit = 100L)

# 3 fold cross validation
set_cv=makeResampleDesc("CV",iters = 3L)

# Tuning parameters
xg_tune=tuneParams(learner=xg_set, task=trainTask, resampling=set_cv,measures=acc,par.set=xg_ps, control=rancontrol)

# Setting parameters
xg_new=setHyperPars(learner=xg_set, par.vals=xg_tune$x)

# Train model
xgmodel=train(xg_new, trainTask)

# Test model
predict.xg=predict(xgmodel, testTask)

# Submission file
submit7=data.frame(class = test$class, class_Status = predict.xg$data$response)
write.csv(submit7, "C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/submit7.csv",row.names = F)

table(submit7$class,submit7$class_Status)
mean(submit7$class==submit7$class_Status)
