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

#paralléliser
library(parallelMap)

#démarrer la parallélisation
parallelStartSocket(cpus=4)


set.seed(12345)

# Chargement de la table de donnees :

# dat=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/creditcard.csv",header=T,sep=",")
dat=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/creditcard.csv",header=T,sep=",")

# On change le type de la variable de reponse "Class" (integer -> factor)
dat$Class=as.factor(dat$Class)


attach(dat)


# Reechantillonnage afin d'obtenir 50% de class = 0, 50% de class=1
new=SMOTE(dat[,-31],dat[,31],K=3,dup_size = 0)

#Transformation du type de variable de class en factor (auparavant character)
new$data$class=as.factor(new$data$class)


# Exportation de la nouvelle table en CSV (optionnelle), utile pour ne pas avoir a refaire l'etape de reechantillonnage chaque fois

# write.csv(new$data,"/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/newdat.csv")
write.csv(new$data,"C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/projetSVM/new.csv")
table(new$data$class) # 284315 Class=0
# 283884 Class=1


# Chargement de la table reechantillonnee

#data=read.csv("/Users/Maxime/Documents/Cours/Master/M2/M2S1/SVM/Docs Projet/new.csv",header=T,sep=",")
#data=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/projetSVM/new.csv",header=T,sep=",")

data=read.csv("C:/Users/kevas/Desktop/projetSVM/new.csv",header=T,sep=",")
data=data[,c(-1,-17,-15,-27,-24,-28)] # suppression des var les moins importantes
saveRDS(data,"C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/projetSVM/new.rds",compress=TRUE)



######################################################################################################
################################## DEBUT #############################################################
######################################################################################################



data=readRDS("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/projetSVM/new.rds")
data=readRDS("/Users/Maxime/Documents/Cours/Master/M2/M2S1/SVM/projetSVM/new.rds")

data$class=as.factor(data$class)
set.seed(12345)





# Creation d'un echantillon d'apprentissage (70%) et test (30%) :

## Echantillon apprentissage pour faire tourner le svm rapidement

taille_ech=30000
index=1:nrow(data)
trainindex=sample(index,round(taille_ech*0.7))
train=data[trainindex,]
itest=sample(index,round(taille_ech*0.3))
test=data[itest,]
attach(train)


# create a task
trainTask=makeClassifTask(data=train, target="class")
testTask=makeClassifTask(data=test, target="class")

# Let's consider the positive class as 1
trainTask=makeClassifTask(data=train,target="class", positive="1")

# Let's normalize the variables
trainTask=normalizeFeatures(trainTask,method="standardize")
testTask=normalizeFeatures(testTask,method="standardize")


# Feature importance of variables
imp_feature=generateFilterValuesData(trainTask, method=c("information.gain","chi.squared"))
plotFilterValues(imp_feature, n.show=20)




##############################################################################################
##################################   Logistic Regression   ###################################
##############################################################################################



# Logistic regression
logistic=makeLearner("classif.logreg", predict.type="prob")

# Training the model
model=train(logistic,trainTask)

# Predicting on the test set
pred=predict(model,testTask)

# Measuring the performance
performance(pred, measures=acc)
# 0.9804444

# Create submission file
submit2=data.frame(class=test$class, class_Status=pred$data$response)

table(submit2$class,submit2$class_Status)
mean(submit2$class==submit2$class_Status)







##############################################################################
##########################   Decision Tree   #################################
##############################################################################


getParamSet("classif.rpart")

# Making the decision tree
tree=makeLearner("classif.rpart", predict.type="response")

# Set 3 fold cross validation
set_cv=makeResampleDesc("CV", iters=3)


# Searching for some hyperparameters
#dtparam=makeParamSet(
#                makeIntegerParam("minsplit", lower=5, upper=50),
#                makeIntegerParam("minbucket", lower=5, upper=50),
#                makeNumericParam("cp", lower=0.001, upper=0.5))


# Minsplit represents the minimum number of observation in a node for a split to take place.
# Minbucket says the minimum number of observation I should keep in terminal nodes.
# Cp is the complexity parameter.
# The lesser it is, the tree will learn more specific relations in the data which might result in overfitting.


# Grid search
#gridsearchcontrol=makeTuneControlGrid()

# Hypertuning the parameters
#stune=tuneParams(learner=tree, resampling=set_cv, task=trainTask, par.set=dtparam, control=gridsearchcontrol, measures=acc)

# Checking the best parameter
stune$x
# [Tune] Result: minsplit=25; minbucket=5; cp=0.001


# Cross validation result
stune$y
# acc.test.mean
# 0.9697619


# Using hyperparameters for modeling
tun.tree=setHyperPars(tree, par.vals=list(minsplit=25,minbucket=10,cp=0.001))

# Train the model
tun.rpart=train(tun.tree, trainTask)
getLearnerModel(tun.rpart)

# Make predictions
treetestpred=predict(tun.rpart, testTask)

# Create a submission file
submit3=data.frame(class=test$class, class_Status=treetestpred$data$response)

table(submit3$class,submit3$class_Status)
mean(submit3$class==submit3$class_Status)





##############################################################################
##########################   Random Forest   #################################
##############################################################################

getParamSet("classif.randomForest")

# Create a learner
rf=makeLearner("classif.randomForest", predict.type="response", par.vals=list(ntree=200, mtry=3))
rf$par.vals=list(importance=TRUE)



# Set tunable parameters
# Grid search to find hyperparameters
#rf_param=makeParamSet(
#                      makeIntegerParam("ntree",lower=50,upper=200),
#                      makeIntegerParam("mtry",lower=5,upper=20),
#                      makeIntegerParam("nodesize", lower=10, upper=26))

# Let's do random search for 10 iterations
#rancontrol=makeTuneControlRandom(maxit=10)

# Set 3 fold cross validation
#set_cv=makeResampleDesc("CV", iters=3)


# Hypertuning
#rf_tune=tuneParams(learner=rf, resampling=set_cv, task=trainTask, par.set=rf_param, control=rancontrol, measures=acc)


# cv accuracy
rf_tune$y
# acc.test.mean=0.9901429

# The best parameters
rf_tune$x
# [Tune] Result: ntree=195; mtry=16; nodesize=11


# Building the RF model now & checking its accuracy

# Using hyperparameters for modeling
rf.tree=setHyperPars(rf, par.vals=list(ntree=195,mtry=16,nodesize=11))

# Train a model
rforest=train(rf.tree, trainTask)
getLearnerModel(tun.rpart)

# Making some predictions
rfmodel=predict(rforest, testTask)

# Submission file
submit4=data.frame(class = test$class, class_Status=rfmodel$data$response)

table(submit4$class,submit4$class_Status)
mean(submit4$class==submit4$class_Status)




##############################################################################
################################   SVM   #####################################
##############################################################################


# list of parameters which can be tuned
getParamSet("classif.svm")

# LEARNERS
learner=makeLearner("classif.svm", predict.type="prob")


# RESAMPLE
cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)

# Random search
ctrl=makeTuneControlRandom(maxit=5)

# TUNING SVM Parameters

param.svm=makeParamSet(
  makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
  makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
  makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
  makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
  makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
  makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
  makeLogicalLearnerParam(id="shrinking")
)

# Searching the optimal parameters
svm.res=tuneParams(learner, trainTask, resampling=cv.svm,
                   par.set=param.svm, control=ctrl,measures=acc)

# parameters optimal values
svm.res$x

# asset the parmeters to ours leaner
final_svm=setHyperPars(learner=learner, par.vals=svm.res$x)


# TRAINS
svm.model = train(final_svm,trainTask)

# PREDICTIONS
predict.svm = predict(svm.model, testTask)

# Submission file
submit5=data.frame(class = test$class, class_status = predict.svm$data$response)
write.csv(submit5, "C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/submit5.csv",row.names = F)

table(submit5$class,submit5$class_status)
mean(submit5$class==submit5$class_status)

#calculateConfusionMatrix(predict.svm, relative = TRUE)
#calculateROCMeasures(predict.svm)






##############################################################################
##########################   Gradient Boosting Machines   ####################
##############################################################################


# Loading GBM
getParamSet("classif.gbm")
g.gbm=makeLearner("classif.gbm", predict.type="response")

# Specify the tuning method
rancontrol=makeTuneControlRandom(maxit=50L)

# 3 fold CV
set_cv=makeResampleDesc("CV",iters=3L)


# Set tunable parameters
#gbm_par=makeParamSet(
#                  makeDiscreteParam("distribution", values="bernoulli"),
#                  makeIntegerParam("n.trees", lower=100, upper=1000),
#                  makeIntegerParam("interaction.depth", lower = 2, upper=10),
#                  makeIntegerParam("n.minobsinnode", lower=10, upper=80),
#                  makeNumericParam("shrinkage",lower=0.01, upper=1))

# n.minobsinnode refers to the minimum number of observations in a tree node
# shrinkage is the regulation parameter which dictates how fast / slow the algorithm should move

### Tuning parameters
#tune_gbm=tuneParams(learner = g.gbm, task = trainTask,resampling = set_cv,
#                    measures = acc,par.set = gbm_par,control = rancontrol)


# Checking CV accuracy
tune_gbm$y
# acc.test.mean=0.9915714


# Setting parameters
final_gbm=setHyperPars(learner=g.gbm,
                       par.vals=list(distribution="bernoulli",
                                     n.trees=640,interaction.depth=8,n.minobsinnode=15,
                                     shrinkage=0.341))

# Train
to.gbm=train(final_gbm, trainTask)

# Test
pr.gbm=predict(to.gbm, testTask)

# Submission file
submit6=data.frame(class = test$class, class_Status = pr.gbm$data$response)
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
#xg_ps=makeParamSet(
#          makeIntegerParam("nrounds",lower=200,upper=600),
#          makeIntegerParam("max_depth",lower=3,upper=20),
#          makeNumericParam("lambda",lower=0.55,upper=0.60),
#          makeNumericParam("eta", lower = 0.001, upper = 0.5),
#          makeNumericParam("subsample", lower = 0.10, upper = 0.80),
#          makeNumericParam("min_child_weight",lower=1,upper=5),
#          makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8))


# Defining search function
rancontrol=makeTuneControlRandom(maxit = 50L)

# 3 fold cross validation
set_cv=makeResampleDesc("CV",iters = 3L)

# Tuning parameters
#xg_tune=tuneParams(learner=xg_set, task=trainTask, resampling=set_cv,measures=acc,par.set=xg_ps, control=rancontrol)

# Setting parameters
xg_new=setHyperPars(learner=xg_set, par.vals=list(nrounds=299,max_depth=18,lambda=0.564,eta=0.104,subsample=0.656,min_child_weight=1.13,colsample_bytree=0.572),xg_tune$x)

# parameters optimal values
xg_new$x
#acc.test.mean=0.9908570


# Train model
xgmodel=train(xg_new, trainTask)

# Test model
predict.xg=predict(xgmodel, testTask)

# Submission file
submit7=data.frame(class = test$class, class_Status = predict.xg$data$response)

table(submit7$class,submit7$class_Status)
mean(submit7$class==submit7$class_Status)



######
# A FAIRE !!!!!

# ROC et comparer les méthodes de ML



#stopper la parallélisation
parallelStop()
