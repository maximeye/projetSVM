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

# Transformation du type de variable de class en factor (auparavant character)
new$data$class=as.factor(new$data$class)


# Exportation de la nouvelle table en CSV (optionnelle), utile pour ne pas avoir à refaire l'etape de rééchantillonnage à chaque fois.

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



zozo=read.table('C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/creditcard.csv',header=T,sep=",")

# On change le type de la variable de reponse "Class" (integer -> factor)
zozo$Class=as.factor(zozo$Class)
attach(zozo)

write.csv(zozo,"C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/newcreditcard.csv")
table(zozo$Class) # 284315 Class=0
                  # 492 Class=1

# Chargement de la table reechantillonnee
zozo=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/newcreditcard.csv",header=T,sep=",")
zozo=zozo[,c(-1,-17,-15,-27,-24,-28)] # suppression des var les moins importantes
saveRDS(zozo,"C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/newcreditcard.rds",compress=TRUE)







######################################################################################################
################################## DEBUT #############################################################
######################################################################################################

zozo=readRDS('C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/newcreditcard.rds')
zozo$Class=as.factor(zozo$Class)


data=readRDS("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/projetSVM/new.rds")
#data=readRDS("/Users/Maxime/Documents/Cours/Master/M2/M2S1/SVM/projetSVM/new.rds")
data$class=as.factor(data$class)
set.seed(12345)




# Creation d'un echantillon d'apprentissage (55%), test (18%) & validation (27%) :

## Echantillon apprentissage pour faire tourner le svm plus rapidement

taille_ech=175000
index=1:nrow(data)
trainindex=sample(index,round(taille_ech*0.55))
train=data[trainindex,]
validateindex=sample(index,round(taille_ech*0.27))
validate=data[validateindex,]
itest=sample(index,round(taille_ech*0.18))
test=data[itest,]
attach(train)


# Create a task
trainTask=makeClassifTask(data=train, target="class")
testTask=makeClassifTask(data=test, target="class")
validateTask=makeClassifTask(data=validate, target="class")
zozoTask=makeClassifTask(data=zozo, target="Class")

# Let's consider the positive class as 1
trainTask=makeClassifTask(data=train,target="class", positive="1")
testTask=makeClassifTask(data=test,target="class", positive="1")
validateTask=makeClassifTask(data=validate,target="class", positive="1")
zozoTask=makeClassifTask(data=zozo,target="Class", positive="1")

# Let's normalize the variables
trainTask=normalizeFeatures(trainTask,method="standardize")
testTask=normalizeFeatures(testTask,method="standardize")
validateTask=normalizeFeatures(validateTask,method="standardize")
zozoTask=normalizeFeatures(zozoTask,method="standardize")

# Feature importance of variables
imp_feature=generateFilterValuesData(trainTask, method=c("information.gain","chi.squared"))
plotFilterValues(imp_feature, n.show=20)




##############################################################################################
##################################   Logistic Regression   ###################################
##############################################################################################



# Logistic regression
logistic=makeLearner("classif.logreg", predict.type="prob")

# Training the model
model=train(logistic, trainTask)

# Predicting on the test set
pred1.test=predict(model, testTask)
# Measuring the performance
performance(pred1.test, measures=acc)
# 0.980254
# Create submission file
submit2=data.frame(class=test$class, class_Status=pred1.test$data$response)
table(submit2$class,submit2$class_Status)
mean(submit2$class==submit2$class_Status)
# Calculating the false/true positive rates on the test set & ploting the ROC Curve
roclog.test=generateThreshVsPerfData(pred1.test, measures = list(fpr, tpr, acc))
plotROCCurves(roclog.test)


# Training the model
#model=train(logistic, trainTask)
# Predicting on a new dataset
#pred1.zozo=predict(model, zozoTask)
# Create submission file
#submit2=data.frame(Class=zozo$Class, class_Status=pred1.zozo$data$response)
#table(submit2$class,submit2$class_Status)
#mean(submit2$class==submit2$class_Status)
# Calculating the false/true positive rates on a new dataset & ploting ROC Curve
#roclog.zozo=generateThreshVsPerfData(pred1.zozo, measures = list(fpr, tpr, acc))
#plotROCCurves(roclog.zozo)



##############################################################################
#############################   Decision Tree   ##############################
##############################################################################

# Making the decision tree
tree=makeLearner("classif.rpart", predict.type="prob")

# Set 3 fold cross validation
set_cv=makeResampleDesc("CV", iters=3)

# Searching for some hyperparameters
dtparam=makeParamSet(
                makeIntegerParam("minsplit", lower=5, upper=50),
                makeIntegerParam("minbucket", lower=5, upper=50),
                makeNumericParam("cp", lower=0.001, upper=0.5))


# Minsplit represents the minimum number of observation in a node for a split to take place.
# Minbucket says the minimum number of observation I should keep in terminal nodes.
# Cp is the complexity parameter.
# The lesser it is, the tree will learn more specific relations in the data which might result in overfitting.


# Grid search
gridsearchcontrol=makeTuneControlGrid()

# Hypertuning the parameters
#stune=tuneParams(learner=tree, resampling=set_cv, task=validateTask, par.set=dtparam, control=gridsearchcontrol, measures=acc)

# Checking the best parameter
stune$x
# [Tune] Result: minsplit=5; minbucket=15; cp=0.001


# Cross validation result
stune$y
# acc.test.mean
# 0.9737143


# Using hyperparameters for modeling
tun.tree=setHyperPars(tree, par.vals=list(minsplit=5,minbucket=15,cp=0.001))


# Train the model
tun.rpart=train(tun.tree, trainTask)
# Make predictions on the test set
treetestpred=predict(tun.rpart, testTask)
# Create a submission file
submit3=data.frame(class=test$class, class_Status=treetestpred$data$response)
table(submit3$class,submit3$class_Status)
mean(submit3$class==submit3$class_Status)
# 0.970381

# Calculating the false/true positive rates on the test set & ploting the ROC Curve
roc_dt.test = generateThreshVsPerfData(treetestpred, list(fpr, tpr, acc))
plotROCCurves(roc_dt.test)



# Train the model
#tun.rpart=train(tun.tree, trainTask)
# Make predictions on a a new dataset
#treezozopred=predict(tun.rpart, zozoTask)
# Create a submission file
#submit3=data.frame(class=zozo$Class, class_Status=treezozopred$data$response)
#table(submit3$class,submit3$class_Status)
#mean(submit3$class==submit3$class_Status)
# 0.4168121
# Calculating the false/true positive rates on a new dataset & ploting the ROC Curve
#roc_dt.zozo = generateThreshVsPerfData(treezozopred, list(fpr, tpr, acc))
#plotROCCurves(roc_dt.zozo)




##############################################################################
##########################   Random Forest   #################################
##############################################################################

# Create a learner
rf=makeLearner("classif.randomForest", predict.type="prob", par.vals=list(ntree=200, mtry=3))
rf$par.vals=list(importance=TRUE)


# Set tunable parameters
# Grid search to find hyperparameters
rf_param=makeParamSet(
                  makeIntegerParam("ntree",lower=50,upper=200),
                     makeIntegerParam("mtry",lower=5,upper=20),
                     makeIntegerParam("nodesize", lower=10, upper=26))

# Let's do random search for 10 iterations
rancontrol=makeTuneControlRandom(maxit=10)

# Set 3 fold cross validation
set_cv=makeResampleDesc("CV", iters=3)


# Hypertuning
# rf_tune=tuneParams(learner=rf, resampling=set_cv, task=validateTask, par.set=rf_param, control=rancontrol, measures=acc)


# cv accuracy
rf_tune$y
# acc.test.mean=0.9958942

# The best parameters
rf_tune$x
# [Tune] Result: ntree=157; mtry=9; nodesize=12


# Building the RF model now & checking its accuracy
# Using hyperparameters for modeling
rf.tree=setHyperPars(rf, par.vals=list(ntree=157,mtry=9,nodesize=12))
# Train a model
rforest=train(rf.tree, trainTask)

# Making some predictions on the test set
rfmodeltest=predict(rforest, testTask)
# Submission file
submit4=data.frame(class = test$class, class_Status=rfmodeltest$data$response)
table(submit4$class,submit4$class_Status)
mean(submit4$class==submit4$class_Status)
# 0.9982222
# Calculating the false/true positive rates on the test set & ploting the ROC Curve
rocrf.test=generateThreshVsPerfData(rfmodeltest, measures = list(fpr, tpr, acc))
plotROCCurves(rocrf.test)


# Making some predictions on a new database
#rfmodelzozo=predict(rforest, zozoTask)
# Submission file
#submit4=data.frame(class = zozo$Class, class_Status=rfmodelzozo$data$response)
#table(submit4$class,submit4$class_Status)
#mean(submit4$class==submit4$class_Status)




##############################################################################
################################   SVM   #####################################
##############################################################################

# Creating a learner
learner=makeLearner("classif.svm", predict.type="prob")

# Resampling
cv.svm=makeResampleDesc("CV", iters=3, stratify=TRUE)

# Random search
ctrl=makeTuneControlRandom(maxit=3)

# Tuning the SVM Parameters

param.svm=makeParamSet(
  makeDiscreteLearnerParam(id="type",values=c("C-classification", "nu-classification")),
  makeDiscreteLearnerParam(id="kernel", values=c("linear", "polynomial", "radial", "sigmoid")),
  makeNumericLearnerParam(id="cost", lower=1,upper=100, requires=quote(type == "C-classification")),
  makeNumericLearnerParam(id="nu", lower=0,upper=1, requires=quote(type == "nu-classification")),
  makeIntegerLearnerParam(id="degree", lower=1,upper=3 ,requires=quote(kernel == "polynomial")),
  makeNumericLearnerParam(id="gamma", lower=2^-3,upper=1, requires=quote(kernel != "linear")),
  makeLogicalLearnerParam(id="shrinking"))

# Searching the optimal parameters
# svm.res=tuneParams(learner, validateTask, resampling=cv.svm,
#                   par.set=param.svm, control=ctrl,measures=acc)

# Parameters optimal values
# svm.res$x
# Best : type=C-classification, kernel=radial, cost=25.8, gamma=0.573, shrinking=TURE
# acc.test.mean=0.9985397


# Asset the parmeters to our leaner
final_svm=setHyperPars(learner=learner, par.vals=list(type="C-classification", kernel="radial", cost=25.8, gamma=0.573, shrinking=FALSE))

# Training a model
svm.model=train(final_svm, trainTask)

# Making some predictions on the test set
predict.svm.test=predict(svm.model, testTask)

# Submission file
submit5=data.frame(class=test$class, class_status=predict.svm.test$data$response)
table(submit5$class,submit5$class_status)
mean(submit5$class==submit5$class_status)
# 0.9996508 de bonne classif
# Calculating the false/true positive rates on the test set & ploting the ROC Curve
rocsvm.test=generateThreshVsPerfData(predict.svm.test, measures = list(fpr, tpr, acc))
plotROCCurves(rocsvm.test)


# Making some predictions on a new datset
#predict.svm.zozo=predict(svm.model, zozoTask)

# Submission file
#submit5=data.frame(class=zozo$Class, class_status=predict.svm.zozo$data$response)
#table(submit5$class,submit5$class_status)
#mean(submit5$class==submit5$class_status)


#calculateROCMeasures(predict.svm)






##############################################################################
##########################   Gradient Boosting Machines   ####################
##############################################################################


# Loading GBM
g.gbm=makeLearner("classif.gbm", predict.type="prob")

# Specify the tuning method
rancontrol=makeTuneControlRandom(maxit=5)

# 3 fold CV
set_cv=makeResampleDesc("CV",iters=3)


# Set tunable parameters
gbm_par=makeParamSet(
                  makeDiscreteParam("distribution", values="bernoulli"),
                  makeIntegerParam("n.trees", lower=100, upper=500),
                  makeIntegerParam("interaction.depth", lower = 2, upper=10),
                  makeIntegerParam("n.minobsinnode", lower=10, upper=80),
                  makeNumericParam("shrinkage",lower=0.01, upper=1))

# n.minobsinnode refers to the minimum number of observations in a tree node
# shrinkage is the regulation parameter which dictates how fast / slow the algorithm should move

### Tuning parameters
# tune_gbm=tuneParams(learner = g.gbm, task = validateTask,resampling = set_cv,
#                    measures = acc,par.set = gbm_par,control = rancontrol)


# Checking CV accuracy
#tune_gbm$y
# acc.test.mean=0.9956825

# Best : distribution="bernoulli", n.trees=256, interaction.depth=5, n.minobsinnode=33, shrinkage=0.244

# Setting parameters
final_gbm=setHyperPars(learner=g.gbm,
                       par.vals=list(distribution="bernoulli", n.trees=256,
                                      interaction.depth=5, n.minobsinnode=33,
                                      shrinkage=0.244))



# Train
to.gbm=train(final_gbm, trainTask)

# Predicting on the test set
pr.gbm.test=predict(to.gbm, testTask)
# Submission file
submit6=data.frame(class = test$class, class_Status = pr.gbm.test$data$response)
table(submit6$class,submit6$class_Status)
mean(submit6$class==submit6$class_Status)
# 0.9973333
rocgbm.test=generateThreshVsPerfData(pr.gbm.test, measures = list(fpr, tpr, acc))
plotROCCurves(rocgbm.test)



# Predicting on a new dataset
#pr.gbm.zozo=predict(to.gbm, zozoTask)
# Submission file
#submit6=data.frame(class=zozo$Class, class_Status=pr.gbm.zozo$data$response)
#table(submit6$class,submit6$class_Status)
#mean(submit6$class==submit6$class_Status)



##############################################################################
################################   XGBoost   #################################
##############################################################################


# XGBoost is a decision-tree-based ensemble Machine Learning algorithm
# that uses a gradient boosting framework.


# Make learner with inital parameters
xg_set=makeLearner("classif.xgboost", predict.type = "prob")
xg_set$par.vals=list(objective = "binary:logistic",
                     eval_metric = "error",
                     nrounds = 250)

# Defining parameters for tuning
xg_ps=makeParamSet(
          makeIntegerParam("nrounds",lower=200,upper=500),
          makeIntegerParam("max_depth",lower=3,upper=20),
          makeNumericParam("lambda",lower=0.55,upper=0.60),
          makeNumericParam("eta", lower = 0.001, upper = 0.5),
          makeNumericParam("subsample", lower = 0.10, upper = 0.80),
          makeNumericParam("min_child_weight",lower=1,upper=5),
          makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8))


# Defining search function
rancontrol=makeTuneControlRandom(maxit=5)

# 3 fold cross validation
set_cv=makeResampleDesc("CV",iters=3)

# Tuning parameters
#xg_tune=tuneParams(learner=xg_set, task=validateTask, resampling=set_cv,measures=acc,par.set=xg_ps, control=rancontrol)

# Setting parameters
xg_new=setHyperPars(learner=xg_set, par.vals=list(nrounds=256,max_depth=20,lambda=0.56,eta=0.278,subsample=0.56,min_child_weight=3.84,colsample_bytree=0.683))

# parameters optimal values
#xg_new$x
#acc.test.mean=0.9978836


# Train model
xgmodel=train(xg_new, trainTask)


# Predicting on the test set
predict.xg.test=predict(xgmodel, task=testTask)
# Submission file
submit7=data.frame(class = test$class, class_Status = predict.xg.test$data$response)
table(submit7$class,submit7$class_Status)
mean(submit7$class==submit7$class_Status)
# 0.9991429
rocxgb.test=generateThreshVsPerfData(predict.xg.test, measures = list(fpr, tpr, acc))
plotROCCurves(rocxgb.test)



####################################################################################
################### Comparing the Roc Curves for the test dataset ##################
####################################################################################

roc_compare=generateThreshVsPerfData(list(Logistic=pred1.test,Tree=treetestpred,
                                          Random_Forest=rfmodeltest,SVM=predict.svm.test,
                                          Gradient_Boosting=pr.gbm.test,XGBoost=predict.xg.test),
                                          list(fpr, tpr))
plotROCCurves(roc_compare)

# Arrêter la parallélisation.
parallelStop()