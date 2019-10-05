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


#Chargement de la table de donnees :

#dat=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/creditcard.csv",header=T,sep=",")
dat=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/creditcard.csv",header=T,sep=",")


attach(dat)

# On change le type de la variable de reponse "Class" (integer -> factor)
dat$Class=as.factor(dat$Class)


#### Process de selection de variables les plus significatives ####
set.seed(12345)

# On change le type de la variable de reponse "Class" (integer -> factor)
dat$Class=as.factor(dat$Class)

# Process de selection de variables les plus significatives
regs=regsubsets(Class~.,data=dat,nvmax = 10)
summary(regs)
# V17, V12 et V14 sont les variables les plus significatives
dat.signif=dat[,c(18,15,13,31)]


# On change le type de la variable de reponse "Class" (integer -> factor)
dat.signif$Class=as.factor(dat.signif$Class)



# Reechantillonnage afin d'obtenir 50% de class = 0, 50% de class=1

newdat <- SMOTE(dat.signif[,1:3],dat.signif[,4],K=3,dup_size = 0)

#Transformation du type de variable de class en factor (auparavant character)
newdat$data$class=as.factor(newdat$data$class)



#Exportation de la nouvelle table en CSV (optionnelle), utile pour ne pas avoir a refaire l'etape de reechantillonnage chaque fois

#write.csv(newdat$data,"/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/newdat.csv")
write.csv(newdat$data,"C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/newdat.csv")
table(newdat$data$class) # 284315 Class=0
                         # 283884 Class=1






                ########################### DEBUT ###########################

# Chargement de la table reechantillonnee

data=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/newdat.csv",header=T,sep=",")
data=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/projetSVM/newdat.csv",header=T,sep=",")

data$class=as.factor(data$class)
set.seed(12345)
#data=data[,-c(1,4)]


#### Debut de partitionnage Apprentissage / Test ####


#Creation d'un echantillon d'apprentissage (70%) et test (30%) :
################NE PAS EXECUTER CE QUI SUIT  ############

#index=1:nrow(data)
#trainindex=sample(index,397740)
#train=data[trainindex,]
#test=data[-trainindex,]
#attach(train)



## Echantillon apprentissage pour faire tourner le svm rapidement

taille_ech=10000      #nrow(data)
index=1:nrow(data)
trainindex=sample(index,round(taille_ech*0.7))
train=data[trainindex,]
itest=sample(index,round(taille_ech*0.3))
test=data[itest,]
attach(train)



###Logistic Regression

# The glm() function fits generalized glm()linear models, a class of models
# that includes logistic regression.


glm.fit=glm(class~V17+V14,data=data,family=binomial)
glm.probs=predict(glm.fit,type='response',test)
glm.pred=rep(0,nrow(test))
glm.pred[glm.probs>.5]=1

table(glm.pred,test$class)
mean(glm.pred==test$class) #91,2% de bonnes classifications
mean(glm.pred!=test$class) #8,8% de mauvaises classifications


library(ROCR)
roclogit=predict(glm.fit,newdata=test,type="response")
predlogit=prediction(roclogit,test[,"class"])
perflogit=performance(predlogit, "tpr","fpr")
plot(perflogit,colorize=TRUE,lwd=2)

library(pROC)
pROC_obj <- roc(predlogit@labels[[1]],predlogit@predictions[[1]],
                # arguments for ci
                ci=TRUE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE)
plot(ci.sp(pROC_obj, sensitivities=seq(0, 1, .05)), type="shape")

sens.ci <- ci.se(pROC_obj,specificities = seq(0,1,.05))
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

library(ROCit)
ROCit_obj=rocit(score=predlogit@predictions[[1]],class=predlogit@labels[[1]],method='bi')
ROCCI_obj=ciROC(ROCit_obj)

plot(ROCCI_obj,YIndex=FALSE,level=0.95)
plot(ROCCI_obj, col = c(2,9))
plot(ROCCI_obj, col = c(2,9), legendpos = "bottom", lty = c(1,3))


### Linear Discriminant Analysis

lda.fit=lda(class~V17+V14,data=data)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit, test)
names(lda.pred)
lda.class =lda.pred$class
table(lda.class ,test$class)

mean(lda.class==test$class) #89,5% de bonnes classifications
mean(lda.class!=test$class) #10,5% de mauvaises classifications


roclda=predict(lda.fit,newdata=test,type="response")
predlda=prediction(roclda$posterior[,2],test[,"class"])
perflda=performance(predlda, "tpr","fpr")
plot(perflda,colorize=TRUE,lwd=2)

pROC_obj <- roc(predlda@labels[[1]],predlda@predictions[[1]],
                # arguments for ci
                ci=TRUE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE)
plot(ci.sp(pROC_obj, sensitivities=seq(0, 1, .03)), type="shape")
sens.ci <- ci.se(pROC_obj,specificities = seq(0,1,.03))
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

library(ROCit)
ROCit_obj=rocit(score=predlda@predictions[[1]],class=predlda@labels[[1]],method='bi')
ROCCI_obj=ciROC(ROCit_obj)

plot(ROCCI_obj,YIndex=FALSE,level=0.95)
plot(ROCCI_obj, col = c(2,9))
plot(ROCCI_obj, col = c(2,9), legendpos = "bottom", lty = c(1,3))




### Quadratic Discriminant Analysis

qda.fit=qda(class~V17+V14,data=data)
qda.fit
qda.class=predict(qda.fit,test)$class
table(qda.class,test$class)

mean(qda.class==test$class) #91,8% de bonnes classifications
mean(qda.class!=test$class) #8,2% de mauvaises classifications


rocqda=predict(qda.fit,newdata=test,type="response")
predqda=prediction(rocqda$posterior[,2],test[,"class"])
perfqda=performance(predqda, "tpr","fpr")
plot(perfqda,colorize=TRUE,lwd=2)

pROC_obj <- roc(predqda@labels[[1]],predqda@predictions[[1]],
                # arguments for ci
                ci=TRUE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE)
plot(ci.sp(pROC_obj, sensitivities=seq(0, 1, .03)), type="shape")
sens.ci <- ci.se(pROC_obj,specificities = seq(0,1,.03))
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")



library(ROCit)
ROCit_obj=rocit(score=predqda@predictions[[1]],class=predqda@labels[[1]],method='bi')
ROCCI_obj=ciROC(ROCit_obj)

plot(ROCCI_obj,YIndex=FALSE,level=0.95)
plot(ROCCI_obj, col = c(2,9))
plot(ROCCI_obj, col = c(2,9), legendpos = "bottom", lty = c(1,3))



######################################################################################
######################################################################################



                    ###############  SVM kernel lineaire  ###############


model=svm(class~.,data=train,kernel="linear",scale=F,cost=100)
w = t(model$coefs) %*% model$SV

# Taux de bonnes/mauvaises classifications sur echantillon TEST :
Y=predict(model,newdata = test)

table(test$class,Y)
mean(test$class==Y) #91,4% de bonne classification
mean(test$class!=Y) #8,6% de mauvaise classification

#list(cbind(test, Y), model$index)


# Taux de bonnes/mauvaises classifications sur echantillon COMPLET :
dataa=data[,] # Ici, on choisit le nombre de lignes qui d?finiera l'?chantillon complet
Y=predict(model,newdata = dataa)

table(dataa$class,Y)
mean(dataa$class==Y) # 92,3% de bonne classification
mean(dataa$class!=Y) # 7,7% de mauvaise classification

#list(cbind(test, Y), model$index)



# Plot echantillon d'APPRENTISSAGE

colors =c("blue","red")
p3d<- plot3d(train$V12, train$V14, train$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

#text3d(train$V12, train$V14, train$V17, cex=0.5, adj = 1)


###Plot3D


nnew = 5
newdat.list = lapply(data[,-c(1,5)], function(x) seq(min(x), max(x), len=nnew))
newdat      = expand.grid(newdat.list)
Y=predict(model,newdata = test)
newdat.dv   = attr(Y, 'decision.values')
newdat.dv   = array(newdat.dv, dim=rep(nnew, 3))

# Fit/plot an isosurface to the decision boundary
contour3d(newdat.dv, level=0, x=newdat.list$V17, y=newdat.list$V14, z=newdat.list$V12, add=T)


####

# Plot echantillon de TEST
colors =c("blue","red")
p3d<- plot3d(test$V12, test$V14, test$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

# text3d(test$V12, test$V14, test$V17, cex=0.5, adj = 1)


length = 100                                                                                                                                                                 
grid = expand.grid(seq(from=min(train$V17),to=max(train$V17),length.out=length),                                                                                                         
                    seq(from=min(train$V14),to=max(train$V14),length.out=length))                                                                                                         
z = (model$rho - w[1,1]*grid[,1] - w[1,2]*grid[,2])
#z=(w[1,1]*grid[,1] + w[1,2]*grid[,2])


# Drawing the plane &nd adding points to the graphic
plot3d(grid[,1],grid[,2],z)

points3d(train$V17[which(train$class==0)], train$V14[which(train$class==0)], train$V12[which(train$class==0)], col='red')
points3d(train$V17[which(train$class==1)], train$V14[which(train$class==1)], train$V12[which(train$class==1)], col='blue')

prediction=predict(model, test[,-4])
(tab=table(pred = prediction, true=test[,4]))



##########  Optimal Gamma/Cost (Ne pas executer a chaque session, on a deja les resultats) ##########


# 10 Fold cross validation to determine the optimal gamma and C (cost) in the SVM

attach(train)
tuned = tune.svm(class~., data = train, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned)

# Best parameters : Gamma : 0.01  / Cost (C) : 10
gamma=0.01
C=10



                    ###############  SVM kernel Radial Basis  ###############


model=svm(class~., data=train, kernel="radial",gamma = 0.01, cost = 10) 
w = t(model$coefs) %*% model$SV


#### Taux de bonne prediction sur echantillon TEST
Y=predict(model,newdata = test)
table(test$class,Y)
mean(test$class==Y) # 91,4% de bonne classification
mean(test$class!=Y) # 8,6% de mauvaise classification


#### Taux de bonnes/mauvaises classifications sur echantillon COMPLET :
dataa=data[,] # Ici, on choisit le nombre de lignes qui d?finiera l'?chantillon complet
Y=predict(model,newdata = dataa)

table(dataa$class,Y)
mean(dataa$class==Y) # 92,2% de bonne classification
mean(dataa$class!=Y) # 7,8% de mauvaise classification


# Plot echantillon d'APPRENTISSAGE

colors =c("blue","red")
p3d = plot3d(train$V12, train$V14, train$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

# text3d(train$V12, train$V14, train$V17, cex=0.5, adj = 1)

# Plot echantillon de TEST
colors =c("blue","red")
p3d = plot3d(test$V12, test$V14, test$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

# text3d(test$V12, test$V14, test$V17, cex=0.5, adj = 1)




length = 100
grid = expand.grid(seq(from=min(train$V17),to=max(train$V17),length.out=length),                                                                                                         
                   seq(from=min(train$V14),to=max(train$V14),length.out=length))  
z= exp((-0.000001)*(((w[1,1]*grid[,1]) - (w[1,2]*grid[,2]))^2))



# Drawing the plane &nd adding points to the graphic
plot3d(grid[,1],grid[,2],z,aspect = T)


points3d(train$V17[which(train$class==0)], train$V14[which(train$class==0)], train$V12[which(train$class==0)], col='red')
points3d(train$V17[which(train$class==1)], train$V14[which(train$class==1)], train$V12[which(train$class==1)], col='blue')

prediction=predict(model, test[,-4])
(tab=table(pred = prediction, true=test[,4]))




                    ###############  SVM kernel Polynomial  ###############


model=svm(class~., data=train, kernel="polynomial",gamma = 0.01, cost = 10) 
w = t(model$coefs) %*% model$SV


#### Taux de bonne prediction sur echantillon TEST
Y=predict(model,newdata = test)
table(test$class,Y)
mean(test$class==Y) # 64,6% de bonne classification
mean(test$class!=Y) # 35,4% de mauvaise classification

#### Taux de bonnes/mauvaises classifications sur echantillon COMPLET :
dataa=data[,]  # Ici, on choisit le nombre de lignes qui d?finiera l'?chantillon complet
Y=predict(model,newdata = dataa)

table(dataa$class,Y)
mean(dataa$class==Y) # 63,4% de bonne classification
mean(dataa$class!=Y) # 36,6% de mauvaise classification


# Plot echantillon d'APPRENTISSAGE

colors =c("blue","red")
p3d = plot3d(train$V12, train$V14, train$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

# text3d(train$V12, train$V14, train$V17, cex=0.5, adj = 1)

# Plot echantillon de TEST
colors =c("blue","red")
p3d = plot3d(test$V12, test$V14, test$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(test$class) ,
             box=FALSE, size=5)

# text3d(test$V12, test$V14, test$V17, cex=0.5, adj = 1)




length = 100                                                                                                                                                                 
grid = expand.grid(seq(from=min(train$V17),to=max(train$V17),length.out=length),                                                                                                         
                   seq(from=min(train$V14),to=max(train$V14),length.out=length))                                                                                                         
z=((gamma*(w[1,1]*grid[,1] + w[1,2]*grid[,2])+model$coef0))^2

# Drawing the plane &nd adding points to the graphic
plot3d(grid[,1],grid[,2],z)  

points3d(train$V17[which(train$class==0)], train$V14[which(train$class==0)], train$V12[which(train$class==0)], col='red')
points3d(train$V17[which(train$class==1)], train$V14[which(train$class==1)], train$V12[which(train$class==1)], col='blue')


prediction=predict(model, test[,-4])
(tab=table(pred = prediction, true=test[,4]))




                    ###############  SVM kernel sigmoid  ###############

model=svm(class~., data=train, kernel="sigmoid",gamma = 0.01, cost = 10) 
w = t(model$coefs) %*% model$SV


#### Taux de bonne prediction sur echantillon TEST
Y=predict(model,newdata = test)
table(test$class,Y)
mean(test$class==Y) # 91,5% de bonne classification
mean(test$class!=Y) # 8,5% de mauvaise classification

#### Taux de bonnes/mauvaises classifications sur echantillon COMPLET :
dataa=data[,] # Ici, on choisit le nombre de lignes qui d?finiera l'?chantillon complet
Y=predict(model,newdata = dataa)

table(dataa$class,Y)
mean(dataa$class==Y) # 92,2% de bonne classification
mean(dataa$class!=Y) # 7,8% de mauvaise classification


# Plot echantillon d'APPRENTISSAGE

colors=c("blue","red")
p3d = plot3d(train$V12, train$V14, train$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

# text3d(train$V12, train$V14, train$V17, cex=0.5, adj = 1)

# Plot echantillon de TEST
colors=c("blue","red")
p3d = plot3d(test$V12, test$V14, test$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

# text3d(test$V12, test$V14, test$V17, cex=0.5, adj = 1)




length = 100                                                                                                                                                                 
grid = expand.grid(seq(from=min(train$V17),to=max(train$V17)),                                                                                                         
                   seq(from=min(train$V14),to=max(train$V14)))                                                                                                         

z=tanh((gamma*(w[1,1]*grid[,1] + w[1,2]*grid[,2]))+model$coef0)

# Drawing the plane &nd adding points to the graphic
plot3d(grid[,1],grid[,2],z,aspect = F)

points3d(train$V17[which(train$class==0)], train$V14[which(train$class==0)], train$V12[which(train$class==0)], col='red')
points3d(train$V17[which(train$class==1)], train$V14[which(train$class==1)], train$V12[which(train$class==1)], col='blue')

prediction=predict(model, test[,-4])
(tab=table(pred = prediction, true=test[,4]))





#############PASSAGE EN 2D

data=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/newdat.csv",header=T,sep=",")
data=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/projetSVM/newdat.csv",header=T,sep=",")
data$class=as.factor(data$class)
set.seed(12345)
data=data[,-c(1,4)]



taille_ech=15000
index=1:nrow(data)
trainindex=sample(index,round(taille_ech*0.7))
train=data[trainindex,]
itest=sample(index,round(taille_ech*0.3))
test=data[itest,]
attach(train)

###LINEAIRE
model=svm(class~.,data=train,kernel="linear",scale=F,cost=100)
###RBF
model=svm(class~., data=train, kernel="radial",gamma = 0.01, cost = 10) 
###POLYNOMIAL
model=svm(class~., data=train, kernel="polynomial",gamma = 0.01, cost = 10) 
###SIGMOID
model=svm(class~., data=train, kernel="sigmoid",gamma = 0.01, cost = 10) 





# Taux de bonnes/mauvaises classifications sur echantillon TEST :
Y=predict(model,newdata = test)

table(test$class,Y)
mean(test$class==Y) 
mean(test$class!=Y) 



#PLOT

plot(train$V17,train$V14,type="p",col=c("blue","red")[train$class])

beta0 = model$rho
beta1 = sum(model$coefs*train$V17[model$index])
beta2 = sum(model$coefs*train$V14[model$index])

##Marges
#abline(-beta0/beta2,-beta1/beta2,col="green")
#abline((-beta0-5.0)/beta2,-beta1/beta2,col="gray")
#abline((-beta0+5.0)/beta2,-beta1/beta2,col="gray")


plot(model,train,col=c("bisque","lightblue"))


###Test read.csv repo
?httr
library(httr)
file="https://raw.githubusercontent.com/maximeye/projetSVM/master/newdat.csv"
data=read.csv(file=url(file),header=T,sep=",")






