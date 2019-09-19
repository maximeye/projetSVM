library(e1071)
library(smotefamily)
library(ggplot2)
library(rgl)
library(misc3d)
library(ROCR)
library(leaps)
library(caTools)

#Chargement de la table de donnees :

dat=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/creditcard.csv",header=T,sep=",")
#dat=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/creditcard.csv",header=T,sep=",")


attach(dat)

# On change le type de la variable de reponse "Class" (integer -> factor)
dat$Class=as.factor(dat$Class)

# Process de selection de variables les plus significatives
set.seed(12345)

# On change le type de la variable de reponse "Class" (integer -> factor)
dat$Class=as.factor(dat$Class)

# Process de selection de variables les plus significatives
regs=regsubsets(Class~.,data=dat,nvmax = 10)
summary(regs)
# V17, V12 et V14 sont les variables les plus significatives
dat.signif=dat[,c(18,15,13,31)]

# On change le type de la variable de reponse "Class" (integer -> factor)
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






###########################DEBUT#####################

# Chargement de la table reechantillonnee

data=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/newdat.csv",header=T,sep=",")
data=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/newdat.csv",header=T,sep=",")
data$class=as.factor(data$class)
set.seed(12345)
data=data[,-1]

## Debut de partitionnage Apprentissage / Test


#Creation d'un echantillon d'apprentissage (70%) et test (30%) :
################NE PAS EXECUTER CE QUI SUIT  ############

#index=1:nrow(data)
#testindex=sample(index,397740)
#train=data[testindex,]
#test=data[-testindex,]
#attach(train)



## Echantillon apprentissage pour faire tourner le svm rapidement

taille_ech=10000
index=1:nrow(data)
trainindex=sample(index,round(taille_ech*0.7))
train=data[trainindex,]
itest=sample(index,round(taille_ech*0.3))
test=data[itest,]
attach(train)





###########################################################




###############  SVM kernel lineaire  ###############


model=svm(class~.,data=train,kernel="linear",scale=F,cost=105)
w <- t(model$coefs) %*% model$SV

#Taux de bonnes/mauvaises classifications sur echantillon test:

Y=predict(model,newdata = test)

#list(cbind(test, Y), model$index)

table(test$class,Y)
mean(test$class==Y)
mean(test$class!=Y)



#Taux de bonnes/mauvaises classifications sur echantillon complet:
dataa=data[1:60000,]
Y=predict(model,newdata = dataa)

#list(cbind(test, Y), model$index)

table(dataa$class,Y)
mean(dataa$class==Y)
mean(dataa$class!=Y)


#Plot echantillon APPRENTISSAGE

colors =c("blue","red")
p3d<- plot3d(train$V12, train$V14, train$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

text3d(train$V12, train$V14, train$V17, cex=0.5, adj = 1)

#Plot echantillon de TEST
colors =c("blue","red")
p3d<- plot3d(test$V12, test$V14, test$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

text3d(test$V12, test$V14, test$V17, cex=0.5, adj = 1)

length = 100                                                                                                                                                                 
grid = expand.grid(seq(from=min(train$V17),to=max(train$V17),length.out=length),                                                                                                         
                    seq(from=min(train$V14),to=max(train$V14),length.out=length))                                                                                                         
z = (model$rho- w[1,1]*grid[,1] - w[1,2]*grid[,2]) / w[1,3]
#z=(w[1,1]*grid[,1] + w[1,2]*grid[,2])

plot3d(grid[,1],grid[,2],z)  # this will draw the plane
# adding of points to the graphics.
points3d(train$V17[which(train$class==0)], train$V14[which(train$class==0)], train$V12[which(train$class==0)], col='red')
points3d(train$V17[which(train$class==1)], train$V14[which(train$class==1)], train$V12[which(train$class==1)], col='blue')




###############  Optimal Gamma/Cost  ###############


#10 Fold cross validation to determine the optimal gamma and C (cost) in the SVM
#Best parameters : Gamma : 0.01  / Cost (C) : 10
attach(train)
tuned = tune.svm(class~., data = train, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned)

gamma=0.01
C=10



###############  SVM kernel Radial Basis  ###############


model=svm(class~., data=train, kernel="radial",gamma = 0.01, cost = 10) 
w <- t(model$coefs) %*% model$SV


####Taux de bonne prediction sur echantillon test
Y=predict(model,newdata = test)
table(test$class,Y)
mean(test$class==Y)
mean(test$class!=Y)

####Taux de bonnes/mauvaises classifications sur echantillon complet:
dataa=data[1:60000,]
Y=predict(model,newdata = dataa)

table(dataa$class,Y)
mean(dataa$class==Y)
mean(dataa$class!=Y)


#Plot echantillon APPRENTISSAGE

colors =c("blue","red")
p3d<- plot3d(train$V12, train$V14, train$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

text3d(train$V12, train$V14, train$V17, cex=0.5, adj = 1)

#Plot echantillon de TEST
colors =c("blue","red")
p3d<- plot3d(test$V12, test$V14, test$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

text3d(test$V12, test$V14, test$V17, cex=0.5, adj = 1)




length = 50                                                                                                                                                                
grid = expand.grid(seq(from=min(train$V17),to=max(train$V17),length.out=length),                                                                                                         
                   seq(from=min(train$V14),to=max(train$V14),length.out=length))                                                                                                         
z= exp((-gamma)*(model$rho- w[1,1]*grid[,1] - w[1,2]*grid[,2])^2)

plot3d(grid[,1],grid[,2],z)

points3d(train$V17[which(train$class==0)], train$V14[which(train$class==0)], train$V12[which(train$class==0)], col='red')
points3d(train$V17[which(train$class==1)], train$V14[which(train$class==1)], train$V12[which(train$class==1)], col='blue')




###############  SVM kernel Polynomial  ###############


model=svm(class~., data=train, kernel="polynomial",gamma = 0.01, cost = 10) 
w <- t(model$coefs) %*% model$SV


####Taux de bonne prediction sur echantillon test
Y=predict(model,newdata = test)
table(test$class,Y)
mean(test$class==Y)
mean(test$class!=Y)

####Taux de bonnes/mauvaises classifications sur echantillon complet:
dataa=data[1:60000,]
Y=predict(model,newdata = dataa)

table(dataa$class,Y)
mean(dataa$class==Y)
mean(dataa$class!=Y)


#Plot echantillon APPRENTISSAGE

colors =c("blue","red")
p3d<- plot3d(train$V12, train$V14, train$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

text3d(train$V12, train$V14, train$V17, cex=0.5, adj = 1)

#Plot echantillon de TEST
colors =c("blue","red")
p3d<- plot3d(test$V12, test$V14, test$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

text3d(test$V12, test$V14, test$V17, cex=0.5, adj = 1)




length = 100                                                                                                                                                                 
grid = expand.grid(seq(from=min(train$V17),to=max(train$V17),length.out=length),                                                                                                         
                   seq(from=min(train$V14),to=max(train$V14),length.out=length))                                                                                                         
z=((gamma*(w[1,1]*grid[,1] + w[1,2]*grid[,2])+model$coef0))^2

plot3d(grid[,1],grid[,2],z)  # this will draw plane.
# adding of points to the graphics.
points3d(train$V17[which(train$class==0)], train$V14[which(train$class==0)], train$V12[which(train$class==0)], col='red')
points3d(train$V17[which(train$class==1)], train$V14[which(train$class==1)], train$V12[which(train$class==1)], col='blue')


prediction=predict(model, test[,-4])
(tab=table(pred = prediction, true=test[,4]))




###############  SVM kernel sigmoid  ###############

model=svm(class~., data=train, kernel="sigmoid",gamma = 0.01, cost = 10) 
w <- t(model$coefs) %*% model$SV


####Taux de bonne prediction sur echantillon test
Y=predict(model,newdata = test)
table(test$class,Y)
mean(test$class==Y)
mean(test$class!=Y)

####Taux de bonnes/mauvaises classifications sur echantillon complet:
dataa=data[1:60000,]
Y=predict(model,newdata = dataa)

table(dataa$class,Y)
mean(dataa$class==Y)
mean(dataa$class!=Y)


#Plot echantillon APPRENTISSAGE

colors =c("blue","red")
p3d<- plot3d(train$V12, train$V14, train$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

text3d(train$V12, train$V14, train$V17, cex=0.5, adj = 1)

#Plot echantillon de TEST
colors =c("blue","red")
p3d<- plot3d(test$V12, test$V14, test$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

text3d(test$V12, test$V14, test$V17, cex=0.5, adj = 1)




length = 100                                                                                                                                                                 
grid = expand.grid(seq(from=min(train$V17),to=max(train$V17),length.out=length),                                                                                                         
                   seq(from=min(train$V14),to=max(train$V14),length.out=length))                                                                                                         

z=tanh((gamma*(w[1,1]*grid[,1] + w[1,2]*grid[,2]))+model$coef0)
plot3d(grid[,1],grid[,2],z)

points3d(train$V17[which(train$class==0)], train$V14[which(train$class==0)], train$V12[which(train$class==0)], col='red')
points3d(train$V17[which(train$class==1)], train$V14[which(train$class==1)], train$V12[which(train$class==1)], col='blue')




#Polynomial / Lineaire : OK
#RBF / Sigmoid NON
