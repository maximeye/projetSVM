library(e1071)
library(smotefamily)
library(ggplot2)
library(rgl)
library(misc3d)
library(ROCR)
library(leaps)
library(caTools)

#Chargement de la table de donn??es :

#dat=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/creditcard.csv",header=T,sep=",")
dat=read.csv("C:/Users/kevas/Desktop/Cours/M2/Support_Vector_Machine/Dossier_SVM/creditcard.csv",header=T,sep=",")


attach(dat)

# On change le type de la variable de réponse "Class" (integer -> factor)
dat$Class=as.factor(dat$Class)

# Process de sélection de variables les plus significatives
regs=regsubsets(Class~.,data=dat,nvmax = 10)
summary(regs)
# V17, V12 et V14 sont les variables les plus significatives
dat.signif=dat[,c(18,15,13,31)]

# On change le type de la variable de réponse "Class" (integer -> factor)
dat.signif$Class=as.factor(dat.signif$Class)





n=10000
dat.signif=dat.signif[1:n,]

set.seed(123456)
dat.signif.split=sample.split(dat.signif,SplitRatio = 0.3)

# subsetting into Train data
train =subset(dat.signif,dat.signif.split==FALSE)

# subsetting into Test data
test =subset(dat.signif,dat.signif.split==TRUE)









#R????chantillonnage afin d'obtenir 50% de class = 0, 50% de class=1

newdat <- SMOTE(dat[,1:3],dat[,4],K=3,dup_size = 0)

#Transformation du type de variable de class en factor (auparavant character)
newdat$data$class=as.factor(newdat$data$class)

#Exportation de la nouvelle table en CSV (optionnelle), utile pour ne pas avoir ?? refaire l'??tape de r????chantillonnage chaque fois

write.csv(newdat$data,"/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/newdat.csv")
head(newdat)
table(newdat$Class)

###########################DEBUT#####################
#Chargement de la table r??echantillonn??e

data=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/newdat.csv",header=T,sep=",")
data$class=as.factor(data$class)



#Creation d'un ??chantillon d'apprentissage (70%) et test (30%) :
################NE PAS EXECUTER CE QUI SUIT  ############

#index=1:nrow(data)
#testindex=sample(index,397740)
#train=data[testindex,]
#test=data[-testindex,]
#attach(train)

###########################################################


##Echantillon apprentissage pour faire tourner le svm rapidement
#taille_ech=10000
#index=1:nrow(data)
#testindex=sample(index,round(taille_ech*0.7))
#train=data[testindex,]
#itest=sample(index,round(taille_ech*0.3))
#test=data[itest,]
#test=data[-testindex,]
#attach(train)

#SVM


model=svm(class~.,data=train,kernel="radial",scale=F,cost=105)


#Taux de bonnes/mauvaises classifications sur ??chantillon test:

Y=predict(model,newdata = test)

#list(cbind(test, Y), model$index)

table(test$class,Y)
mean(test$class==Y)
mean(test$class!=Y)



#Plot ??chantillon APPRENTISSAGE

colors =c("blue","red")
p3d<- plot3d(train$V12, train$V14, train$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

text3d(train$V12, train$V14, train$V17, cex=0.5, adj = 1)

#Plot ??chantillon de TEST
colors =c("blue","red")
p3d<- plot3d(test$V12, test$V14, test$V17, xlab="V12", ylab="V14",
             zlab="V17",type="s",radius =0.3,
             col=as.integer(train$class) ,
             box=FALSE, size=5)

text3d(test$V12, test$V14, test$V17, cex=0.5, adj = 1)





################################################################




################################################################



library(e1071)
library(rgl)
library(misc3d)

n    = 100
nnew = 50

# Simulate some data
set.seed(12345)
group = sample(2, n, replace=T)
dat   = data.frame(group=factor(group), matrix(rnorm(n*3, rep(group, each=3)), ncol=3, byrow=T))

# Fit SVM
fit = svm(group ~ ., data=dat)

# Plot original data
plot3d(dat[,-1], col=dat$group)

# Get decision values for a new data grid
newdat.list = lapply(train[,-c(1,5)], function(x) seq(min(x), max(x),len=nnew))
newdat      = expand.grid(newdat.list)
newdat.pred = predict(model, newdata=newdat, decision.values=T)
Y=predict(model,newdata = test,decision.values = T)
newdat.dv   = attr(Y, 'decision.values')
newdat.dv   = array(newdat.dv, dim=rep(nnew,3))

# Fit/plot an isosurface to the decision boundary
contour3d(newdat.dv, level=-1, x=newdat.list$V12, y=newdat.list$V14, z=newdat.list$V17, add=T)



##########

c=t(model$coefs) %*% model$SV
detalization=100
g=expand.grid(seq(from=min(test$V12),to=max(test$V12),length.out=detalization),                                                                                                         
              seq(from=min(test$V17),to=max(test$V17),length.out=detalization),
              seq(from=min(test$V14),to=max(test$V14),length.out=detalization) )
z <- (model$rho- c[1,2]*g[,1] - c[1,4]*g[,2]) / c[1,3]


plot3d(g[,1],g[,2],g[,3])  # this will draw plane.
# adding of points to the graphics.
points3d(test$V12[which(test$class==0)], test$V17[which(test$class==0)], test$V14[which(test$class==0)], col='red')
points3d(test$V12[which(test$class==1)], test$V17[which(test$class==1)], test$V14[which(test$class==1)], col='blue')



#w <- t(svm_model$coefs) %*% svm_model$SV

#detalization <- 100                                                                                                                                                                 
#grid <- expand.grid(seq(from=min(test$V12),to=max(test$V12),length.out=detalization),                                                                                                         
              #      seq(from=min(test$V17),to=max(test$V17),length.out=detalization))                                                                                                         
#z <- (svm_model$rho- w[1,1]*grid[,1] - w[1,2]*grid[,2]) / w[1,3]

plot3d(grid[,1],grid[,2],z)  # this will draw plane.
# adding of points to the graphics.
points3d(t$x[which(t$cl==-1)], t$y[which(t$cl==-1)], t$z[which(t$cl==-1)], col='red')
points3d(t$x[which(t$cl==1)], t$y[which(t$cl==1)], t$z[which(t$cl==1)], col='blue')


tune()
dataaa=cbind(data[itest,],Y)
pp=dataaa$class
ll=dataaa$labels
p=performance(Y,)



dataa=data[1:60000,]
Y=predict(model,newdata = dataa)
table(dataa$class,Y)
mean(dataa$class==Y)
mean(dataa$class!=Y)
