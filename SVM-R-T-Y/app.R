library(shiny)
library(e1071)
library(smotefamily)
library(ggplot2)
library(rgl)
library(misc3d)
library(ROCR)
library(leaps)
library(caTools)
source("Projet R.R")




ui <- fluidPage(
    
(navbarPage(title="SUPPORT VECTOR MACHINE",

# Application title
tabPanel("Data presentation",

# Sidebar with a slider input for number of bins
sidebarLayout(
sidebarPanel(
fileInput(inputId='File', label='make your choice', multiple = TRUE, buttonLabel = 'Browse...',placeholder = 'not file selected'),
actionButton(inputId="click",label="MANUAL",helpText='Please open the manual to understand how the application works')
),

# Show a plot of the generated distribution
mainPanel(
textOutput('presentation'),
tableOutput("dim"),
tableOutput("name"),
dataTableOutput('sum')

)
)
),
tabPanel("Description of SVM",
sidebarLayout(
sidebarPanel(
selectInput(inputId="kernel1",label="choose the kernel type",choices=c('linear','polynomial','radial basis','sigmoid'),multiple = F,selected = 'linear'),
numericInput(inputId='degree1', label='degree',value=3,min=0),
numericInput(inputId='coef01', label='b',value=0,min=0),
sliderInput(inputId='c1', label='C',min=1,max=100,value=50,step=1),
actionButton("submit1" ,"submit", icon("refresh"))
),
mainPanel(
plotOutput('plot1'),
textOutput('explication1'),
textOutput('sv1'),
plotOutput('plot2'),
textOutput('explication2')
)
)
)
,
tabPanel("SVM's Parameters settings",
sidebarLayout(
sidebarPanel(
selectInput(inputId="kernel",label="choose the kernel type",choices=c('linear','polynomial','radial basis','sigmoid'),multiple = F,selected = 'linear'),
sliderInput(inputId='c', label='Cost parameter',min=0,max=1000,value=10,step=1),
sliderInput(inputId="n","Sample size:",min = 1,max = 600000, value=10000),
actionButton("submit" ,"submit", icon("refresh"))
),
mainPanel(
dataTableOutput("tablesvm"),
textOutput('text1'),
textOutput("predsvm"),
)
)
)
,
tabPanel("Comparison",
sidebarLayout(
sidebarPanel(
sliderInput(inputId="n1","Sample size:",min = 1,max = 600000, value=10000),
selectInput(inputId='Model', label='choose the model', choices=c('logistic regression','linear discriminant analysis','quadratic discriminant analysis','tree') ,multiple = F, selected='logistic regression'),
selectInput(inputId="kernel2",label="choose the kernel type",choices=c('linear','polynomial','radial basis','sigmoid'),multiple = F,selected = 'linear'),
sliderInput(inputId='c2', label='Cost parameter',min=0,max=1000,value=10,step=1),
actionButton("submit2" ,"submit", icon("refresh"))
),
mainPanel(
dataTableOutput("table"),
textOutput("text"),
textOutput("pred"),
textOutput("conclusion")
)
)
)
)))





server <- function(input, output) {

observeEvent(input$click,{file.show("D:/PC/M2/SVM/projet/projet_final/notice.html")})
##observeEvent(input$File,{ data=read.csv(input$File,header=T,sep=",")
data1=read.csv("/Volumes/CALAMITY/PC/M2/SVM/projet/creditcard.csv",header=T,sep=",")
attach(data1)
set.seed(12345)
# On change le type de la variable de reponse "Class" (integer -> factor)
data1$Class=as.factor(data1$Class)
# Process de selection de variables les plus significatives
regs=regsubsets(Class~.,data=data1,nvmax = 10)
# V17, V12 et V14 sont les variables les plus significatives
dat=data1[,c(18,15,13,31)]
# On change le type de la variable de reponse "Class" (integer -> factor)
dat$Class=as.factor(dat$Class)
# Reechantillonnage afin d'obtenir 50% de class = 0, 50% de class=1
newdat <- SMOTE(dat[,1:3],dat[,4],K=3,dup_size = 0)
#Transformation du type de variable de class en factor (auparavant character)
newdat$data$class=as.factor(newdat$data$class)
data=newdat$data
index=1:nrow(data)



gamma=0.01
C=10





#p1

output$presentation <- renderText({
    'The dataset presents transactions that occured in two days made by credit cards in September 2013 by european cardholders.
    We have first the number of observation, and after the number of variables. Then the names of variables and Statisitics of the dataset by variables selected among the most significant, and the variable of response. '
})
output$dim <- renderTable({
    dim(data1)
})
output$name <- renderTable({
    names(data)
})
output$sum <- renderDataTable({summary(data)
    
})










#p2

set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)
dat = data.frame(x, y = as.factor(y))
output$plot1 <- renderPlot({
    input$submit1
    if (input$kernel1=='linear'){
        svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = isolate(input$c1), scale = FALSE)
        plot(svmfit, dat)
    }
    if (input$kernel1=='polynomial'){
        svmfit = svm(y ~ ., data = dat, kernel = "polynomial", degree=isolate(input$degree1), coef0=isolate(input$coef01), cost = isolate(input$c1), scale = FALSE)
        plot(svmfit, dat)
    }
    if (input$kernel1=='radial basis'){
        svmfit = svm(y ~ ., data = dat, kernel = "radial", cost = isolate(input$c1), scale = FALSE)
        plot(svmfit, dat)
    }
    if (input$kernel1=='sigmoid'){
        svmfit = svm(y ~ ., data = dat, kernel = "sigmoid",coef0=isolate(input$coef01), cost = isolate(input$c1), scale = FALSE)
        plot(svmfit, dat)
    }
})
output$explication1 <- renderText({
    'We can observe that the SVM create a line who separate data in two parts, we have above points where realisations are 1 and below realisations wich are -1. And the number of support vector is for each side of the line :'
    
})
output$sv1 <- renderText({
    input$submit1
    if (input$kernel1=='linear'){
        svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = isolate(input$c1), scale = FALSE)
    }
    if (input$kernel1=='polynomial'){
        svmfit = svm(y ~ ., data = dat, kernel = "polynomial", degree=isolate(input$degree1), coef0=isolate(input$coef01), cost = isolate(input$c1), scale = FALSE)
    }
    if (input$kernel1=='radial basis'){
        svmfit = svm(y ~ ., data = dat, kernel = "radial", cost = isolate(input$c1), scale = FALSE)
    }
    if (input$kernel1=='sigmoid'){
        svmfit = svm(y ~ ., data = dat, kernel = "sigmoid",coef0=isolate(input$coef01), cost = isolate(input$c1), scale = FALSE)
    }
    svmfit$nSV
})
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = isolate(input$c1), scale = FALSE)
make.grid = function(x, n = 75) {
    grange = apply(x, 2, range)
    x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
    x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
    expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(x)
xgrid[1:10,]
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho

output$plot2 <- renderPlot({
    input$submit1
    if (input$kernel1=='linear'){
        plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
        points(x, col = y + 3, pch = 19)
        points(x[svmfit$index,], pch = 5, cex = 2)
        abline(beta0 / beta[2], -beta[1] / beta[2])
        abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
        abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)
    }
})







#p3






output$tablesvm <- renderDataTable({
    input$submit
    trainindex=sample(index,round(isolate(input$n)*0.7))
    train=data[trainindex,]
    attach(train)
    dataa=data[1:isolate(input$n),]
    model.linear=svm(class~.,data=train,kernel='linear',scale=F,cost= isolate(input$c))
    Y.linear=predict(model.linear,newdata = dataa)
    model.poly=svm(class~., data=train, kernel="polynomial",gamma = 0.01, cost = isolate(input$c))
    Y.poly=predict(model.poly,newdata = dataa)
    model.radial=svm(class~., data=train, kernel="radial",gamma = 0.01, cost = isolate(input$c))
    Y.radial=predict(model.radial,newdata = dataa)
    model.sigmoid=svm(class~., data=train, kernel="sigmoid",gamma = 0.01, cost = isolate(input$c))
    Y.sigmoid=predict(model.sigmoid,newdata = dataa)
    if (input$kernel=='linear'){
        table=table(dataa$class,Y.linear)}
    if (input$kernel=='polynomial'){
        table=table(dataa$class,Y.poly)}
    if (input$kernel=='radial basis'){
        table=table(dataa$class,Y.radial)}
    if (input$kernel=='sigmoid'){
        table=table(dataa$class,Y.sigmoid)}
    table
})

output$text1 <- renderText({input$submit
    'the percent of good prediction is : '
})

output$predsvm <- renderText({
    input$submit
    trainindex=sample(index,round(isolate(input$n)*0.7))
    train=data[trainindex,]
    dataa=data[1:isolate(input$n),]
    attach(train)
    if (input$kernel=='linear'){
        model.linear=svm(class~.,data=train,kernel='linear',scale=F,cost= isolate(input$c))
        Y.linear=predict(model.linear,newdata = dataa)
        ex.svm=mean(dataa$class==Y.linear)
    }
    if (input$kernel=='polynomial'){
        model.poly=svm(class~., data=train, kernel="polynomial",gamma = 0.01, cost = isolate(input$c))
        Y.poly=predict(model.poly,newdata = dataa)
        ex.svm=mean(dataa$class==Y.poly)
    }
    if (input$kernel=='radial basis'){
        model.radial=svm(class~., data=train, kernel="radial",gamma = 0.01, cost = isolate(input$c))
        Y.radial=predict(model.radial,newdata = dataa)
        ex.svm=mean(dataa$class==Y.radial)
    }
    if (input$kernel=='sigmoid'){
        model.sigmoid=svm(class~., data=train, kernel="sigmoid",gamma = 0.01, cost = isolate(input$c))
        Y.sigmoid=predict(model.sigmoid,newdata = dataa)
        ex.svm=mean(dataa$class==Y.sigmoid)
    }
    ex.svm
})















#p4




output$table <- renderDataTable({
    input$submit2
    trainindex=sample(index,round(isolate(input$n1)*0.7))
    train=data[trainindex,]
    dataa=data[1:isolate(input$n1),]
    attach(train)
    glm.fit=glm(data$class~data$V12+data$V14+data$V17,data=data,family=binomial)
    glm.probs=predict(glm.fit,type='response',dataa)
    glm.pred=rep(0,nrow(dataa))
    glm.pred[glm.probs>.5]=1
    lda.fit=lda(data$class~data$V12+data$V14+data$V17,data=data)
    lda.pred=predict(lda.fit, dataa)
    qda.fit=qda(data$class~data$V12+data$V14+data$V17,data=data)
    qda.pred=predict(qda.fit,dataa,type='class')
    if (input$Model=='logistic regression'){table=table(glm.pred,data$class)}
    if (input$Model=='linear discriminant analysis'){table=table(lda.pred$class,data$class)}
    if (input$Model=='quadratic discriminant analysis'){table=table(qda.pred$class,data$class)}
    table})
output$text <- renderText({'the percent of good prediction is : '})
output$pred <- renderText({
    input$submit2
    trainindex=sample(index,round(isolate(input$n1)*0.7))
    train=data[trainindex,]
    dataa=data[1:isolate(input$n1),]
    attach(train)
    glm.fit=glm(data$class~data$V12+data$V14+data$V17,data=data,family=binomial)
    glm.probs=predict(glm.fit,type='response',dataa)
    glm.pred=rep(0,nrow(dataa))
    glm.pred[glm.probs>.5]=1
    lda.fit=lda(data$class~data$V12+data$V14+data$V17,data=data)
    lda.pred=predict(lda.fit, dataa)
    qda.fit=qda(data$class~data$V12+data$V14+data$V17,data=data)
    qda.pred=predict(qda.fit,dataa,type='class')
    
    
    if (input$kernel2=='linear'){
        model.linear=svm(class~.,data=train,kernel='linear',scale=F,cost= isolate(input$c2))
        Y.linear=predict(model.linear,newdata = dataa)
        ex.svm=mean(dataa$class==Y.linear)
    }
    if (input$kernel2=='polynomial'){
        model.poly=svm(class~., data=train, kernel="polynomial",gamma = 0.01, cost = isolate(input$c2))
        Y.poly=predict(model.poly,newdata = dataa)
        ex.svm=mean(dataa$class==Y.poly)
    }
    if (input$kernel2=='radial basis'){
        model.radial=svm(class~., data=train, kernel="radial",gamma = 0.01, cost = isolate(input$c2))
        Y.radial=predict(model.radial,newdata = dataa)
        ex.svm=mean(dataa$class==Y.radial)
    }
    if (input$kernel2=='sigmoid'){
        model.sigmoid=svm(class~., data=train, kernel="sigmoid",gamma = 0.01, cost = isolate(input$c2))
        Y.sigmoid=predict(model.sigmoid,newdata = dataa)
        ex.svm=mean(dataa$class==Y.sigmoid)
    }
    
    
    if (input$Model=='logistic regression'){
        ex=mean(glm.pred==data$class)*100
        decision=ifelse(ex>ex.svm,"Selected model prefered","SVM prefered")
        output$conclusion <- renderText({decision})
    }
    if (input$Model=='linear discriminant analysis'){
        ex=mean(lda.pred$class==data$class)*100
        decision=ifelse(ex>ex.svm,"Selected model prefered","SVM prefered")
        output$conclusion <- renderText({decision})
    }
    if (input$Model=='quadratic discriminant analysis'){
        ex=mean(qda.pred$class==data$class)*100
        decision=ifelse(ex>ex.svm,"Selected model prefered","SVM prefered")
        output$conclusion <- renderText({decision})
    }
    ex})


## })



})
}





shinyApp(ui = ui, server = server)
