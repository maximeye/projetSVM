library(shiny)
library(e1071)
library(smotefamily)
library(ggplot2)
library(rgl)
library(misc3d)
library(ROCR)
library(leaps)
library(caTools)
library(httr)
#source("Projet R.R")


ui <- fluidPage(

#Titre Programme tout à gauche
(navbarPage(title="Machine Learning using SVM",

# 1er onglet :
tabPanel("Data presentation",

sidebarLayout(
sidebarPanel(
#actionButton(inputId="click",label="MANUAL",helpText='Please open the manual to understand how the application works')
),

mainPanel(
textOutput('presentation'),
tableOutput("dim"),
tableOutput("name"),
dataTableOutput('sum')

)
)
),












#Onglet 2 : 
tabPanel("Description of SVM",
sidebarLayout(
sidebarPanel(
sliderInput(inputId='n', label='Sample size',min=2,max=568199,value=10000,step=50),
selectInput(inputId="kernel",label="Choose a kernel",choices=c('Linear','Polynomial','Radial Basis','Sigmoid'),multiple = F,selected = 'Linear'),
sliderInput(inputId='cost', label='C',min=1,max=500,value=10,step=1),
sliderInput(inputId='gamma', label='Gamma',min=0,max=1,value=0.01,step=0.01),
actionButton("submit" ,"Refresh", icon("refresh"))
),
mainPanel(
plotOutput('plot1'),
textOutput('explication1')
)

            )
        )















,
#Onglet 3: 
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
textOutput("predsvm")
)
)
)
,
#Onglet 4: 
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


###############################SERVER####################################
########################################################################

library(shiny)
library(e1071)
library(smotefamily)
library(ggplot2)
library(rgl)
library(misc3d)
library(ROCR)
library(leaps)
library(caTools)
library(httr)
#source("Projet R.R")

server <- function(input, output) {

    notice="https://raw.githubusercontent.com/maximeye/projetSVM/master/notice.Rmd"
observeEvent(input$click,{file.show(url(notice))})
    
        file="https://raw.githubusercontent.com/maximeye/projetSVM/master/newdat.csv"
    data=read.csv(file=url(file),header=T,sep=",")
attach(data)
set.seed(12345)

data$class=as.factor(data$class)
index=(1:nrow(data))




#p1

output$presentation <- renderText({
    
})
output$dim <- renderTable({
    dim(data)
})
output$name <- renderTable({
    names(data)
})
output$sum <- renderDataTable({summary(data)
    
})
































#p2




output$plot1 <- renderPlot({

    
    taille_ech=input$n
    index=(1:nrow(data))
    trainindex=sample(index,round(taille_ech*0.7))
    train=data[trainindex,]
    itest=sample(index,round(taille_ech*0.3))
    test=data[itest,]
    attach(train)


    if (input$kernel=='Linear'){
        model=svm(class~.,data=train,kernel="linear",scale=F,cost=input$cost)
        plot(model,train,col=c("bisque","lightblue"))
    }
    else if (input$kernel=='Polynomial'){
        model=svm(class~., data=train, kernel="polynomial",gamma =input$gamma, cost =input$cost) 
        plot(model,train,col=c("bisque","lightblue"))
    }
    else if (input$kernel=='Radial Basis'){
        model=svm(class~., data=train, kernel="radial",gamma = input$gamma, cost =input$cost) 
        plot(model,train,col=c("bisque","lightblue"))
    }
    else if (input$kernel=='Sigmoid'){
        model=svm(class~., data=train, kernel="sigmoid",gamma = input$gamma, cost =input$cost) 
        plot(model,train,col=c("bisque","lightblue"))
    }
})
output$explication1 <- renderText({
    'We can observe that the SVM create a line who separate data in two parts, we have above points where realisations are 1 (fraud) and below realisations wich are 0 (no fraud). 
    The rate of good classification is :' 
    Y=predict(model,newdata = test)
    table(test$class,Y)
    mean(test$class==Y)
    
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



}






shinyApp(ui = ui, server = server)
