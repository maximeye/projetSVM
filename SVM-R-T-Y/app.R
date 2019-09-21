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
    
    titlePanel("SVM's Parameters settings"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="k",label = 'Choice the Kernel used in the SVM',
                        choices=c('Linear',"Radial Basis", "Polynomial","Sigmoid"),multiple=FALSE, selected='Linear'),
            sliderInput("n","Sample size:",min = 1,max = 600000, value=10000),
            
            sliderInput("C", "Cost parameter", min = 0, max = 1000,value = 10),
           
        ),
        mainPanel(
            plotOutput("Scatterplot")
        )
    )

   
)





server <- function(input, output) {

    values<- reactiveValues(x="")
    observeEvent({input$k
        input$n},
        
        {k=input$k
        if (input$k=="Linear") {values$x="linear"}
        if (input$k=="Radial Basis") {values$x="radial"}
        if (input$k=="Polynomial") {values$x="polynomial"}
        if (input$k=="Sigmoid") {values$x="sigmoid"}

        }
    )
}
    
    output$Scatterplot <- renderPlot({
  
        dat=read.csv("/Users/Maxime/Documents/Cours/Master/M2/S1/SVM/Docs Projet/creditcard.csv",header=T,sep=",")
        attach(dat)
        dat$Class=as.factor(dat$Class)
        set.seed(12345)
        dat$Class=as.factor(dat$Class)
        dat.signif$Class=as.factor(dat.signif$Class)
        newdat <- SMOTE(dat.signif[,1:3],dat.signif[,4],K=3,dup_size = 0)
        
        newdat$data$class=as.factor(newdat$data$class)
        
        data=newdat$data
        data=data[,-1]
        
        taille_ech=input$n
        index=1:nrow(data)
        trainindex=sample(index,round(taille_ech*0.7))
        train=data[trainindex,]
        itest=sample(index,round(taille_ech*0.3))
        test=data[itest,]
        attach(train)
        ker=input$k
        
        gam=0.01
        C=10
        
        length = 100                                                                                                                                                                 
        grid = expand.grid(seq(from=min(train$V17),to=max(train$V17),length.out=length),                                                                                                         
                           seq(from=min(train$V14),to=max(train$V14),length.out=length))
        
        
        
        model=svm(class~.,data=train,kernel=kernel,scale=F,cost=105)
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
SV=function(ker){
      
        if ker="linear" {
            z = (model$rho- w[1,1]*grid[,1] - w[1,2]*grid[,2]) / w[1,3]
        }
        else if ker="radial" {
            z= exp((-gamma)*(model$rho- w[1,1]*grid[,1] - w[1,2]*grid[,2])^2)
            
        }
        else if ker="polynomial" {
            z=((gamma*(w[1,1]*grid[,1] + w[1,2]*grid[,2])+model$coef0))^2
            
        }
        else if ker="sigmoid" {
            z=tanh((gamma*(w[1,1]*grid[,1] + w[1,2]*grid[,2]))+model$coef0)
            
        }
    
}
        
        plot3d(grid[,1],grid[,2],z)  
       
        points3d(train$V17[which(train$class==0)], train$V14[which(train$class==0)], train$V12[which(train$class==0)], col='red')
        points3d(train$V17[which(train$class==1)], train$V14[which(train$class==1)], train$V12[which(train$class==1)], col='blue')
        
        
        
        
        
    })
    

}






shinyApp(ui = ui, server = server)
