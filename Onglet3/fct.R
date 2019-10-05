options(shiny.error = browser)



SVM=function(kernel){
  if (kernel=='Linear'){
    model=svm(class~.,data=train,kernel="linear",scale=F,cost=input$cost)
    pl=plot(model,train,col=c("bisque","lightblue"))
  }
  else if (kernel=='Polynomial'){
    model=svm(class~., data=train, kernel="polynomial",gamma =input$gamma, cost =input$cost) 
    pl=plot(model,train,col=c("bisque","lightblue"))
  }
  else if (kernel=='Radial Basis'){
    model=svm(class~., data=train, kernel="radial",gamma = input$gamma, cost =input$cost) 
    pl=plot(model,train,col=c("bisque","lightblue"))
  }
  else {
    model=svm(class~., data=train, kernel="sigmoid",gamma = input$gamma, cost =input$cost) 
    pl=plot(model,train,col=c("bisque","lightblue"))
  }
  return(pl)
}

SVM(input$kernel)