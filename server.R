#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret) #for machine learning
library("ggplot2") #for diamonds dataset
library("mgcv")
library(Metrics)



# Define server logic 
shinyServer(function(input, output) {
    #Load diamonds dataset
    data("diamonds")
    
    #simplify dataset by filtering out diamonds of perfect clarity
    diamonds <- diamonds[diamonds$clarity == "VS2",]
    
    #Keep only columns of interest
    diamonds <- diamonds[,c("price","carat","cut","color")]
    diamonds$cut <- factor(diamonds$cut, ordered = F)
    diamonds$color <- factor(diamonds$color, ordered = F)
    
    #encode categorical values
    dmy <- dummyVars("~ .", data = data.frame(diamonds), fullRank = F)
    diamondsD <- data.frame(predict(dmy, newdata = diamonds))
    
    #do train-test split
    set.seed(123)
    index <- createDataPartition(diamondsD$price, p=0.75, list=FALSE)
    trainSet <- diamondsD[index,]
    testSet <- diamondsD[-index,]
    

    #set model parameters
    trctrl <- trainControl(method = "cv", number = 5)
        
    #train model
    knn_fit <- train(price ~., data = trainSet,
                    method = "knn",
                    trControl=trctrl,
                    #preProcess = c("center", "scale")
    )
    train_pred_knn <- predict(knn_fit, newdata = trainSet)
    
    test_pred_knn <- predict(knn_fit, newdata = testSet)
    pred_knn <- data.frame(Predictions = test_pred_knn, True_price = testSet$price, Carat = testSet$carat)
    
    ggplot(data = pred_knn)+
        geom_point(data = pred_knn, 
                   aes(x=True_price, y = Predictions,
                       size = Carat))
    
    #Initialize input dataframe
    my_input <- as.data.frame(rbind(c(NaN, NaN, NaN, NaN)), stringsAsFactors = T)
    colnames(my_input) <- c("price", "carat", "cut", "color")
    
    
    my_inputD_pred <- reactive({ 
        #collect inputs
        my_input[1,"carat"] <- input$Carat
        my_input[1,"cut"] <- input$Cut
        my_input[1,"color"] <- input$Color
    
        #ensure inputs are of the correct format
        my_input$carat <- as.numeric(my_input$carat)
        my_input$cut <- factor(my_input$cut, levels = levels(diamonds$cut))
        my_input$color <- factor(my_input$color, levels = levels(diamonds$color))
    
        #convert input to dummy variables
        my_inputD <- data.frame(predict(dmy, newdata = my_input))
    
        #Calculate the predicted price of the input diamond
        round(predict(knn_fit, newdata = my_inputD), digits = 0)
    })
    #return the predicted price
    output$text1 = renderText({
        my_inputD_pred()
        })

    
})
