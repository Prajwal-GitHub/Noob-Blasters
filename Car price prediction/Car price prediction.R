getwd()
library(shiny)

ui = fluidPage(headerPanel("Car Selling Price Prediction"),
               sidebarPanel(
                 textInput("name","Car Name :- ",""),
                 textInput("year","Manufacturing Year :- ",""),
                 textInput("km_driven","Kms Car has driven :- ",""),
                 selectInput("fuel_user","Choose fuel type",list("For Petrol: 5",
                                                                 "For Diesel: 2",
                                                                 "For CNG: 1")),
                 textInput("fuel","Fuel Type :- ",""),
                 selectInput("seller_user","Choose seller type",list("If Individual: 2",
                                                                     "If Dealer: 1")),
                 textInput("seller_type","Individual/Dealer :- ",""),
                 selectInput("transmission_user","Choose Transmission",list("For Manual: 2",
                                                                                 "For Automatic: 1")),
                 textInput("transmission","Transmission type :- ",""),
                 selectInput("owner_user","Choose Owner Number",list("For First: 1",
                                                                     "For Second: 3",
                                                                     "For Third: 5")),
                 textInput("owner","Owner No :- ",""),
                 actionButton('go',"Predict")
               ),
               mainPanel(
                 sidebarPanel(width = 25,
                              headerPanel("The Predection of Car Price is:- "),
                              textOutput("value"))
               ))

server <- function(input, output) {
  
  data2 = reactiveValues()
  observeEvent(input$go,{
    
    data_set = read.csv("CAR DETAILS FROM CAR DEKHO.csv")
    View(data_set)
    
    str(data_set)
    
    data_set$name <- as.factor(data_set$name)
    data_set$name_id <- sapply(data_set$name,as.numeric)
    View(data_set$name_id)
    
    data_set$fuel <- as.factor(data_set$fuel)
    data_set$fuel_id <- sapply(data_set$fuel,as.numeric)
    View(data_set$fuel_id)
    
    data_set$seller_type <- as.factor(data_set$seller_type)
    data_set$seller_type_id <- sapply(data_set$seller_type,as.numeric)
    View(data_set$seller_type_id)
    
    data_set$transmission <- as.factor(data_set$transmission)
    data_set$transmission_id <- sapply(data_set$transmission,as.numeric)
    View(data_set$transmission_id)
    
    data_set$owner <- as.factor(data_set$owner)
    data_set$owner_id <- sapply(data_set$owner,as.numeric)
    View(data_set$owner_id)
    
    pre_final_ds <- cbind(data_set,name_id2=data_set$name_id,fuel_id2=data_set$fuel_id,
                          seller_type_id2=data_set$seller_type_id,
                          transmission_id2=data_set$transmission_id,
                          owner_id2=data_set$owner_id)
    View(pre_final_ds)
    
    final_ds <- pre_final_ds[,c('name','name_id2','fuel','fuel_id2','km_driven','year',
                                'seller_type','seller_type_id2','transmission',
                                'transmission_id2','owner','owner_id2','selling_price')]
    View(final_ds)
    
    final_ds2 = final_ds[,c("name_id2","year","km_driven","fuel_id2","seller_type_id2","transmission_id2","owner_id2","selling_price")]
    View(final_ds2)
    
    data2$myname <- as.numeric(input$name)
    data2$myyear <- as.numeric(input$year)
    data2$mykm_driven <- as.numeric(input$km_driven)
    data2$myfuel <- as.numeric(input$fuel)
    data2$myseller_type <- as.numeric(input$seller_type)
    data2$mytransmission <- as.numeric(input$transmission)
    data2$myowner <- as.numeric(input$owner)
    
    newPredict = data.frame(name_id2 = data2$myname,year = data2$myyear, km_driven = data2$mykm_driven,
                            fuel_id2 = data2$myfuel, seller_type_id2 = data2$myseller_type,
                            transmission_id2 = data2$mytransmission, owner_id2 = data2$myowner)
    
    modelLM = lm(selling_price ~ name_id2+fuel_id2+year+km_driven+seller_type_id2+transmission_id2+owner_id2,
                 data = final_ds2)
    
    data2$op = predict(modelLM, newPredict)
  })
  
  output$value <- renderPrint({data2$op})
}

shinyApp(ui, server)
