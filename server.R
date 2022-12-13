#Final Project 
#Richard Xiao


library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(psych)
library(shiny)
library(shinydashboard)
library(DescTools)
library(summarytools)
library(corrr)
library(DT)
library(caret)
library(dplyr)
#initialization and data preparation
trctrl <- trainControl(method = "cv" , number = 5)
life <-read_csv("C:/Users/richa/OneDrive/Documents/ST 558/Final Project/Final-Project/Life Expectancy Data.csv")
life <- life %>% rename(life_expectancy = `Life expectancy`, adult_mortality = `Adult Mortality`, infant_deaths = `infant deaths`, under_five_deaths = `under-five deaths`, hepatitis = `Hepatitis B`, hiv_aids = `HIV/AIDS`)

Africa <-  c("Algeria","Angola", "Benin","Botswana", "Burkina Faso", "Central African Republic", "Burundi", "Côte d'Ivoire", "Cabo Verde", "Cameroon", "Chad", "Comoros", "Congo", "Democratic Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea","Eritrea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Somalia", "Senegal", "Seychelles", "Sierra Leone", "South Africa
", "South Sudan", "Sudan", "Togo", "Tunisia", "Uganda", "Swaziland", "United Republic of Tanzania","Zambia","Zimbabwe")
Asia <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei Darussalam", "Cambodia", "China", "Cyprus", "Democratic People's Republic of Korea", "Georgia", "India", "Indonesia", "Iran (Islamic Republic of)
", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar", "Nepal", "Oman", "Pakistan", "Philippines", "Qatar", "Republic of Korea", "Saudi Arabia", "Singapore", "Sri Lanka", "Syrian Arab Republic", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", "Turkmenistan","United Arab Emirates","Uzbekistan", "Viet Nam", "Yemen")

Europe <- c("Albania", "Austria","Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Montenegro", "Netherlands", "Monaco", "Norway", "Poland", "Portugal", "Republic of Moldova", "Romania", "Russian Federation", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Switzerland", "Sweden", "The former Yugoslav republic of Macedonia", "Ukraine", "United Kingdom of Great Britain and Northern Ireland
")

south_america <- c("Argentina", "Bolivia (Plurinational State of)", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela (Bolivarian Republic of)")

north_america <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Canada","Costa Rica", "Cuba", "Dominican Republic", "El Salvador", "Grenada", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines
", "Trinidad and Tobago", "United States of America")


life <- life %>% mutate(continent = ifelse(Country %in% Africa, "Africa",ifelse(Country %in% Asia,"Asia", ifelse(Country %in% Europe, "Europe", ifelse(Country %in% south_america, "South America", ifelse(Country %in% north_america, "North America", "Oceania"))))))

life<- na.omit(life)
new_life <- data.frame(life)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #Note, here for my image, I made a file called www in my finalproject folder with my server and ui files.
  output$image <- renderImage({ list(src = "www/kaggle_img.jpg",
                                     width = "100%",
                                     height = 330)
    
  }, deleteFile = F)
  #Data Exploration Page
  get_choices <- reactive({
    df_choices <- list(x = input$x_variable,y = input$y_variable, group = input$group, color = input$color)
  })
  
  #initializes variables for box and scatterplot.
  output$expPlot <- renderPlot({
    df_plot <- get_choices()
    
    if(input$plot_type == "scatter"){
      g_exp <- ggplot(life, aes_string(x = df_plot$x, y = df_plot$y)) +
        geom_point(aes_string(color = df_plot$color))
    }
    if(input$plot_type == "boxplot"){
      g_exp <- ggplot(life, aes(x = continent, y = adult_mortality, color = continent)) +
        geom_boxplot(fill="grey") + 
        geom_jitter()
    }
    
    g_exp
  })
  output$expTable <- renderDataTable({life})
  
  output$continentPlot <- renderPlot({
    cont_plot <- ggplot(new_life, aes(x = continent)) +
      geom_bar(fill = "steelblue") 
    cont_plot
  })
  
  #Declares table subsetted by continent.
  output$continentTable <- renderTable({
    tab_continent <- table(new_life$continent)
    tab_continent
  })
  
  
  
  output$countryPlot <- renderPlot({
    graph <- ggplot(new_life, aes(x = Country)) +
      geom_bar(fill = "steelblue") 
    graph
  })
  
  #Declares table subsetted by country.
  output$countryTable <- renderTable({
    tab_country <- table(new_life$Country)
    tab_country
  })
  
  output$measlesPlot <- renderPlot({
    graph <- ggplot(new_life, aes(x = Measles)) +
      geom_bar(fill = "steelblue") 
    graph
  })
  
  #Declares table subsetted by measles.
  output$measlesTable <- renderTable({
    tab_measles <- table(new_life$Measles)
    tab_measles
  })
  
  output$polioPlot <- renderPlot({
    graph <- ggplot(new_life, aes(x = Polio)) +
      geom_bar(fill = "steelblue") 
    graph
  })
  
  #Declares table subsetted by polio
  output$polioTable <- renderTable({
    tab_polio <- table(new_life$Polio)
    tab_polio
  })
  
  output$HIVPlot <- renderPlot({
    graph <- ggplot(new_life, aes(x = hiv_aids)) +
      geom_bar(fill = "steelblue") 
    graph
  })
  
  #Declares table subsetted by polio
  output$HIVTable <- renderTable({
    tab_hiv <- table(new_life$hiv_aids)
    tab_hiv
  })
  
  output$hepatitisPlot <- renderPlot({
    graph <- ggplot(new_life, aes(x = hepatitis)) +
      geom_bar(fill = "steelblue") 
    graph
  })
  
  #Declares table subsetted by polio
  output$hepatitisTable <- renderTable({
    tab_hepatitis <- table(new_life$hepatitis)
    tab_hepatitis
  })
  
  
  
  
  output$ex1 <- renderUI({
    withMathJax(
      helpText("The  linear regression equation is as follows: $$Y= b_0+b_1X_1+b_2X_2+...+ b_kX_k $$")
    )
  })
  
  output$ex2 <- renderUI({
    withMathJax(
      helpText("To get the mean squared error (MSE): $$MSE=1/N*\\Sigma(f_i-y_i)^2$$")
      
    )
  })
  
  output$ex3 <- renderUI({
    withMathJax(
      helpText("For random forest, to calculate the Gini index, it would be $$Gini = 1 - \\Sigma(P_i)^2$$")
    )
  })
  
  train_split <- eventReactive(input$analysis,{
    input$split / 100
  })
  select_mtry <- eventReactive(input$analysis,{
    input$mtry
  })
  select_cp <- eventReactive(input$analysis,{
    input$cp
  })
  #Creating the split index for use
  set.seed(666)  
  trainingRowIndex <-
    eventReactive(input$analysis,{
      sample(1:nrow(new_life),
             round(train_split() * nrow(new_life)))
    })
  #Creating the training dataset
  trainingData <- eventReactive(input$analysis,{
    new_life[trainingRowIndex(), ]
  })
  #Creating test dataset
  testData <- eventReactive(input$analysis,{
    new_life[-trainingRowIndex(), ]
  })
  #Formula for prediction
  model_var <- eventReactive(input$analysis,{
    reformulate(input$preds, input$outcome)
  })
  #Linear Regression
  model_reg <- reactive ({
    train(model_var(), data = trainingData(),
          preProcess = c("center", "scale"),
          method = "lm",
          trControl = trctrl)
  })
  #Random Forest
  model_rand <- reactive ({
  train(model_var(), data = trainingData(),
  preProcess = c("center", "scale"),
  method = "rf",
  tuneGrid = data.frame(mtry =1:select_mtry()),
  trControl = trctrl)
})
#Regression Tree
model_tree <- reactive ({
train(model_var(), data = trainingData,
preProcess = c("center", "scale"),
method = "rpart",
tuneGrid = data.frame(cp = seq(0, select_cp(), 0.001)),
trControl = trctrl)
})
output$reg <- renderPrint(summary(model_reg()))
output$rand_table <- renderPrint(summary(model_rand()))
output$tree_table <- renderPrint(summary(model_tree()))
#output$rand_imp <- renderPrint(varImp(model_rand(), scale = FALSE))
#output$tree_imp <- renderPrint(varImp(model_tree(), scale = FALSE))

reg_pred <- reactive ({
predict(model_reg(), newdata = testData())
})
rand_pred <- reactive ({
predict(model_rand(), newdata = testData())
})
tree_pred <- reactive ({
predict(model_tree(), newdata = testData())
})
#Results on test data
output$rmse_reg <- renderPrint(postResample(reg_pred(), testData()$life_expectancy))
output$rmse_rand <- renderPrint(postResample(rand_pred(), testData()$life_expectancy))
output$rmse_tree <- renderPrint(postResample(tree_pred(), testData()$life_expectancy))

#Downloads file


output$downloadfile <- downloadHandler(
  filename = function() {
    paste("life_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(new_life, file)
  }
)



})


