#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #Data Exploration Page
  get_choices <- reactive({
    df_choices <- list(x = input$x_variable,y = input$y_variable, group = input$group, color = input$color)
    
  })
  
  output$expPlot <- renderPlot({
    df_plot <- get_choices()
    # plot_type <- switch(input$plot_type,
    #                    "scatter" = geom_point(),
    #                   "boxplot" 	= geom_boxplot(),
    #                   "histogram" =	geom_histogram(alpha=0.5,position="identity"),
    #                  "density" 	=	geom_density(alpha=.75),
    #                  "bar" 		=	geom_bar(position="dodge")
    #                  )
    if(input$plot_type == "scatter"){
      g_exp <- ggplot(life, aes_string(x = df_plot$x, y = df_plot$y, color = df_plot$color)) +
        geom_point(stat = "identity", position = "jitter")
    }
    if(input$plot_type == "boxplot"){
      g_exp <- ggplot(life, aes(x = continent, y = adult_mortality, color = continent)) +
        geom_boxplot(fill="grey") + 
        geom_jitter()
    }
    
    g_exp
  })
  output$expTable <- renderDataTable({life})

    })


