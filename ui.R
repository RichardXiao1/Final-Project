#Project Final
#Richard Xiao
#

library(shiny)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(shinycssloaders)
library(dplyr)


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "midnight",
                    
                    dashboardHeader(title="Final Project: Life Expectancy and Mortality Rates  Dashboard",titleWidth=1000),
                    
                    #define sidebar items
                    dashboardSidebar(sidebarMenu(
                      menuItem("About", tabName = "about", icon = icon("user")),
                      
                      menuItem("Data Exploration", tabName = "exp",icon = icon("square-poll-vertical")),
                      menuItem("Modeling", tabName = "model",icon = icon("chalkboard-user"),
                               menuSubItem("Modeling Info", tabName = "info")
                               
                      ),
                      menuItem("Data", tabName = "data",icon = icon("download"))
                    )),
                    
                    #define the body of the app
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "about",
                                fluidRow(
                                  #add in latex functionality if needed
                                  withMathJax(),
                                  h1("Purpose"),
                                  p("This app allows a user to find information about life expectancy and mortality rates in different countries/continents and other various factors(alcohol, measles and BMI). The link for the data is https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who "),
                                  h1("Pages"),
                                  h3("Variable Information"),
                                  p("This page contains a tab for each variable in the dataset. In each tab, the user can find a definition of the variable along with summary statistics."),
                                  h3("Data Exploration"),
                                  p("This page allows the user to select variables, as well as either scatterplot or boxplot analysis."),
                                  h3("Modeling"),
                                  p("This page holds three tabs: modeling info, model fitting, and prediction."),
                                  p("In the first tab, the user will be given a description of each of the three models they can create along with pros and cons of each."),
                                  p("In the second tab, the user will be able to create models by specifying the proportion of data to be used as the training set, and the predictor variables. After creation, the user will be able to find summaries and graphs."),
                                  p("In the final tab, the user will be able to submit values in for the predictors and return a predicted value."),
                                  h3("Data"),
                                  p("Finally, the Data page will allow the user to  download the data as a .csv file."),
                                  imageOutput("image")
                                )
                        ),
                        
                        tabItem(tabName = "exp",
                                fluidRow(
                                  h1("Data Exploration"),
                                  h5("Selection."),
                                  varSelectInput("x_variable", "X  Variables:", select(life, where(is.numeric)))),
                                varSelectInput("y_variable", "Y  Variables:", select(life, where(is.numeric))),
                                selectInput("plot_type","Plot Type:", c(scatter = "scatter", boxplot = "boxplot")
                                ),
                                varSelectInput("group","Group by:",select(life, where(is.factor))),
                                selectInput("color","Color:",c(Continent = "continent", Status = "Status" 
                                )),
                                selectInput("death","Death rate:",c(None = "NULL", Continent = "continent" 
                                )),
                                
                                
                                plotOutput("expPlot", heigh = "600px"),
                                dataTableOutput("expTable")
                        ) 
                      ),
                      tabItem(tabName = "continent",
                              fluidRow(
                                h1("Definition"),
                                h4("Continent stats"),
                                plotOutput("continentPlot"),
                                tableOutput("continentTable")
                              )
                      ),
                      tabItem(tabName = "country",
                              fluidRow(
                                h1("Definition"),
                                h4("Country stats"),
                                plotOutput("countryPlot"),
                                tableOutput("countryTable")
                              )
                      ),
                      
                      tabItem(tabName = "info", fluidRow( h1("Model Comparison"),
                      p("Random forests is a specialized form of bagging.It selects a subset of predictors.Random Forests have an advantage  on no overfitting, reduces variance and is also more accurate compared to other methods.However, due to a large number of trees it incorporates, it is too slow and can be ineffective at times.
                      Better than bagged trees since if a strong predictor exists, it won't dominate the tree fits. You would use m = p/3 for regression where p is 
                        number of predictors.Forward fitting regression is a method where we start with an empty data set and gradually add in more variables.It is computationally efficient but 
                        typically it has worse performace compared to other methods. Finally, for regression trees, it is a method where the predictor space 
                        is split into regions with different predictions for each region. They are used to predict a continuous response. They require less effort for data preparation during pre-processing and is easy to understand and explain. However
                        , small change in the data can cause large change in the output, which makes it unstable to use compared with other methods.")), withMathJax(),uiOutput("ex1"),uiOutput("ex2"),uiOutput("ex3")),
                      tabItem(tabName = "fit",
                              fluidRow(
                                column(width = 12,
                                       box(
                                         selectInput(
                                           "preds",
                                           label = "Select variables:",
                                           choices = names(life),
                                           multiple = TRUE,
                                           selected = c("Alcohol", "Measles", "BMI", "Polio","Hepatitis", "HIV")
                                         ),
                                         solidHeader = TRUE,
                                         width = 2,
                                         status = "primary",
                                         title = "Predictor Variables"
                                       ),
                                       box(
                                         selectInput("outcome", label = "Select variable:", "life_expectancy"),
                                         solidHeader = TRUE,
                                         width = 2,
                                         status = "primary",
                                         title = "Outcome Variable",
                                         selected = c("life_expectancy", "adult_mortality", "infant_deaths", "under_five")
                                       ),
                                       box(
                                         sliderInput("split", label = h3("Percent of Data for Training:"),min = 50,max = 95,value = 75),
                                         solidHeader = TRUE,
                                         width = 2,
                                         status = "primary",
                                         title = "Splitting the Data"
                                       ),
                                       box(
                                         sliderInput("mtry",label = h3("Number of Randomly Selected Predictors for Random Forest."),min = 1,max = 15,value = 3),
                                         solidHeader = TRUE,
                                         width = 2,
                                         status = "primary",
                                         title = "Random Forest Parameter"
                                       ),
                                       box(
                                         sliderInput("cp",label = h3("Complexity Parameter for Regression Tree."),min = 0,max = .1,value = .005),
                                         solidHeader = TRUE,
                                         width = 2,
                                         status = "primary",
                                         title = "Regression Tree Parameter"
                                       ),
                                       actionButton("analysis", "Analyze!")
                                ),
                                box(width = 6,
                                    h3("Summary Information"),
                                    withSpinner(verbatimTextOutput("reg")),
                                    #h3("RMSE from Predicting on the Test Data"),
                                    #withSpinner(verbatimTextOutput("rmse_reg")),
                                    title = "Linear Regression",
                                    solidHeader = TRUE,
                                    status = "primary"
                                ),
                                box(width = 6,
                                    h3("Summary Information"),
                                    withSpinner(tableOutput("rand_table")),
                                    h3("Variable Importance"),
                                    h4("Note: Output"),
                                    withSpinner(verbatimTextOutput("rand_imp")),
                                    h3("RMSE from Predicting on the Test Data"),
                                    withSpinner(verbatimTextOutput("rmse_rand")),
                                    title = "Random Forest",
                                    solidHeader = TRUE,
                                    status = "primary"
                                ),
                                box(width = 6,
                                    h3("Summary Information"),
                                    withSpinner(tableOutput("tree_table")),
                                    h3("Variable Importance"),
                                    withSpinner(verbatimTextOutput("tree_imp")),
                                    h3("RMSE from Predicting on the Test Data"),
                                    withSpinner(verbatimTextOutput("rmse_tree")),
                                    title = "Regression Tree",
                                    solidHeader = TRUE,
                                    status = "primary"
                                    
                                ),
                                downloadButton("downloadfile","Download Table as a .csv"),
                                box(
                                  title = "Life Analysis",
                                  dataTableOutput("data_table"),
                                  width = 12
                                )
                              ))
                    
                    
                    
                    
                    
                    
))
