#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)




shinyUI(navbarPage(title = "DEMO",
                   tabPanel("Description",
                            fluidPage(
                              sidebarLayout(
                                sidebarPanel(
                                  # uiOutput('markdown')
                                  includeHTML("Description.Rhtml")
                                ),
                                mainPanel(
                                  imageOutput("flowchart"),
                                  downloadButton("downloadExcel", "Download the Dataset")
                                )
                              )
                              
                            )),
           tabPanel("Historical Performance",
                    fluidPage(
                    verticalLayout(
                      wellPanel(
                        dateRangeInput(inputId = "dateRange",
                                    label = "Strategy duration:",
                                    start = '2009-01-06',
                                    end = '2017-06-25',
                                    min = '2009-01-06',
                                    max = '2017-07-25'),
                        verbatimTextOutput("stats"),
                        sliderInput(inputId = "bins",
                                    label = "Number of bins:",
                                    min = 10,
                                    max = 50,
                                    value = 30)
                      ),
                      
                      # Show a plot of the generated distribution
                      # mainPanel(
                        plotOutput("hist"),
                        plotlyOutput("totalReturn")
                      # )
                    )
                    )),
           
           tabPanel("Lookback",
                    fluidPage(
                      titlePanel("Performance"),
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          dateInput(inputId = "refDate", 
                                    label = "Reference date", 
                                    value = "2017-07-01"),
                          sliderInput(inputId = "Lookback",
                                      label = "Trade starts from:",
                                      min = as.Date("2009-01-06"),
                                      max = as.Date("2017-01-06"),
                                      value = as.Date("2009-07-25"))
                        ),
                      
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Total Return",plotlyOutput("totalReturn2")),
                            tabPanel("Distribution",plotOutput("hist3")),
                            tabPanel("Drawdowns",plotOutput("hist4"))
                          )
                        )
                      
                      )
                    )),
        
           tabPanel("Bootstrapped Data",
                    fluidPage(
                      # uiOutput('markdown'),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "nweeks",
                                      label = "Investment horizon(weeks):",
                                      min = 52,
                                      max = 408,
                                      value = c(60, 400))
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("hist2"),
                          plotlyOutput("trend")
                        )
                      )
                    ))
           # tabPanel("Spreadsheets",
           #          fluidPage(
           #            dataTableOutput('table')
           #          ))
))















