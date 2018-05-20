#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyUI(navbarPage(title = "DEMO",
           tabPanel("Plot",
                    fluidPage(
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput(inputId = "bins",
                                    label = "Number of bins:",
                                    min = 10,
                                    max = 50,
                                    value = 30),
                        dateRangeInput(inputId = "dateRange",
                                    label = "Strategy duration:",
                                    start = '2009-01-06',
                                    end = '2017-06-25',
                                    min = '2009-01-06',
                                    max = '2017-07-25'),
                        verbatimTextOutput("stats")
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        plotOutput("hist"),
                        plotOutput("totalReturn")
                      )
                    )
                    )),
           tabPanel("Bootstrapped Data",
                    fluidPage(
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
                          plotOutput("trend")
                        )
                      )
                    ))
))















