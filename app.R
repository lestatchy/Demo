#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("Demo.R")



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Histogram"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 10,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     
      
      # draw the histogram with the specified number of bins
      ggplot(Strgy, aes(x=Gain)) +
        geom_histogram(aes(y=..density..), colour="black", fill="white",bins=input$bins) + 
        xlab("Distribution of Weekly Gain")+
        scale_x_continuous(breaks=seq(round(min(Strgy$Gain),2),
                                      round(max(Strgy$Gain),2),
                                       by=round((max(Strgy$Gain)-min(Strgy$Gain))/5,2)
        )) +
        geom_vline(aes(xintercept=mean(Gain, na.rm=T)),   # Ignore NA values for mean
                    color="red", linetype="dashed", size=1)+
       geom_density(alpha=.2, fill="#FF6666")
     
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

