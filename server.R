#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



library(shiny)
library(ggplot2)
library(moments)
source("Demo.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  dat <- reactive({
    # Strgy[(as.Date(Strgy$Date)>=input$dateRange[1])&(as.Date(Strgy$Date)<=input$dateRange[2]),]
    OutSample = Dat[(as.Date(Dat$Date)>=input$dateRange[1]) & (as.Date(Dat$Date)<=input$dateRange[2]),]
    Strgy = data.frame(Date = OutSample$Date, Position = rep(0,nrow(OutSample)),Gain = rep(0,nrow(OutSample)),TR = rep(1,nrow(OutSample)) )
    for (i in seq(1,nrow(Strgy)-1)) {
      x = OutSample$Position[i]
      lb = OutSample$LB[i]
      ub = OutSample$UB[i]
      pos = ifelse(x<lb,1,ifelse(x>ub,-1,Strgy$Position[i]))
      gain = -pos*OutSample$Libor[i]/10000+pos*(OutSample$Swap[i+1]/OutSample$Swap[i]-1)
      totalRet = Strgy$TR[i]*(1+gain)
      Strgy$Position[i+1] = pos
      Strgy$Gain[i+1] = gain
      Strgy$TR[i+1] = totalRet
    }
  })
  dataTable <- reactive(
    data.frame(Days = difftime(input$dateRange[2],input$dateRange[1]),
               Mean = mean(x$Gain),Median = median(x$Gain),
               Skewness = skewness(x$Gain), Kurtosis = kurtosis(x$Gain)
  ))
  output$hist <- renderPlot({
    # generate bins based on input$bins from ui.R
    # draw the histogram with the specified number of bins
    ggplot(dat(), aes(x=Gain)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white",bins=input$bins) + 
      xlab("Distribution of Weekly Gain")+
      scale_x_continuous(breaks=seq(round(min(x$Gain),2),
                                    round(max(x$Gain),2),
                                    by=round((max(x$Gain)-min(x$Gain))/5,2)
      )) +
      geom_vline(aes(xintercept=mean(Gain, na.rm=T)),   # Ignore NA values for mean
                 color="red", linetype="dashed", size=1)+
      geom_density(alpha=.2, fill="#FF6666")
    
    
  })
  output$stats <- renderPrint({
    print(dataTable(),row.names = FALSE,digits = 2)
    })
  
  output$totalReturn <- renderPlot({
    x = Strgy[(as.Date(Strgy$Date)>=input$dateRange[1])&(as.Date(Strgy$Date)<=input$dateRange[2]),]
    x$TR = x$TR/x$TR[1]
    ggplot(data=x, aes(x=Date, y=TR, group=1)) +
      geom_line(color="red")+
      geom_point()
  })
  
})
