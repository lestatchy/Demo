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
library(data.table)
source("Demo.R")

# Gains <- function(input, output, session,start,end){
#   # start = "2009-01-01"
#   # end = "2010-01-01"
#   Dat = readRDS("Dat.rds")
#   OutSample = Dat[as.Date(Dat$Date)>=as.Date(start) & as.Date(Dat$Date)<=as.Date(end),]
#   Strgy = data.frame(Date = OutSample$Date, Position = rep(0,nrow(OutSample)),Gain = rep(0,nrow(OutSample)),TR = rep(1,nrow(OutSample)) )
#   for (i in seq(1,nrow(Strgy)-1)) {
#     x = OutSample$Position[i]
#     lb = OutSample$LB[i]
#     ub = OutSample$UB[i]
#     pos = ifelse(x<lb,1,ifelse(x>ub,-1,Strgy$Position[i]))
#     gain = -pos*OutSample$Libor[i]/10000+pos*(OutSample$Swap[i+1]/OutSample$Swap[i]-1)
#     totalRet = Strgy$TR[i]*(1+gain)
#     Strgy$Position[i+1] = pos
#     Strgy$Gain[i+1] = gain
#     Strgy$TR[i+1] = totalRet
#   }
#   reactive(Strgy)
# }



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  dat <- reactive({
    # Strgy[as.Date(Strgy$Date)>=input$dateRange[1] & as.Date(Strgy$Date)<=input$dateRange[2],]
    Gains(input$dateRange[1],input$dateRange[2])
  })
  # dat <- callModule(Gains,"id",reactive(min(input$dateRange)),reactive(max(input$dateRange)))
  dataTable <- reactive(
    data.frame(Days = difftime(input$dateRange[2],input$dateRange[1]),
               Mean = mean(Gains(input$dateRange[1],input$dateRange[2])$Gain),
               Median = median(Gains(input$dateRange[1],input$dateRange[2])$Gain),
               Skewness = skewness(Gains(input$dateRange[1],input$dateRange[2])$Gain), 
               Kurtosis = kurtosis(Gains(input$dateRange[1],input$dateRange[2])$Gain)
  ))
  

  output$hist <- renderPlot({
    # generate bins based on input$bins from ui.R
    # draw the histogram with the specified number of bins
    ggplot(dat(), aes(x=Gain)) +
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
  
  output$hist2 <- renderPlot({
    BootstrapData = readRDS("BootstrapData.rds")
    t1 = input$nweeks[1]
    t2 = input$nweeks[2]
    colnames(BootstrapData)[3] = "weeks"
    BootstrapData = BootstrapData[BootstrapData$weeks>=t1 & BootstrapData$weeks<=t2, ]
    ggplot(BootstrapData, aes(x=gain)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white",bins=20) +
      xlab("Distribution of Weekly Gain") +
      geom_vline(aes(xintercept=mean(gain, na.rm=T)),   # Ignore NA values for mean
                 color="red", linetype="dashed", size=1)+
      geom_density(alpha=.2, fill="#FF6666")
  })
  
  output$trend <- renderPlot({
    Dat1 = readRDS("trendData.rds")
    Dat1 = Dat1[Dat1$weeks>=input$nweeks[1] & Dat1$weeks<=input$nweeks[2],]
    ggplot(data=Dat1, aes(x=weeks, y=mean, ymin=low, ymax=high,fill = 'blue'))+ 
      geom_line() + geom_ribbon(alpha=.5) + theme(legend.position="none")
  })
  
})
