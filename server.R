#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



library(shiny)
library(knitr)
library(ggplot2)
library(plotly)
library(moments)
library(data.table)
library(gridExtra)
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
               "Annualized Mean(%)" = mean(Gains(input$dateRange[1],input$dateRange[2])$Gain),
               "Annualized Median(%)" = median(Gains(input$dateRange[1],input$dateRange[2])$Gain),
               "Annualized StdDev(%)" = sd(Gains(input$dateRange[1],input$dateRange[2])$Gain)/sqrt(52),
               "Annualized Sharpe" = mean(Gains(input$dateRange[1],input$dateRange[2])$Gain)*sqrt(52)/sd(Gains(input$dateRange[1],input$dateRange[2])$Gain),
               Skewness = skewness(Gains(input$dateRange[1],input$dateRange[2])$Gain), 
               Kurtosis = kurtosis(Gains(input$dateRange[1],input$dateRange[2])$Gain),check.names = F
  ))
  
  # data <- read_xls("UST10y_update2.xlsx",sheet = "Cash")

  output$hist <- renderPlot({
    # generate bins based on input$bins from ui.R
    # draw the histogram with the specified number of bins
    ggplot(dat(), aes(x=Gain)) +
      geom_histogram( colour="black", fill="white",bins=input$bins) + 
      xlab("Distribution of strategy weekly annualized returns(%)")+
      # scale_x_continuous(breaks=seq(round(min(Strgy$Gain),2),
      #                               round(max(Strgy$Gain),2),
      #                               by=round((max(Strgy$Gain)-min(Strgy$Gain))/5,2)
      # )) +
      geom_vline(aes(xintercept=mean(Gain, na.rm=T)),   # Ignore NA values for mean
                 color="red", linetype="dashed", size=1)
    
    
  })
  output$stats <- renderPrint({
    print(dataTable(),row.names = FALSE,digits = 2)
    })
  
  output$totalReturn <- renderPlotly({
    x = Strgy[(as.Date(Strgy$Date)>=input$dateRange[1])&(as.Date(Strgy$Date)<=input$dateRange[2]),]
    x$TR = x$TR/x$TR[1]
    gg = ggplot(data=x, aes(x=Date, y=TR)) +
      geom_line(color="red")+
      geom_point()
    ggplotly(gg)
  })
  
  output$hist2 <- renderPlot({
    BootstrapData = readRDS("BootstrapData.rds")
    t1 = input$nweeks[1]
    t2 = input$nweeks[2]
    colnames(BootstrapData)[3] = "weeks"
    BootstrapData = BootstrapData[BootstrapData$weeks>=t1 & BootstrapData$weeks<=t2, ]
    ggplot(BootstrapData, aes(x=gain)) +
      geom_histogram(colour="black", fill="white",bins=40) +
      xlab("Distribution of strategy weekly annualized returns(%)") +
      geom_vline(aes(xintercept=mean(gain, na.rm=T)),   # Ignore NA values for mean
                 color="red", linetype="dashed", size=1)
  })
  
  output$trend <- renderPlotly({
    Dat1 = readRDS("trendData.rds")
    Dat1 = Dat1[Dat1$weeks>=input$nweeks[1] & Dat1$weeks<=input$nweeks[2],]
    p1 = ggplot(data=Dat1, aes(x=weeks, y=mean, ymin=low, ymax=high))+ 
      geom_line() + geom_ribbon(alpha=.5) + theme(legend.position="none") +
      xlab("Different investment horizons (weeks)") +
      ylab("expected annulized weekly returns(%)")
    p2 = ggplot(data = Dat1,aes(x=weeks,y=sharpe)) +
      geom_line() +
      xlab("Different investment horizons (weeks)") +
      ylab("Average Sharpe Ratio")
    p1 = ggplotly(p1)
    p2 = ggplotly(p2)
    subplot(p1, p2)
  })
  
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('Description.rmd')))
  })

  # output$table <- renderDataTable({
  #   dataTable
  # })
  
  output$flowchart <- renderImage({
    
      return(list(
        src = "Trading Strategy.png",
        filetype = "image/png",
        alt = "This is a chainring"
      ))
    
    
  }, deleteFile = FALSE)
  
  output$downloadExcel <-downloadHandler(
    filename <- function(){
      paste("Strategy Replication","xlsx",sep=".")
    },
    content <- function(file){
      file.copy("UST10y_update2.xlsx", file)
    },
    contentType = "application/xlsx"
  )
  
  output$totalReturn2 <- renderPlotly({

    
    begin = as.Date(input$Lookback)
    end = as.Date(input$refDate)
    testData = Strgy[as.Date(Strgy$Date)>begin & as.Date(Strgy$Date)<end,]
    maxs = data.frame(Date = testData$Date[1],Max = 0)
    mins = data.frame(Date = testData$Date[1],Min = 0)
    testData$TR = testData$TR/testData$TR[1]
    L = nrow(testData)
    max = testData$TR[1]
    min = max
    for(i in 1:(L-1)){
      temp = testData$TR[i]
      forward = testData$TR[i+1]
      if(temp > forward & temp > max){
        max = temp
        newrowmax = data.frame(Date = testData$Date[i],Max = max)
        maxs = rbind(maxs,newrowmax)
        min = max
      }else if(temp < forward & temp < min){
        min = temp
        newrowmin = data.frame(Date = testData$Date[i],Min = min)
        mins = rbind(mins,newrowmin)
      }
    }
    maxs = maxs[-1,]
    mins = mins[-1,]
    drawndowns = data.frame(Date = testData$Date[1],TR = 0, DD = 0)
    LL = nrow(maxs)
    for(i in 2:LL){
      t1 = maxs$Date[i-1]
      t2 = maxs$Date[i]
      x = testData[testData$Date>t1 & testData$Date<t2,]
      min = min(x$TR)
      temp = x[x$TR == min,]
      newrow = data.frame(Date = temp$Date,TR = temp$TR,DD = (min-maxs$Max[i])/maxs$Max[i])
      drawndowns = rbind(drawndowns,newrow)
    }
    drawndowns = drawndowns[-1,]
    
    
    p = ggplot(data=testData, aes(x=Date, y=TR)) +
      geom_line(color="black")+
      geom_point(data=maxs, aes(y=Max), colour="green", size=2)+
      geom_point(data=drawndowns, colour="red", size=2)
    ggplotly(p)
  })
  output$hist3 <- renderPlot({
    begin = as.Date(input$Lookback)
    end = as.Date(input$refDate)
    testData = Strgy[as.Date(Strgy$Date)>begin & as.Date(Strgy$Date)<end,]
    ggplot(data = testData, aes(x=Gain))+
      geom_histogram(colour="black", fill="white", binwidth = 0.002)+
      geom_density(colours="orange",fill="orange",alpha=0.2)+
      geom_vline(aes(xintercept=mean(Gain, na.rm=T)),   # Ignore NA values for mean
                 color="red", linetype="dashed", size=1)
  })
  output$hist4 <- renderPlotly({
    begin = as.Date(input$Lookback)
    end = as.Date(input$refDate)
    testData = Strgy[as.Date(Strgy$Date)>begin & as.Date(Strgy$Date)<end,]
    maxs = data.frame(Date = testData$Date[1],Max = 0)
    testData$TR = testData$TR/testData$TR[1]
    L = nrow(testData)
    max = testData$TR[1]
    for(i in 1:(L-1)){
      temp = testData$TR[i]
      forward = testData$TR[i+1]
      if(temp > forward & temp > max){
        max = temp
        newrowmax = data.frame(Date = testData$Date[i],Max = max)
        maxs = rbind(maxs,newrowmax)
      }
    }
    maxs = maxs[-1,]
    drawndowns = data.frame(Date = testData$Date[1],TR = 0, DD = 0)
    LL = nrow(maxs)
    for(i in 2:LL){
      t1 = maxs$Date[i-1]
      t2 = maxs$Date[i]
      x = testData[testData$Date>t1 & testData$Date<t2,]
      min = min(x$TR)
      temp = x[x$TR == min,]
      newrow = data.frame(Date = temp$Date,TR = temp$TR,DD = (min-maxs$Max[i])/maxs$Max[i])
      drawndowns = rbind(drawndowns,newrow)
    }
    drawndowns = drawndowns[-1,]
    ggplot(data = drawndowns, aes(x=DD))+
      geom_histogram(colour="black", fill="white", bins = 10)
  })
  
})
