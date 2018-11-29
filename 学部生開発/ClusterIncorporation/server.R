library(tidyverse) # この中にggplot2,readr,dplyrが含まれている
library(tcltk)
library(devtools)
library(ggTimeSeries)
library(readr)
library(shiny)
library(DT)
library(knitr)
library(rmarkdown)
library(shinyBS)
library(plyr)
library(data.table) # 追加　2018/11/27
library(stats) # 追加　2018/11/29

######## パッケージが重複することの無いように！！！！！ #################################


# ここに記述しても意味がない！Shinyサーバーの中に入れるように！！！！

# Shinyサーバー
shinyServer(function(input, output, session) {
  
  output$qqq <- renderPlot({
    
    
    print(qqq)
  })
  
  
  ## 初期データ #################################
  initData <- reactive({
    if (!is.null(input$file)) {
      firstData <- read_csv(input$file$datapath, col_names = F, skip = 1)
    } else {
      firstData <- NULL
    }
    
    return(firstData)
  }) ### initDataの最終部分
  
  ## カレンダープロット用のデータ ##
  Data_calender <- reactive({
    if (!is.null(input$file)) {
      x <- initData()$X2+ initData()$X3
      x <- data.frame(x)
      
      tx <- strptime(unlist(initData()$X1),"%Y/%m/%d %H:%M")
      time.2016 <- format(tx, "%H:%M")
      date.2016 <- format(tx, "%Y/%m/%d")
      date.day <- levels(factor(date.2016))
      hour.2016 <- levels(factor(time.2016))
      lab.date <- list(date.day,hour.2016)
      y <- matrix(x[1:nrow(x),],ncol=24,byrow=TRUE,dimnames=lab.date)
      
      inital.v <- apply(y,2, quantile, seq(0,1,1/6))
      
      kmean.y <- kmeans(y, inital.v[2:7,])
      cal.mat <- as.data.frame(cbind("Date" = date.day, "Cluster" = kmean.y$cluster))
      
      cal.mat[,1] <- as.Date(cal.mat[,1])
      
      xxx <- data.frame(WeekN = format(cal.mat$Date, "%m%U"), 
                        Week = as.POSIXlt(cal.mat$Date)$wday, 
                        Clust = cal.mat$Cluster, 
                        Month = format(cal.mat$Date, "%y年%m月"), stringsAsFactors = TRUE)
      xxx$WeekN <- factor(xxx$WeekN, ordered = TRUE)
      xxx$Week <- factor(xxx$Week,  labels = c("日", "月", "火", "水", "木", "金", "土"))#,ordered=TRUE)
      xxx$week <- as.numeric(format(cal.mat$Date, "%m%U"))
      xxx <- ddply(xxx, .(Month), transform, WeekN = 1 + week - min(week))
    } else {
      xxx <- NULL
    }
    
    return(xxx)
  }) ### Data_calenderの最終部分
  
  # オリジナルデータの集計の出力
  output$Data_calender <- renderDataTable({
    datatable(Data_calender())
  }) ### summary_conの最終部分
  
  ## カレンダープロット ##
  Calender <- reactive({
    if (!is.null(input$file)) {
      
      #カレンダープロットの作成
      pp <- ggplot(Data_calender(), aes(Week, WeekN, fill = Clust))
      ppp <- pp + geom_tile(color="gray")+   facet_wrap(~ Month, ncol = 2, dir = "v") + scale_y_reverse()
      
      print(ppp)
    } else {
      print(NULL)
    }
    
  }) ### Calenderの最終部分
  
  output$CalenderPlot <- renderPlot({
    Calender()
  }) ### CalenderPlotの最終部分
  
  
  
})
