############################################################################################################################
######### 試作１ - server.R ################################################################################################
############################################################################################################################

library(shiny)
library(tidyverse)
library(gridExtra)

# ディレクトリ設定
userdir <- Sys.getenv("USERPROFILE")
wdr <- paste0(userdir, "\\Documents\\projects2018\\Prototype 1\\")
setwd(wdr)

# データの読み込み
ds17 <- read_rds("dep_df_2017.rds") %>% mutate(date = substr(label, 6, 10), time = substr(label, 12, 16))
ds16 <- read_rds("dep_df_2016.rds") %>% mutate(date = substr(label, 6, 10), time = substr(label, 12, 16))


shinyServer(function(input, output) {
  output$dataset <- renderTable({
    theData = switch (input$dataset,
      "2017" = ds17,
      "2016" = ds16
    )
    md <- paste0(input$month, "-", input$day)
    theData2 <- theData %>% filter(date == md) %>% 
      select(time, Temperture, Humidity, starts_with("Dep"))
    theData2
  })
  
  output$datatext <- renderText({
    paste0("これは", input$dataset, "年度の", input$month, "月", input$day, "日のデータセットです")
  })
  
  output$hiddentext <- renderText({
    paste0(input$dataset, "年度の大学全体の最大電力量は", max(switch (input$dataset,
      "2017" = ds17$Dep1,
      "2016" = ds16$Dep1
    )), "[kWh]です")
  })
  
  output$plotDisplay <- renderPlot({
    theData = switch (input$dataset,
                      "2017" = ds17,
                      "2016" = ds16
                      )
    md = paste0(input$month, "-", input$day)
    theData3 <- theData %>% filter(date == md) %>% 
      select(-c(date, time, Temperture, Humidity)) %>% 
      gather(`Dep1`, `Dep2`, `Dep3`, `Dep4`, `Dep5`, `Dep6`, `Dep7`, `Dep8`, `Dep9`,
             `Dep10`, `Dep11`, `Dep12`, `Dep13`, `Dep14`, `Dep15`, `Dep16`, `Dep17`,
             key = "部局", value = "cases")
    
    mon1 <- ggplot(theData3, aes(x=label, y=cases, group=`部局`, color = `部局`)) + 
      geom_line() + xlab("時刻") + ylab("消費電力[kW]") + 
      ggtitle("各部局の電力消費トレンドグラフ")
    
    theData4 <- theData %>% filter(date == md) %>% 
      select(c(label, Temperture, Humidity)) %>% 
      gather(`Temperture`, `Humidity`, key = "凡例", value = "cases")
    
    mon2 <- ggplot(theData4, aes(x=label, y=cases, group=`凡例`, color = `凡例`)) + 
      geom_line() + xlab("時刻") + ylab("実績値（気温[℃], 湿度[%])") +
      ggtitle("一日の気温と湿度のトレンドグラフ")
    
    grid.arrange(mon1, mon2)
    
  })
})
