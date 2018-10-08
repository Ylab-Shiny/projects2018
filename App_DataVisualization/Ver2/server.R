##############################################################################################################################
#### 中部大電力消費実績描画アプリ -- server.R ################################################################################
##############################################################################################################################
# ライブラリ一覧
{
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(rgdal)
  library(RColorBrewer)
  library(googleVis)
  library(leaflet)
  library(DT)
  library(knitr)
  library(rmarkdown)
  library(readr)
  library(shinyBS)
}
# ブラウザでの立ち上げ
options(shiny.launch.browser = T)

# ユーザープロファイルの取得
user <- Sys.getenv("USERPROFILE")
## 年データのパス一覧 ##
# 2018
path2018 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\Dataset2018.rds")
# 2017
path2017 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\Dataset2017.rds")
# 2016
path2016 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\Dataset2016.rds")
# 2015
path2015 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\Dataset2015.rds")
# 2014
path2014 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\Dataset2014.rds")
# 2012
path2012 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\Dataset2012.rds")


## shinyサーバー ##
shinyServer(function(input, output, session){
  
  # UIの選択によって、読み込むデータを変える
  passData <- reactive({
    if(input$Years == "2018") {
      firstData <- read_rds(path = path2018)
    } else if(input$Years == "2017") {
      firstData <- read_rds(path = path2017)
    } else if(input$Years == "2016") {
      firstData <- read_rds(path = path2016)
    } else if(input$Years == "2015") {
      firstData <- read_rds(path = path2015)
    } else if(input$Years == "2014") {
      firstData <- read_rds(path = path2014)
    } else if(input$Years == "2012") {
      firstData <- read_rds(path = path2012)
    }
    
    return(firstData)
  }) ### passDataの最終部分
  
  # カレンダーによる日付範囲の設定
  output$DateRange <- renderUI({
    dateRangeInput(inputId = "theRange", label = "日付範囲を指定してください",
                   start = substr(passData()$label[1], 1, 10),
                   end = substr(passData()$label[nrow(passData())], 1, 10),
                   format = "yyyy-mm-dd"
                     )
  }) ### DateRangeの最終部分
  
  # カレンダーの範囲によって、変容するデータ
  passData2 <- reactive({
    # 日付ラベルを追加
    firstData <- passData() %>% mutate(date = substr(label, 1, 10))
    secondData <- firstData %>% filter(
      date >= input$theRange[1] & date <= input$theRange[2]
    ) %>% select(-c(date))
    
    # labelの型をPOSIXctに変換
    secondData$label <- as.POSIXct(secondData$label, "%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    return(secondData)
    
  }) ### passData2の最終部分
  
  
  output$selectDeps <- renderUI({
    # 列名labelは必要ないので除外
    Deplist = names(passData()[-1])
    
    selectInput(inputId = "theDeps", label = "部局を指定してください（複数選択可）",
                Deplist, multiple = T, selected = Deplist[1])
  }) ### selectDepsの最終部分
  
  # 選択された部局のみ取り出す
  passData3 <- reactive({
    firstData <- passData2() %>% select(label, input$theDeps)
    
    return(firstData)
  }) ### passData3の最終部分
  
  # ggplot用にデータをgatherで整形しなおす
  passData4 <- reactive({
    firstData <- passData3() %>% tidyr::gather(input$theDeps, key = "Deps", value = "P_con")
    
    return(firstData)
  }) ### passData4の最終部分
  
  # データテーブルのアウトプット
  output$DataTable <- renderDataTable({
    datatable(passData3(),
              options = list(
                lengthMenu = c(10, 100, 1500),
                pageLength = 100,
                width = 1000,
                scrollX = "200px",
                scrollY = "700px",
                scrollCollapse = T
              ))
  }) ### DataTableの最終部分
  
  # アイコン
  # 全学電力量の最大値をアイコンとして出力
  output$Max <- renderInfoBox({
    
    infoBox("大学全体の最大電力[kW]", max(passData2()$`Dep1（全学電力量）`, na.rm = T), color = "red")
    
  }) ### Maxの最終部分
  
  # 全学電力量の最小値のアイコンとして出力
  output$Min <- renderInfoBox({
    
    infoBox("大学全体の最小電力[kW]", min(passData2()$`Dep1（全学電力量）`, na.rm = T), color = "blue")
    
  }) ### Minの最終部分
  
  # 全学電力量の平均電力をアイコンとして出力
  output$Mean <- renderInfoBox({
    
    infoBox("大学全体の平均電力消費[kW]", as.integer(mean(passData2()$`Dep1（全学電力量）`, na.rm = T)), color = "green")
    
  }) ### Meanの最終部分
  
  ## トレンドグラフ ###
  output$trendGragh <- renderPlot({
    
    ggplot(passData4(), aes(x = label, y = P_con, color = Deps)) + 
      geom_line() + ylim(0, 4000) + xlab("時間") + ylab("部局毎の電力消費[kW]") + ggtitle("トレンドグラフ")
    
    }) ### trendGraghの最終部分
  
}) ###  shinyServerの最終部分