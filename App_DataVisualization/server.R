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
path2018 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\d")
fnames2018 <- dir(path2018)
for(i in 1:length(fnames2018)-1) { # -1はファイル転送のシェルスクリプトがあるため
  x <- as.Date(substr(fnames2018[i], 2, 7), "%y%m%d")
  if(i == 1) {
    dl2018 <- x
  } else {
    dl2018 <- c(dl2018, x)
  }
}
df2018 <- cbind.data.frame(dl2018, fnames2018[1:length(fnames2018)-1])

# 2017
path2017 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\2017\\")
fnames2017 <- dir(path2017)
for(i in 1:length(fnames2017)) {
  x <- as.Date(substr(fnames2017[i], 2, 7), "%y%m%d")
  if(i == 1) {
    dl2017 <- x
  } else {
    dl2017 <- c(dl2017, x)
  }
}
df2017 <- cbind.data.frame(dl2017, fnames2017)

# 2016
path2016 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\2016\\")
fnames2016 <- dir(path2016)
for(i in 1:length(fnames2016)) {
  x <- as.Date(substr(fnames2016[i], 2, 7), "%y%m%d")
  if(i == 1) {
    dl2016 <- x
  } else {
    dl2016 <- c(dl2016, x)
  }
}
df2016 <- cbind.data.frame(dl2016, fnames2016)

# 2015
path2015 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\2015\\")
fnames2015 <- dir(path2015)
for(i in 1:length(fnames2015)) {
  x <- as.Date(substr(fnames2015[i], 17, 22), "%y%m%d")
  if(i == 1) {
    dl2015 <- x
  } else {
    dl2015 <- c(dl2015, x)
  }
}
df2015 <- cbind.data.frame(dl2015, fnames2015)

# 2014
path2014 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\2014\\")
fnames2014 <- dir(path2014)
for(i in 1:length(fnames2014)) {
  x <- as.Date(substr(fnames2014[i], 17, 22), "%y%m%d")
  if(i == 1) {
    dl2014 <- x
  } else {
    dl2014 <- c(dl2014, x)
  }
}
df2014 <- cbind.data.frame(dl2014, fnames2014)

## shinyサーバー ##
shinyServer(function(input, output, session){
  # 年別の日付範囲
  output$reactRanges <- renderUI({
    switch (input$Years,
      "2018" = selectInput(inputId = "date2018", "日付を選択してください（複数選択不可）",
                         dl2018, multiple = F),
      "2017" = selectInput(inputId = "date2017", "日付を選択してください（複数選択不可）",
                         dl2017, multiple = F),
      "2016" = selectInput(inputId = "date2016", "日付を選択してください（複数選択不可）",
                         dl2016, multiple = F),
      "2015" = selectInput(inputId = "date2015", "日付を選択してください（複数選択不可）",
                         dl2015, multiple = F),
      "2014" = selectInput(inputId = "date2014", "日付を選択してください（複数選択不可）",
                         dl2014, multiple = F)
    )
  }) ### reactRangesの最終部分
  
  # UIの選択によって、読み込むデータを変える
  passData <- reactive({
    if(input$Years == "2018") {
      filename <- df2018 %>% filter(dl2018 == input$date2018)
      firstData <- read_csv(file = paste0(path2018, "\\", as.character(filename$fnames2018[1])),
                            locale = locale(encoding = "SJIS"), skip = 1)
    } else if(input$Years == "2017") {
      filename <- df2017 %>% filter(dl2017 == input$date2017)
      firstData <- read_csv(file = paste0(path2017, "\\", as.character(filename$fnames2017[1])),
                            locale = locale(encoding = "SJIS"), skip = 1)
    } else if(input$Years == "2016") {
      filename <- df2016 %>% filter(dl2016 == input$date2016)
      firstData <- read_csv(file = paste0(path2016, "\\", as.character(filename$fnames2016[1])),
                            locale = locale(encoding = "SJIS"), skip = 1)
    } else if(input$Years == "2015") {
      filename <- df2015 %>% filter(dl2015 == input$date2015)
      firstData <- read_csv(file = paste0(path2015, "\\", as.character(filename$fnames2015[1])),
                            locale = locale(encoding = "SJIS"), skip = 1)
    } else if(input$Years == "2014") {
      filename <- df2014 %>% filter(dl2014 == input$date2014)
      firstData <- read_csv(file = paste0(path2014, "\\", as.character(filename$fnames2014[1])),
                            locale = locale(encoding = "SJIS"), skip = 1)
    }
    
    return(firstData)
  }) ### passDataの最終部分
  
  # データテーブルのアウトプット
  output$DataTable <- renderDataTable({
    datatable(passData(),
              options = list(
                lengthMenu = c(10, 100, 1500),
                pageLength = 100,
                width = 1000,
                scrollX = "200px",
                scrollY = "700px",
                scrollCollapse = T
              ))
  }) ### DataTableの最終部分
  
}) ###  shinyServerの最終部分