library(shiny)

# Shinyのユーザーインターフェース
shinyUI(fluidPage(
  
  
  # アプリのタイトル
  titlePanel("クラスタリングの！下請け"),
  
  # ファイルのアップロードUI
  fileInput("file", "csvファイルをアップロードしてください",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  
  
  # サイドバー 
  sidebarLayout(
    sidebarPanel(
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
    ),
    
    # メインパネル
    mainPanel(
       plotOutput("CalenderPlot"),
       plotOutput("qqq")
    )
  )
))

