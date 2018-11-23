library(shiny)

# Shinyのユーザーインターフェース
shinyUI(fluidPage(
  
  # アプリのタイトル
  titlePanel("クラスタリングの！下請け"),
  
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
       plotOutput("RenderPlot"),
       plotOutput("qqq")
    )
  )
))
