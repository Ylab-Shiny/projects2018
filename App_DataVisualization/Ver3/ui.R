##############################################################################################################################
#### 中部大電力消費実績描画アプリ -- ui.R ####################################################################################
##############################################################################################################################
# ライブラリ一覧
{
  library(shiny)
  library(shinydashboard)
  library(shinyBS)
  library(leaflet)
  library(shinythemes)
}

### 構成要素 ###
# header #
header <- dashboardHeader(title = "中部大学の電力消費実績描画アプリ", titleWidth = 500) ### headerの最終部分

# sidebar #
sidebar <- dashboardSidebar(
  # サイドバーメニュー
  sidebarMenu(
    menuItem("データセット[kW]", tabName = "table", icon = icon("table")),
    menuItem("トレンドグラフ", tabName = "trend", icon = icon("dashboard"),
             badgeLabel = "ロード遅め", badgeColor = "red"),
    menuItem("カレンダープロット",tabuname = "calendar", icon("calendar-alt")),
    menuItem("クラスタリング", tabName = "cluste", icon = icon("apple"))
  ),
  
  # 年の選択
  selectInput(inputId = "Years", "表示したい年度を選択してください（複数選択不可）",
              c("2018", "2017", "2016", "2015", "2014", "2012"), multiple = F),
  # 1行改行
  br(),
  
  # カレンダーの出力
  uiOutput("DateRange"),
  
  # 1行改行
  br(),
  
  # 部局選択の出力
  uiOutput("selectDeps"),
  
  # 1行改行
  br(),
  
  sliderInput(inputId = "RangeY", label = "Y軸（電力消費[kW]）の範囲をを指定してください",
              min = 0, max = 4000, value = c(0, 4000), step = 50)
  
) ### sidebarの最終部分

# body #
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "table",
            dataTableOutput("DataTable")),
    
    tabItem(tabName = "trend",
            shiny::fluidRow(
              infoBoxOutput(width = 3, "Max"),
              infoBoxOutput(width = 3, "Min"),
              infoBoxOutput(width = 3, "Mean"),
              
              # トレンドグラフの描画
              plotOutput("trendGragh")
            )),
    tabItem(tabName = "cluste",plotOutput("qqq"))
    
  )
) ### bodyの最終部分

## 組み立て ##
dashboardPage(header, sidebar, body)