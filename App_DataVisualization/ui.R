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
    menuItem("データセット", tabName = "table", icon = icon("table"))
  ),
  
  # 年の選択
  selectInput(inputId = "Years", "表示したい年を選択してください（複数選択不可）",
              c("2018", "2017", "2016", "2015", "2014"), multiple = F),
  
  # 日付の選択
  uiOutput("reactRanges")
) ### sidebarの最終部分

# body #
body <- dashboardBody(
  tabItem(tabName = "table",
          dataTableOutput("DataTable"))
) ### bodyの最終部分

## 組み立て ##
dashboardPage(header, sidebar, body)