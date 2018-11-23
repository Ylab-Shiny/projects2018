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

#max(Dataset$X3)   #欠損値があるかどうか確認できる
#Dataset[2]<-lapply(Dataset[2],abs)　#マイナスの欠損値を絶対値でプラスに変更する
#Dataset[which(Dataset$X8 > 4000),8] 
#which(Dataset$X8>4000)
#class(Dataset[137,8])
#Dataset[820,8] <- Dataset[820-168,8]

## データの読み込み ##
user <- Sys.getenv("USERPROFILE")
path <- "\\Dropbox\\Yamaha-lab\\0_semi\\2018Temp\\clustering\\one year data\\"
datapath <- paste0(user, path, "2017nenn_corr.csv")
Dataset <- read_csv(datapath, col_names = F, skip = 1)

Dataset[Dataset$X2 > 6000, 2] <- Dataset[which(Dataset$X2 > 6000)-168,2]
Dataset[Dataset$X3 > 6000,3] <- Dataset[which(Dataset$X3 > 6000)-168,3]
x <- Dataset$X2+ Dataset$X3
x <- data.frame(x)

tx <- strptime(unlist(Dataset$X1),"%Y/%m/%d %H:%M")
time.2016 <- format(tx, "%H:%M")
date.2016 <- format(tx, "%Y/%m/%d")
date.day <- levels(factor(date.2016))
hour.2016 <- levels(factor(time.2016))
lab.date <- list(date.day,hour.2016)
y <- matrix(x[1:nrow(x),],ncol=24,byrow=TRUE,dimnames=lab.date)

inital.v <- apply(y,2, quantile, seq(0,1,1/6))

kmean.y <- kmeans(y, inital.v[2:7,])


#カレンダープロットの作成
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
pp <- ggplot(xxx, aes(Week, WeekN, fill = Clust))
ppp   <- pp + geom_tile(color="gray")+   facet_wrap(~ Month, ncol = 2, dir = "v") + scale_y_reverse()

kmean.y$centers
tr <- t(kmean.y$centers)
me <- melt(tr)
#me   Var1 Var2    value
#1   00:00    1 130.0625
#2   01:00    1 131.7188
ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line()+xlab("time")
ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line(size=1.2)+xlab("time")#to change the thickness of line
ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line(size=1.2)+
  xlab("time")+theme(axis.text.x = element_text(angle = 90, hjust = 1))#to rotate the x-axis by 90 degree
#different colour ko lines athawa points haru plot garna cha vane yeso garne
mu<-mutate(me,cluster=paste0("cluster",me$Var2))
qqq <- ggplot(mu,aes(x=Var1,y=value,group=Var2,color=cluster))+geom_line(size=1.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ここに記述しても意味がない！Shinyサーバーの中に入れるように！！！！


## shinyサーバー ##
shinyServer(function(input, output, session){
  
  shinyServer(function(input, output, session) {
    output$RenderPlot <- renderPlot({
      
      print(ppp)
    }) ### RenderPlotの最終部分
    output$qqq <- renderPlot({
      
      
      print(qqq)
    })
  })
  
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
      geom_line() + ylim(input$RangeY[1], input$RangeY[2]) + xlab("時間") + ylab("部局毎の電力消費[kW]") + ggtitle("トレンドグラフ")
    
    }) ### trendGraghの最終部分
  
}) ###  shinyServerの最終部分