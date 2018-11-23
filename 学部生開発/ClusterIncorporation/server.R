library(tcltk)
library(devtools)
library(ggTimeSeries)
library(ggplot2)
library(readr)
library(shiny)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(googleVis)
library(leaflet)
library(DT)
library(knitr)
library(rmarkdown)
library(shinyBS)

#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(tcltk)
library(devtools)
library(ggTimeSeries)
library(ggplot2)
library(readr)
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
library(plyr)
library(ggplot2)

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

output$RenderPlot
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
  w <- ppp[,2] bins <- seq(min(x),max(x),length.out = input$bins+1)
  hist(x,breaks = bins,col = 'darkgray',border = 'white')
  
  
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
