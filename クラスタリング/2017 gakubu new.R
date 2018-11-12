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
#----- 工学部
college.namme <- "工学部"
#Dataset$X12[is.na(Dataset$X12)]<- 10000
#Dataset[12]<-lapply(Dataset[12],abs)
Dataset[Dataset$X12 > 4000, 12] <- Dataset[which(Dataset$X12 > 4000)-168,12] 
#Dataset$X11[is.na(Dataset$X11)]<- 10000
#Dataset[11]<-lapply(Dataset[11],abs)
Dataset[Dataset$X11 > 4000, 11] <- Dataset[which(Dataset$X11 > 4000)-168,11]
#Dataset$X29[is.na(Dataset$X29)]<- 10000
Dataset[Dataset$X29 > 4000, 29] <- Dataset[which(Dataset$X29 > 4000)-168,29]
#Dataset$X7[is.na(Dataset$X7)]<- 10000
Dataset[Dataset$X7 > 4000, 7] <- Dataset[which(Dataset$X7 > 4000)-168,7]
x <- Dataset$X12+ Dataset$X11+Dataset$X29+Dataset$X7
x <- data.frame(x)
#----- 応用生物学部
college.namme <- "応用生物学部"
Dataset[Dataset$X18 > 4000, 18] <- Dataset[which(Dataset$X18 > 4000)-168,18]
Dataset[Dataset$X16 > 4000, 16] <- Dataset[which(Dataset$X16 > 4000)-168,16]
Dataset[Dataset$X19 > 4000, 19] <- Dataset[which(Dataset$X19 > 4000)-168,19]
Dataset[Dataset$X17 > 4000, 17] <- Dataset[which(Dataset$X17 > 4000)-168,17]
x <- Dataset$X18+ Dataset$X16+Dataset$X19+Dataset$X17
x <- data.frame(x)
#----- 国際関係学部
college.namme <- "国際関係学部"
Dataset[Dataset$X13 > 4000, 13] <- Dataset[which(Dataset$X13 > 4000)-168,13]
x <- Dataset$X13
x <- data.frame(x)
#----- 経営情報学部
college.namme <- "経営情報学部"
Dataset[Dataset$X23 > 4000, 23] <- Dataset[which(Dataset$X23 > 4000)-168,23]
Dataset[Dataset$X24 > 4000, 24] <- Dataset[which(Dataset$X24 > 4000)-168,24]
Dataset[Dataset$X31 > 4000, 31] <- Dataset[which(Dataset$X31 > 4000)-168,31]
x <- Dataset$X23+ Dataset$X24+Dataset$X31
x <- data.frame(x)
#----- 学生教育推進機構
college.namme <- " 学生教育推進機構"
#Dataset$X20[is.na(Dataset$X20)]<- 10000
Dataset[Dataset$X20 > 4000, 20] <- Dataset[which(Dataset$X20 > 4000)-168,20]
#Dataset$X14[is.na(Dataset$X14)]<- 10000
Dataset[Dataset$X14 > 4000, 14] <- Dataset[which(Dataset$X14 > 4000)-168,14]
#Dataset$X15[is.na(Dataset$X15)]<- 10000
Dataset[Dataset$X15 > 4000, 15] <- Dataset[which(Dataset$X15 > 4000)-168,15]
#Dataset$X28[is.na(Dataset$X28)]<- 10000
Dataset[Dataset$X28 > 4000, 28] <- Dataset[which(Dataset$X28 > 4000)-168,28]
x <- Dataset$X20+ Dataset$X14+Dataset$X15+Dataset$X28
x <- data.frame(x)
#-----事務
college.namme <- "事務"
Dataset[Dataset$X10 > 4000, 10] <- Dataset[which(Dataset$X10 > 4000)-168,10]
Dataset[Dataset$X9 > 4000, 9] <- Dataset[which(Dataset$X9 > 4000)-168,9]
Dataset[Dataset$X27 > 4000, 27] <- Dataset[which(Dataset$X27 > 4000)-168,27]
x <- Dataset$X10+ Dataset$X9+Dataset$X27
x <- data.frame(x)
#-----研究推進機構
college.namme <- "-研究推進機構"
#Dataset[8]<-lapply(Dataset[8],abs)
#Dataset$X8[is.na(Dataset$X8)]<- 10000
Dataset[Dataset$X8 > 4000, 8] <- Dataset[which(Dataset$X8 > 4000)-168,8]
#Dataset$X26[is.na(Dataset$X26)]<- 10000
Dataset[Dataset$X26 > 4000, 26] <- Dataset[which(Dataset$X26 > 4000)-168,26]
x <- Dataset$X8+ Dataset$X26
x <- data.frame(x)
#-----教育支援機構
college.namme <- "教育支援機構"
Dataset[Dataset$X25 > 4000, 25] <- Dataset[which(Dataset$X25 > 4000)-168,25]
x <- Dataset$X25
x <- data.frame(x)
#-----図書館
college.namme <- "図書館"
#Dataset$X30[is.na(Dataset$X21)]<- 10000
Dataset[Dataset$X21 > 4000, 21] <- Dataset[which(Dataset$X21 > 4000)-168,21]
#Dataset$X22[is.na(Dataset$X22)]<- 10000
#Dataset[22]<-lapply(Dataset[22],abs)
Dataset[Dataset$X22 > 4000, 22] <- Dataset[which(Dataset$X22 > 4000)-168,22]
#Dataset$X30[is.na(Dataset$X30)]<- 10000
Dataset[Dataset$X30 > 4000, 30] <- Dataset[which(Dataset$X30 > 4000)-168,30]
x <- Dataset$X21+ Dataset$X22+Dataset$X30
x <- data.frame(x)
#-----生命健康科学部
college.namme <- "生命健康科学部"
Dataset[Dataset$X35 > 4000, 35] <- Dataset[which(Dataset$X35 > 4000)-168,35]
Dataset[Dataset$X36 > 4000, 36] <- Dataset[which(Dataset$X36 > 4000)-168,36]
Dataset[Dataset$X38 > 4000, 38] <- Dataset[which(Dataset$X38 > 4000)-168,38]
Dataset[Dataset$X49 > 4000, 49] <- Dataset[which(Dataset$X49 > 4000)-168,49]
x <- Dataset$X35+ Dataset$X36+Dataset$X38+Dataset$X49
x <- data.frame(x)
#-----現代教育学部
college.namme <- "現代教育学部"
#Dataset$X50[is.na(Dataset$X50)]<- 10000
Dataset[Dataset$X50 > 4000, 50] <- Dataset[which(Dataset$X50 > 4000)-168,50]
#Dataset$X47[is.na(Dataset$X47)]<- 10000
Dataset[Dataset$X47 > 4000, 47] <- Dataset[which(Dataset$X47 > 4000)-168,47]
x <- Dataset$X50+ Dataset$X47
x <- data.frame(x)
#-----人文学部
college.namme <- "人文学部"
Dataset[Dataset$X52 > 4000, 52] <- Dataset[which(Dataset$X52 > 4000)-168,52]
Dataset[Dataset$X32 > 4000, 32] <- Dataset[which(Dataset$X32 > 4000)-168,32]
x <- Dataset$X52+Dataset$X32
x <- data.frame(x)
#-----実験動物教育研究センター
college.namme <- "-実験動物教育研究センター"
#Dataset$X37[is.na(Dataset$X37)]<- 10000
Dataset[Dataset$X37 > 4000, 37] <- Dataset[which(Dataset$X37 > 4000)-168,37]
#Dataset$X45[is.na(Dataset$X45)]<- 10000
Dataset[Dataset$X45 > 4000, 45] <- Dataset[which(Dataset$X45 > 4000)-168,45]
x <- Dataset$X37+ Dataset$X45
x <- data.frame(x)
#-----研究推進機構
college.namme <- "-研究推進機構"
#Dataset$X39[is.na(Dataset$X39)]<- 10000
Dataset[Dataset$X39 > 4000, 39] <- Dataset[which(Dataset$X39 > 4000)-168,39]
#Dataset$X40[is.na(Dataset$X40)]<- 10000
Dataset[Dataset$X40 > 4000, 40] <- Dataset[which(Dataset$X40 > 4000)-168,40]
#Dataset$X46[is.na(Dataset$X46)]<- 10000
Dataset[Dataset$X46 > 4000, 46] <- Dataset[which(Dataset$X46 > 4000)-168,46]
x <- Dataset$X39+ Dataset$X40+Dataset$X46
x <- data.frame(x)
#-----学生推進機構
college.namme <- "学生推進機構"
#Dataset$X43[is.na(Dataset$X43)]<- 10000
Dataset[Dataset$X43 > 4000, 43] <- Dataset[which(Dataset$X43 > 4000)-168,43]
#Dataset$X41[is.na(Dataset$X41)]<- 10000
Dataset[Dataset$X41 > 4000, 41] <- Dataset[which(Dataset$X41 > 4000)-168,41]
#Dataset$X51[is.na(Dataset$X51)]<- 10000
Dataset[Dataset$X51 > 4000, 51] <- Dataset[which(Dataset$X51 > 4000)-168,51]
#Dataset$X42[is.na(Dataset$X42)]<- 10000
Dataset[Dataset$X42 > 4000, 42] <- Dataset[which(Dataset$X42 > 4000)-168,42]
#Dataset$X44[is.na(Dataset$X44)]<- 10000
Dataset[Dataset$X44 > 4000, 44] <- Dataset[which(Dataset$X44 > 4000)-168,44]
#Dataset$X48[is.na(Dataset$X48)]<- 10000
Dataset[Dataset$X48 > 4000, 48] <- Dataset[which(Dataset$X48 > 4000)-168,48]
x <- Dataset$X41 + Dataset$X43 + Dataset$X51 + Dataset$X42 + Dataset$X44+Dataset$X48
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
pp + geom_tile(color="gray")+   facet_wrap(~ Month, ncol = 2, dir = "v") + scale_y_reverse()
#ppp<- pp + geom_tile(color="black")+   facet_wrap(~ Month, ncol = 2, dir = "v") +  scale_fill_grey()+ scale_y_reverse()#to produce blacknwhite
#ppp+ geom_point(aes(shape = Clust)) to produce calendar plot with pattern
#transfer data to excel
write.table(kmean.y$centers, "center21.csv", sep=",", col.names = TRUE, row.names = FALSE)
#to plot line graphs $のあとはファイルの名前は英語推奨


library(reshape2)
library(ggplot2)
tr <-t(kmean.y$centers)

me <- melt(tr)
#me   Var1 Var2    value
#1   00:00    1 130.0625
#2   01:00    1 131.7188
ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line()+xlab("time")
ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line(size=1.2)+xlab("time")#to change the thickness of line
ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line(size=1.2)+xlab("time")+theme(axis.text.x = element_text(angle = 90, hjust = 1))#to rotate the x-axis by 90 degree
#different colour ko lines athawa points haru plot garna cha vane yeso garne
mu<-mutate(me,cluster=paste0("cluster",me$Var2))
ggplot(mu,aes(x=Var1,y=value,group=Var2,color=cluster))+geom_line(size=1.2)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggplot(mu,aes(x=Var1,y=value,group=Var2,color=cluster))+geom_line(size=1.2)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+expand_limits(y=0)
#　最後の関数の後ろに+xlab("名前")かylab("名前")を付けるとグラフに軸ラベルが入れれる
#to make sparate graphs using facet_wrap
ggplot(mu,aes(x=Var1,y=value,group=Var2,color=cluster))+geom_line(size=1.2)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ facet_wrap(~Var2)

# Rコマンダーツールでグラフを書くために
tx <- strptime(unlist(Dataset$X1),"%Y/%m/%d %H:%M")
xxx <- cbind(format(tx,"%Y/%m/%d %H:%M"), value = x[,1])
xxx <- as.data.frame(xxx, stringsAsFactors= F)
xxx[,2] <- as.numeric(xxx[,2])
#........................................................
tx <- strptime(unlist(Dataset$X1),"%Y/%m/%d %H:%M")
xxx <- cbind(format(tx,"%Y/%m/%d %H:%M"), Dataset[,2:ncol(Dataset)])
xxx <- as.data.frame(xxx, stringsAsFactors= F)

#..........................................................................
Dataset$X14[is.na(Dataset$X14)]
Dataset$X14[is.na(Dataset$X14)]<- 10000
Dataset[Dataset$X14>4000,14] <- Dataset[which(Dataset$X14 > 4000)-168,14]
