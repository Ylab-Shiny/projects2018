library(dplyr)
library(vars)
library(tidyverse)

#buil_name <- c("date")

#names(Dataset) <- buil_name

#データはimport dataset にて読み込む方がよい
#library(readr)
#data <- read_csv("C:/Users/naruse/Dropbox/Yamaha-lab/NAVI/H29/VAR/chuubu-bems_20170509.csv",
#col_names = FALSE, skip = 5)
#View(data)

#分析する元データはDataset
# 時刻はX2に入っていいる

start.time = "00:01:00"
end.time = "07:00:00"
end.time.2 = "08:00:00"

data <- dplyr::filter(Dataset,format(X2,"%H:%M:%S") >= start.time & format(X2,"%H:%M;%S") < end.time)

data2 <- dplyr::filter(Dataset,format(X2,"%H:%M:%S") >= end.time & format(X2,"%H:%M;%S") < end.time.2)

dt <- difftime(strptime(paste(as.character(Dataset$X1[1]), end.time), "%Y%m%d %H:%M:%S"), 
               strptime(paste(as.character(Dataset$X1[1]), start.time), "%Y%m%d %H:%M:%S"), 
               units = "days")

e_dt <- 1 + as.numeric(dt)

ts.eng <- ts(data$X4+data$X5+data$X6+data$X7,frequency = 1440) #工学
ts.bio <- ts(data$X19+data$X24+data$X25+data$X9+data$X10,frequency = 1440) #応用生物
ts.intl <- ts(data$X26,frequency = 1440) #国際関係
ts.busi <- ts(data$X27+data$X28+data$X30,frequency = 1440) #経営情報
ts.edup <- ts(data$X34+data$X35+data$X36+data$X75,frequency = 1440) #学生教育推進
ts.cle <- ts(data$X31+data$X32+data$X33,frequency=1440) #事務
ts.stup1 <- ts(data$X37+data$X38,frequency = 1440) #研究推進1
ts.edus <- ts(data$X39,frequency = 1440) #教育支援
ts.lib <- ts(data$X41+data$X42+data$X43,frequency = 1440) #図書館
ts.life <- ts(data$X46+data$X47+data$X49,frequency = 1440) #生命健康
ts.com <- ts(data$X51+data$X52,frequency = 1440) #現代教育
ts.hum <- ts(data$X45+data$X83,frequency = 1440) #人文
ts.ani <- ts(data$X53+data$X58,frequency = 1440) #実験動物
ts.stup2 <- ts(data$X59+data$X61,frequency = 1440) #研究推進2
ts.supp <- ts(data$X62+data$X64+data$X65+data$X66+data$X67,frequency = 1440) #学生推進


ts.east <- ts(data$X3,frequency=1440) #東部局
ts.west <- ts(data$X44,frequency =1440) #西部局
ts.all <- ts(data$X3+data$X44,frequency = 1440) #全体

ts.all.2 <- ts(data2$X3 + data2$X44, start = e_dt, frequency = 1440)

ts.all.1 <- cbind(ts.all,ts.eng,ts.bio,ts.intl,ts.busi,ts.edup,ts.cle,ts.stup1,ts.edus,
                 ts.lib,ts.life,ts.com,ts.hum,ts.ani,ts.stup2,ts.supp)


p <- VARselect(ts.all,lag.max = 120,type = "const") #回帰期間設定
op = as.numeric(p$selection[1]) #lag default:op=1
var.1 <- VAR(ts.all.1,op)

a <- ts(predict(var.1, n.ahead = 60)$fcst$ts.all[,1],  start = e_dt, frequency = 1440)

windows(20,10)
plot(ts.all, xlim=c(1.25, e_dt + 0.01), ylim=c(0, 100))
lines(a, col = "red") #予測

lines(ts.all.2, col = "blue",lty=2) #実測
legend("topleft",legend = c("prediction","survey"),lty=c(1,2),col=c("red","blue"))



