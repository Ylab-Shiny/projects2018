### 追加パッケージの読み込み ###
library(vars)
library(tidyverse)
library(gridExtra)
library(data.table)
library(mice)

### ディレクトリ設定 ###
user.dir <- Sys.getenv("USERPROFILE")
docu <- user.dir %>% paste0("\\Documents\\projects2018\\Prototype 1\\")
setwd(docu)

### 解析データの読み込み ###
# 分析する元データはDataset
Dataset <- read_rds('dep_df_2016.rds')

dataset2 <- Dataset %>%  mutate(date = substr(label, 1, 10))
dataset2$date <- as.Date(dataset2$date)

# 番号の割り当て(月～金:1~5, 土:6, 日:0)
Dataset_jud <- dataset2 %>% mutate(week = format(date, '%w'))
# 平日のみピックアップ（1~5のみ)
Dataset_week <- Dataset_jud %>% filter(Dataset_jud$week >= 1 & 
                                         Dataset_jud$week <= 5)
dataset2 <- Dataset %>%  mutate(date = substr(label , 1 ,10) , hour = substr(label , 12 , 19 ))
Dataset_add <- Dataset_week %>% 
  mutate(datehour = paste(Dataset_week$date, Dataset_week$hour)) 
# 10 ~ 15時の予測をする
start.time = "00:00:00"
end.time = "10:00:00"
end.time2= "11:00:00"
# コンソールで日付入力
input <- readline('調べたい日付を入力してください。(ex. 2017-07-10): ')
# 入力内容を日付型に変更、dayは検証日
day <- as.Date(input)
# 検証日の42日前
start.day <- day - 42 
# POSIXct型の時刻を扱う
start <- as.POSIXct(paste(start.day, start.time))
finish <- as.POSIXct(paste(day, end.time))
finish2 <- as.POSIXct(paste(day, end.time2))
## 指定時刻範囲のデータの取り出し
# 検証日の10時以前
ds <- Dataset_add %>% 
  filter(as.POSIXct(Dataset_add$datehour) <= finish2 &
           Dataset_add$datehour >= start)

nonlb <- ds %>% dplyr::select(starts_with("Dep"))
imp <- nonlb %>% mice(method = "pmm")
impdf <- complete(imp)
days <- cbind(ds[,1:3], impdf, ds[, 21:22])

# 検証日の10時以降
rest <- Dataset_add %>% 
  filter(as.POSIXct(Dataset_add$datehour) >= finish)
# 1単位１時間
{
  ts1 <- ts(days$Dep1, frequency = 24) # Dep1
  ts2 <- ts(days$Dep2, frequency = 24) # Dep2
  ts3 <- ts(days$Dep3, frequency = 24) # Dep3
  ts4 <- ts(days$Dep4, frequency = 24) # Dep4
  ts5 <- ts(days$Dep5, frequency = 24) # Dep5
  ts6 <- ts(days$Dep6, frequency = 24) # Dep6
  ts7 <- ts(days$Dep7, frequency = 24) # Dep7
  ts8 <- ts(days$Dep8, frequency = 24) # Dep8
  ts9 <- ts(days$Dep9, frequency = 24) # Dep9
  ts10 <- ts(days$Dep10, frequency = 24) # Dep10
  ts11 <- ts(days$Dep11, frequency = 24) # Dep11
  ts12 <- ts(days$Dep12, frequency = 24) # Dep12
  ts13 <- ts(days$Dep13, frequency = 24) # Dep13
  ts14 <- ts(days$Dep14, frequency = 24) # Dep14
  ts15<- ts(days$Dep15, frequency = 24) # Dep15
  ts16 <- ts(days$Dep16, frequency = 24) # Dep16
  ts17 <- ts(days$Dep17, frequency = 24) # Dep17
  ts.all <- cbind(ts1, ts2, ts3, ts4, ts5, ts6, ts7, ts8, ts9,
                  ts10, ts11, ts12, ts13, ts14, ts15, ts16, ts17)
  }  

lag = 19 # ラグ次数
# VARモデル
var <- VAR(ts.all, lag, type = 'const')

{# 検証日データ
  subset <- days %>% filter(as.Date(days$date) == day)
  # 絶対にdatehourを書き換えてはいけない!
  subset2 <- subset %>%
    mutate(Times = as.POSIXct(subset$datehour, tz = "Asia/Tokyo"))
  # 予測開始時刻までのデータセット
  past <- melt(subset2,
               id="Times",
               measure=c("Dep1", "Dep2", "Dep3", "Dep4", "Dep5", "Dep6",
                         "Dep7", "Dep8", "Dep9", "Dep10", "Dep11", "Dep12",
                         "Dep13","Dep14", "Dep15", "Dep16", "Dep17"))
}

{# 予測データ
  pre1 <- data.frame(Dep1 = (predict(var, n.ahead = 5)$fcst$ts1[,1]))
  pre2 <- data.frame(Dep2 = (predict(var, n.ahead = 5)$fcst$ts2[,1]))
  pre3 <- data.frame(Dep3 = (predict(var, n.ahead = 5)$fcst$ts3[,1]))
  pre4 <- data.frame(Dep4 = (predict(var, n.ahead = 5)$fcst$ts4[,1]))
  pre5 <- data.frame(Dep5 = (predict(var, n.ahead = 5)$fcst$ts5[,1]))
  pre6 <- data.frame(Dep6 = (predict(var, n.ahead = 5)$fcst$ts6[,1]))
  pre7 <- data.frame(Dep7 = (predict(var, n.ahead = 5)$fcst$ts7[,1]))
  pre8 <- data.frame(Dep8 = (predict(var, n.ahead = 5)$fcst$ts8[,1]))
  pre9 <- data.frame(Dep9 = (predict(var, n.ahead = 5)$fcst$ts9[,1]))
  pre10 <- data.frame(Dep10 = (predict(var, n.ahead = 5)$fcst$ts10[,1]))
  pre11 <- data.frame(Dep11 = (predict(var, n.ahead = 5)$fcst$ts11[,1]))
  pre12 <- data.frame(Dep12 = (predict(var, n.ahead = 5)$fcst$ts12[,1]))
  pre13 <- data.frame(Dep13 = (predict(var, n.ahead = 5)$fcst$ts13[,1]))
  pre14 <- data.frame(Dep14 = (predict(var, n.ahead = 5)$fcst$ts14[,1]))
  pre15 <- data.frame(Dep15 = (predict(var, n.ahead = 5)$fcst$ts15[,1]))
  pre16 <- data.frame(Dep16 = (predict(var, n.ahead = 5)$fcst$ts16[,1]))
  pre17 <- data.frame(Dep17 = (predict(var, n.ahead = 5)$fcst$ts17[,1]))
  mesure <- rest %>% filter(as.Date(rest$date) == day)
  mesure2 <- mesure[2:6,]
  
  fset <- cbind(datehour = mesure2$datehour, pre1, pre2, pre3, pre4, pre5, pre6, pre7, pre8, pre9,
                pre10, pre11, pre12, pre13, pre14, pre15, pre16, pre17)
  fset2 <- fset %>% mutate(Times = as.POSIXct(fset$datehour, tz = "Asia/Tokyo"))
  # 部局ごとの予測データセット
  future <- melt(fset2,
                 id="Times",
                 measure=c("Dep1","Dep2", "Dep3", "Dep4", "Dep5", "Dep6",
                           "Dep7", "Dep8","Dep9", "Dep10", "Dep11", "Dep12",
                           "Dep13", "Dep14", "Dep15", "Dep16", "Dep17"))
  
  ### 目標値の導出 ###
  # 予測時刻のVAR予測合計（大学全体）
  St <- pre1
  # コンソールで設定削減量の百分率を入力
  set <- readline('設定削減量の百分率を入力してください。(ex. 5, 10): ')
  set_v <- as.numeric(set) / 100
  # 大学全体の設定削減量
  k5 <- set_v * St$Dep1[5]
  # インパルス応答(全学に対する全学の反応)
  irf <- irf(var, impulse = "ts1",response = "ts1", n.ahead = 5)
  # インパルス応答比率
  Rt <- c(irf[[1]]$ts1[2]/irf[[1]]$ts1[6], irf[[1]]$ts1[3]/irf[[1]]$ts1[6],
          irf[[1]]$ts1[4]/irf[[1]]$ts1[6], irf[[1]]$ts1[5]/irf[[1]]$ts1[6], 1.00)
  # 大学全体の目標削減量
  Kt <- c(k5*Rt[1], k5*Rt[2], k5*Rt[3], k5*Rt[4], k5*Rt[5])
  # 大学全体の目標値推移
  Dt <- data.frame(goal = (St - Kt))
  goal <- cbind(datehour = mesure2$datehour, Dt)
  goal2 <- goal %>% mutate(Times = as.POSIXct(goal$datehour, tz = "Asia/Tokyo"))
  # 削減目標を引いた電力推移データセット
  gs <- melt(goal2, id="Times", measure=c("Dep1"))
}

'---------------------------------------------------------------------------------------------------------------------'
# 実績値
rest_day <- rest %>% filter(date == day)
act <- rest_day[2:6,]

{# 各部局のggplot
  ### Dep2 ###
  f2 <- future %>% filter(future$variable == "Dep2")
  # 部局への割り振り比率  
  r2 <- pre2$Dep2 / pre1$Dep1
  # 部局目標値
  g2 <- gs %>% mutate(d2 = c(gs$value * r2)) %>%
    melt(id="Times", mesure=c("d2")) %>% filter(variable == "d2")
  g2$value <- as.numeric(g2$value)
  a2 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep2")
  )
  
  pic2 <- ggplot(a2, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g2, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f2, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "研究推進機構（東）") 
  ### Dep3 ###
  f3 <- future %>% filter(future$variable == "Dep3")
  # 部局への割り振り比率
  r3 <- pre3$Dep3 / pre1$Dep1
  # 部局目標値
  g3 <- gs %>% mutate(d3 = c(gs$value * r3)) %>%
    melt(id="Times", mesure=c("d3")) %>% filter(variable == "d3")
  g3$value <- as.numeric(g3$value)
  a3 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep3")
  )
  
  pic3 <- ggplot(a3, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g3, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f3, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "事務")
  ### Dep4 ###
  f4 <- future %>% filter(future$variable == "Dep4")
  # 部局への割り振り比率
  r4 <- pre4$Dep4 / pre1$Dep1
  # 部局目標値
  g4 <- gs %>% mutate(d4 = c(gs$value * r4)) %>%
    melt(id="Times", mesure=c("d4")) %>% filter(variable == "d4")
  g4$value <- as.numeric(g4$value)
  a4 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep4")
  )
  
  pic4 <- ggplot(a4, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g4, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f4, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "学生教育推進機構")  
  ### Dep5 ###
  f5 <- future %>% filter(future$variable == "Dep5")
  # 部局への割り振り比率
  r5 <- pre5$Dep5 / pre1$Dep1
  # 部局目標値
  g5 <- gs %>% mutate(d5 = c(gs$value * r5)) %>%
    melt(id="Times", mesure=c("d5")) %>% filter(variable == "d5")
  g5$value <- as.numeric(g5$value)
  a5 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep5")
  )
  
  pic5 <- ggplot(a5, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g5, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f5, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "不言実行館")  
  ### Dep6 ###
  f6 <- future %>% filter(future$variable == "Dep6")
  # 部局への割り振り比率
  r6 <- pre6$Dep6 / pre1$Dep1
  # 部局目標値
  g6 <- gs %>% mutate(d6 = c(gs$value * r6)) %>%
    melt(id="Times", mesure=c("d6")) %>% filter(variable == "d6")
  g6$value <- as.numeric(g6$value)
  a6 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep6")
  )
  
  pic6 <- ggplot(a6, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g6, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f6, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "経営情報学部")  
  ### Dep7 ###
  f7 <- future %>% filter(future$variable == "Dep7")
  # 部局への割り振り比率
  r7 <- pre7$Dep7 / pre1$Dep1
  # 部局目標値
  g7 <- gs %>% mutate(d7 = c(gs$value * r7)) %>%
    melt(id="Times", mesure=c("d7")) %>% filter(variable == "d7")
  g7$value <- as.numeric(g7$value)
  a7 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep7")
  )
  
  pic7 <- ggplot(a7, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g7, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f7, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "国際関係学部")  
  ### Dep8 ###
  f8 <- future %>% filter(future$variable == "Dep8")
  # 部局への割り振り比率
  r8 <- pre8$Dep8 / pre1$Dep1
  # 部局目標値
  g8 <- gs %>% mutate(d8 = c(gs$value * r8)) %>%
    melt(id="Times", mesure=c("d8")) %>% filter(variable == "d8")
  g8$value <- as.numeric(g8$value)
  a8 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep8")
  )
  
  pic8 <- ggplot(a8, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g8, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f8, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "応用生物学部")  
  ### Dep9 ###
  f9 <- future %>% filter(future$variable == "Dep9")
  # 部局への割り振り比率
  r9 <- pre9$Dep9 / pre1$Dep1
  # 部局目標値
  g9 <- gs %>% mutate(d9 = c(gs$value * r9)) %>%
    melt(id="Times", mesure=c("d9")) %>% filter(variable == "d9")
  g9$value <- as.numeric(g9$value)
  a9 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep9")
  )
  
  pic9 <- ggplot(a9, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g9, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f9, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "工学部")  
  ### Dep10 ###
  f10 <- future %>% filter(future$variable == "Dep10")
  # 部局への割り振り比率
  r10 <- pre10$Dep10 / pre1$Dep1
  # 部局目標値
  g10 <- gs %>% mutate(d10 = c(gs$value * r10)) %>%
    melt(id="Times", mesure=c("d10")) %>% filter(variable == "d10")
  g10$value <- as.numeric(g10$value)
  a10 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep10")
  )
  
  pic10 <- ggplot(a10, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g10, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f10, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "学生推進機構")  
  ### Dep11 ###
  f11 <- future %>% filter(future$variable == "Dep11")
  # 部局への割り振り比率
  r11 <- pre11$Dep11 / pre1$Dep1
  # 部局目標値
  g11 <- gs %>% mutate(d11 = c(gs$value * r11)) %>%
    melt(id="Times", mesure=c("d11")) %>% filter(variable == "d11")
  g11$value <- as.numeric(g11$value)
  a11 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep11")
  )
  
  pic11 <- ggplot(a11, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g11, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f11, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "研究推進機構（西）")  
  ### Dep12 ###
  f12 <- future %>% filter(future$variable == "Dep12")
  # 部局への割り振り比率
  r12 <- pre12$Dep12 / pre1$Dep1
  # 部局目標値
  g12 <- gs %>% mutate(d12 = c(gs$value * r12)) %>%
    melt(id="Times", mesure=c("d12")) %>% filter(variable == "d12")
  g12$value <- as.numeric(g12$value)
  a12 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep12")
  )
  
  pic12 <- ggplot(a12, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g12, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f12, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "実験動物研究センター")  
  ### Dep13 ###
  f13 <- future %>% filter(future$variable == "Dep13")
  # 部局への割り振り比率
  r13 <- pre13$Dep13 / pre1$Dep1
  # 部局目標値
  g13 <- gs %>% mutate(d13 = c(gs$value * r13)) %>%
    melt(id="Times", mesure=c("d13")) %>% filter(variable == "d13")
  g13$value <- as.numeric(g13$value)
  a13 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep13")
  )
  
  pic13 <- ggplot(a13, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g13, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f13, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "人文学部")  
  ### Dep14 ###
  f14 <- future %>% filter(future$variable == "Dep14")
  # 部局への割り振り比率
  r14 <- pre14$Dep14 / pre1$Dep1
  # 部局目標値
  g14 <- gs %>% mutate(d14 = c(gs$value * r14)) %>%
    melt(id="Times", mesure=c("d14")) %>% filter(variable == "d14")
  g14$value <- as.numeric(g14$value)
  a14 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep14")
  )
  
  pic14 <- ggplot(a14, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g14, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f14, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "現代教育学部")  
  ### Dep15 ###
  f15 <- future %>% filter(future$variable == "Dep15")
  # 部局への割り振り比率
  r15 <- pre15$Dep15 / pre1$Dep1
  # 部局目標値
  g15 <- gs %>% mutate(d15 = c(gs$value * r15)) %>%
    melt(id="Times", mesure=c("d15")) %>% filter(variable == "d15")
  g15$value <- as.numeric(g15$value)
  a15 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep15")
  )
  
  pic15 <- ggplot(a15, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g15, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f15, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "生命健康科学部")  
  ### Dep16 ###
  f16 <- future %>% filter(future$variable == "Dep16")
  # 部局への割り振り比率
  r16 <- pre16$Dep16 / pre1$Dep1
  # 部局目標値
  g16 <- gs %>% mutate(d16 = c(gs$value * r16)) %>%
    melt(id="Times", mesure=c("d16")) %>% filter(variable == "d16")
  g16$value <- as.numeric(g16$value)
  a16 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep16")
  )
  
  pic16 <- ggplot(a16, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g16, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f16, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "図書館")  
  ### Dep17 ###
  f17 <- future %>% filter(future$variable == "Dep17")
  # 部局への割り振り比率
  r17 <- pre17$Dep17 / pre1$Dep1
  # 部局目標値
  g17 <- gs %>% mutate(d17 = c(gs$value * r17)) %>%
    melt(id="Times", mesure=c("d17")) %>% filter(variable == "d17")
  g17$value <- as.numeric(g17$value)
  a17 <- act %>% mutate(Times = as.POSIXct(datehour, tz="Asia/Tokyo")) %>% melt(
    id="Times", measure=c("Dep17")
  )
  
  pic17 <- ggplot(a17, aes(x=Times, y=value, group = variable)) +
    geom_line(size=1) + 
    geom_line(data=g17, aes(x=Times, y=value), size=1, color="red") + 
    geom_line(data=f17, aes(x=Times, y=value),linetype=2, size=1) +
    xlab("時刻") + ylab("消費電力量[kWh]") + labs(title = "教育支援機構")  
}

# ダッシュボード
mon <- grid.arrange(pic2, pic3, pic4, pic5, pic6, pic7, pic8, pic9, pic10,
                    pic11, pic12, pic13, pic14, pic15, pic16, pic17)

print(mon)

