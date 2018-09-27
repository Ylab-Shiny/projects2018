### パッケージの読み込み ###
library(tidyverse)
library(hms)

### ディレクトリ設定 ###
user.dir <- Sys.getenv("USERPROFILE")
dir2018 <- paste0(user.dir, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\chuubu_bems\\")
filenames <- dir(dir2018)

### forループブロック #####################################################################################
for(i in 1:(length(filenames)-1)) {
  # ファイルの列名の読み込み
  cnames <- read_csv(paste0(dir2018, filenames[i]),
                     locale = locale(encoding = "SJIS"), col_names = F, 
                     skip = 1, n_max = 1)
  cnames <- cnames %>% select(-c(X1, X2))
  
  # データを取得
  data <- read_csv(paste0(dir2018, filenames[i]),
                   locale = locale(encoding = "SJIS"), col_names = F, skip = 5)
  # 行末の時刻のNAに"24:00:00"を代入
  na_date <- is.na(tail(data$X2, 1))
  if(na_date == T) {
    data$X2[nrow(data)] <- as.hms("24:00:00")
  }
  
  # 日付時刻ラベルの作成
  date_lab <- paste0(substr(data$X1, 1, 4), "-", 
                     substr(data$X1, 5, 6), "-",
                     substr(data$X1, 7, 8))
  label <- paste0(date_lab, " ", data$X2)
  
  # データのテンプレート
  temp <- data %>% select(-c(X1, X2))
  
  # 日付時刻ラベルと列方向に結合
  temp2 <- cbind(label, temp)
  
  # if分岐
  if(i == 1) {
    n_temp <- temp2
  } else {
    x_temp <- temp2
    n_temp <- rbind(n_temp, x_temp)
  }
  
}
###############################################################################################################

# データ結合の判定
judge <- 
  if(i == (length(filenames))-1) {
    print("結合成功")
  } else {
    print("結合失敗")
  }

# 文字列の余分な全角スペースの削除
cnames2 <- gsub("　", "", cnames, fixed = T)
cnames3 <- c("label", cnames2)
# 列名の追加
names(n_temp) <- cnames3
# 型を数値型(numeric)に統一
dataset <- n_temp %>% mutate_if(is.character, as.numeric)

dep_2018 <- dataset %>% mutate(
  `Dep1（全学電力量）` = 東ｷｬﾝﾊﾟｽ受電電力量 + 西ｷｬﾝﾊﾟｽ受電電力量,
  `Dep2（研究推進機構（東））` = `23号館(ﾘｻｰﾁｾﾝﾀｰ)電力量` + `16号館方面電力量`,
  `Dep3（事務）` = ﾒﾓﾘｱﾙﾎｰﾙ電力量 + `2号館電力量 ` + `1号館電力量 `,
  `Dep4（学生教育推進機構）` = `第一学生ﾎｰﾙ電力量 ` + `10号館電力量` + `ｷｬﾝﾊﾟｽﾌﾟﾗｻﾞ･15号館電力量`  + 体育文化センター使用電力量,
  `Dep5（不言実行館）` = 不言実行館使用電力量,
  `Dep6（経営情報学部）` = `21号館電力量` + `22号館(東)電力量` + `22号館(西)電力量`,
  `Dep7（国際関係学部）` = `19･20号館電力量 `,
  `Dep8（応用生物学部）` = `１１号館使用電力量` + `１７号館使用電力量` + `30･31号館電力量 ` + `32号館電力量` + `33号館電力量`,
  `Dep9（工学部）` = `3･7･8号館電力量 ` +`5号館電力量 ` + `5号館屋外電力量 ` + `6号館方面電力量 `,
  `Dep10（学生推進機構）` = `29号館(ｸﾗﾌﾞ･ｻｰｸﾙﾌﾟﾗｻﾞ)電力量` + 体育館電力量 + `体育館(空調)電力量` + 武道体育館電力量 + `ｸﾞﾗｳﾝﾄﾞ電力量 ` + ﾃﾆｽｺｰﾄ電力量,
  # フロンティア研究棟は1年中欠損のため、除く
  `Dep11（研究推進機構（西））` = 高電圧実験室電力量 + 超伝導実験棟電力量,
  `Dep12（実験動物教育研究センター）` = `53号館(地上)電力量` + `53号館(屋上)電力量`,
  `Dep13（人文学部）` = `25号館電力量`,
  `Dep14（現代教育学部）` = `70･71号館電力量 ` + `72号館電力量`,
  `Dep15（生命健康科学部）` = `50･51号館電力量 ` + `52号館電力量` + `55号館電力量`,
  `Dep16（図書館）` = 図書館電力量 + 図書館新館電力量 + `図書館(空調)電力量`,
  `Dep17（教育支援機構）` = `24号館(総合情報ｾﾝﾀｰ)電力量`
  ) %>% select(label, starts_with("Dep"))

# labelの型をPOSIXctに変換
dep_2018$label <- as.POSIXct(dep_2018$label, "%Y-%m-%d %H:%M:%S", tz = "Japan")
# データの差分を求める
dt <- diff(as.numeric(dep_2018$label), differences = 1)
# 最終行の1行分足りなくなるので60を追加
dt <- c(dt, 60)

# データを電力消費[kW]に変換
dep_2018$`Dep1（全学電力量）` <- dep_2018$`Dep1（全学電力量）` / (dt/3600)
dep_2018$`Dep2（研究推進機構（東））` <- dep_2018$`Dep2（研究推進機構（東））` / (dt/3600)
dep_2018$`Dep3（事務）` <- dep_2018$`Dep3（事務）` / (dt/3600)
dep_2018$`Dep4（学生教育推進機構）` <- dep_2018$`Dep4（学生教育推進機構）` / (dt/3600)
dep_2018$`Dep5（不言実行館）` <- dep_2018$`Dep5（不言実行館）` / (dt/3600)
dep_2018$`Dep6（経営情報学部）` <- dep_2018$`Dep6（経営情報学部）` / (dt/3600)
dep_2018$`Dep7（国際関係学部）` <- dep_2018$`Dep7（国際関係学部）` / (dt/3600)
dep_2018$`Dep8（応用生物学部）` <- dep_2018$`Dep8（応用生物学部）` / (dt/3600)
dep_2018$`Dep9（工学部）` <- dep_2018$`Dep9（工学部）` / (dt/3600)
dep_2018$`Dep10（学生推進機構）` <- dep_2018$`Dep10（学生推進機構）` / (dt/3600)
dep_2018$`Dep11（研究推進機構（西））` <- dep_2018$`Dep11（研究推進機構（西））` / (dt/3600)
dep_2018$`Dep12（実験動物教育研究センター）` <- dep_2018$`Dep12（実験動物教育研究センター）` / (dt/3600)
dep_2018$`Dep13（人文学部）` <- dep_2018$`Dep13（人文学部）` / (dt/3600)
dep_2018$`Dep14（現代教育学部）` <- dep_2018$`Dep14（現代教育学部）` / (dt/3600) 
dep_2018$`Dep15（生命健康科学部）` <- dep_2018$`Dep15（生命健康科学部）` / (dt/3600)
dep_2018$`Dep16（図書館）` <- dep_2018$`Dep16（図書館）` / (dt/3600)
dep_2018$`Dep17（教育支援機構）` <- dep_2018$`Dep17（教育支援機構）` / (dt/3600)



# データの保存
save_dir <- paste0(user.dir, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\")
write_excel_csv(dep_2018, file.path(save_dir, "Dataset2018.csv"))
write_rds(dep_2018, file.path(save_dir, "Dataset2018.rds"))

