### パッケージの読み込み ###
library(tidyverse)

### ディレクトリ設定 ###
user.dir <- Sys.getenv("USERPROFILE")
dir2017 <- paste0(user.dir, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\2017\\")
filenames <- dir(dir2017)

# 時刻ラベルの作成
hour_lab <- c("00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:30:00",
              "03:00:00", "03:30:00", "04:00:00", "04:30:00", "05:00:00", "05:30:00", 
              "06:00:00", "06:30:00", "07:00:00", "07:30:00", "08:00:00", "08:30:00", 
              "09:00:00", "09:30:00", "10:00:00", "10:30:00", "11:00:00", "11:30:00", 
              "12:00:00", "12:30:00", "13:00:00", "13:30:00", "14:00:00", "14:30:00",
              "15:00:00", "15:30:00", "16:00:00", "16:30:00", "17:00:00", "17:30:00", 
              "18:00:00", "18:30:00", "19:00:00", "19:30:00", "20:00:00", "20:30:00", 
              "21:00:00", "21:30:00", "22:00:00", "22:30:00", "23:00:00", "23:30:00" )

### forループブロック ##################################################
for(i in 1:length(filenames)) {
  # ファイルから日付の取得
  temp_date <- read_csv(paste0(dir2017, filenames[i]), col_names = F,
                        locale = locale(encoding = "SJIS"),
                        skip = 0, n_max = 1)
  
  # 日付ラベルの作成
  date_lab <- paste0(substr(temp_date$X2, 1, 4), "-", 
                     substr(temp_date$X3, 1, 2), "-",
                     substr(temp_date$X4, 1, 2))
  
  # データを取得 
  temp <- read_csv(paste0(dir2017, filenames[i]), 
                   locale = locale(encoding = "SJIS"), col_names = F,
                   skip = 2)
  
  # 列名のための文字列を取得
  cnames <- temp$X2
  
  # 転置行列化, 最小値・最大値・平均値・合計値の項目を除く
  # 要素化を防ぐため、数値データのみとする
  t_temp <- as.data.frame(t(dplyr::select(temp, X4:X51)))
  
  # 列名の追加
  names(t_temp) <- cnames
  # 列名で整列させる
  {
    c1 <- t_temp$`東ｷｬﾝﾊﾟｽ受電電力量　　　　　　　`
    c2 <- t_temp$`西ｷｬﾝﾊﾟｽ受電電力量　　　　　　　`
    c3 <- t_temp$`中地区方面電力量　　　　　　　　`
    c4 <- t_temp$`西地区方面電力量　　　　　　　　`
    c5 <- t_temp$`北地区方面電力量　　　　　　　　`
    c6 <- t_temp$`6号館方面電力量 　　　　　　　　`
    c7 <- t_temp$`16号館方面電力量　　　　　　　　`
    c8 <- t_temp$`2号館電力量 　　　　　　　　　　`
    c9 <- t_temp$`1号館電力量 　　　　　　　　　　`
    c10 <- t_temp$`5号館電力量 　　　　　　　　　　`
    c11 <- t_temp$`3･7･8号館電力量 　　　　　　　　`
    c12 <- t_temp$`19･20号館電力量 　　　　　　　　`
    c13 <- t_temp$`10号館電力量　　　　　　　　　　`
    c14 <- t_temp$`ｷｬﾝﾊﾟｽﾌﾟﾗｻﾞ･15号館電力量　　　　`
    c15 <- t_temp$`33号館電力量　　　　　　　　　　`
    c16 <- t_temp$`17･11号館電力量 　　　　　　　　`
    c17 <- t_temp$`30･31号館電力量 　　　　　　　　`
    c18 <- t_temp$`32号館電力量　　　　　　　　　　`
    c19 <- t_temp$`第一学生ﾎｰﾙ･9号館電力量 　　　　`
    c20 <- t_temp$`図書館電力量　　　　　　　　　　`
    c21 <- t_temp$`図書館新館電力量　　　　　　　　`
    c22 <- t_temp$`21号館電力量　　　　　　　　　　`
    c23 <- t_temp$`22号館(東)電力量　　　　　　　　`
    c24 <- t_temp$`24号館(総合情報ｾﾝﾀｰ)電力量　　　`
    c25 <- t_temp$`23号館(ﾘｻｰﾁｾﾝﾀｰ)電力量　　　　　`
    c26 <- t_temp$`ﾒﾓﾘｱﾙﾎｰﾙ電力量　　　　　　　　　`
    c27 <- t_temp$`体育･文化ｾﾝﾀｰ電力量 　　　　　　`
    c28 <- t_temp$`5号館屋外電力量 　　　　　　　　`
    c29 <- t_temp$`図書館(空調)電力量　　　　　　　`
    c30 <- t_temp$`22号館(西)電力量　　　　　　　　`
    c31 <- t_temp$`25号館方面電力量　　　　　　　　`
    c32 <- t_temp$`ｸﾞﾗｳﾝﾄﾞ方面電力量 　　　　　　　`
    c33 <- t_temp$`70号館方面電力量　　　　　　　　`
    c34 <- t_temp$`50号館方面電力量　　　　　　　　`
    c35 <- t_temp$`50･51号館電力量 　　　　　　　　`
    c36 <- t_temp$`53号館(地上)電力量　　　　　　　`
    c37 <- t_temp$`52号館電力量　　　　　　　　　　`
    c38 <- t_temp$`ﾌﾛﾝﾃｨｱ研究棟電力量　　　　　　　`
    c39 <- t_temp$`高電圧実験室電力量　　　　　　　`
    c40 <- t_temp$`体育館電力量　　　　　　　　　　`
    c41 <- t_temp$`ｸﾞﾗｳﾝﾄﾞ電力量 　　　　　　　　　`
    c42 <- t_temp$`29号館(ｸﾗﾌﾞ･ｻｰｸﾙﾌﾟﾗｻﾞ)電力量　　`
    c43 <- t_temp$`ﾃﾆｽｺｰﾄ電力量　　　　　　　　　　`
    c44 <- t_temp$`53号館(屋上)電力量　　　　　　　`
    c45 <- t_temp$`超伝導実験棟電力量　　　　　　　`
    c46 <- t_temp$`72号館電力量　　　　　　　　　　`
    c47 <- t_temp$`体育館(空調)電力量　　　　　　　`
    c48 <- t_temp$`55号館電力量　　　　　　　　　　`
    c49 <- t_temp$`70･71号館電力量 　　　　　　　　`
    c50 <- t_temp$`武道体育館電力量　　　　　　　　`
    c51 <- t_temp$`25号館電力量　　　　　　　　　　`
    c52 <- t_temp$`14号館方面電力量　　　　　　　　`
    c53 <- t_temp$`その他電力量　　　　　　　　　　`
    c54 <- t_temp$`不言実行館電力量　　　　　　　　`
    c55 <- t_temp$`外気温度　　　　　　　　　　　　`
    c56 <- t_temp$`外気湿度　　　　　　　　　　　　`
  }
  t_temp2 <- cbind.data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,
                              c11,c12,c13,c14,c15,c16,c17,c18,c10,c20,
                              c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,
                              c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,
                              c41,c42,c43,c44,c45,c46,c47,c48,c49,c50,
                              c51,c52,c53,c54,c55,c56)
  # 一日ごとの日付時刻ラベル作成
  date_time <- paste0(date_lab, " ", hour_lab)
  # 結合処理
  if(i == 1){
    n_temp <- t_temp2
    n_lab <- date_time
  } else {
    x_temp <- t_temp2
    n_temp <- rbind(n_temp, x_temp)
    x_lab <- date_time
    n_lab <- cbind(n_lab, x_lab)
  }
}
########################################################################

# データ結合の判定
judge <- 
  if(i == length(filenames)) {
    print("結合成功")
  } else {
    print("結合失敗")
  }

# 文字列の余分な全角スペースの削除
cnames2 <- gsub("　", "", cnames, fixed = T)

# 列名の追加
names(n_temp) <- cnames2
# 型を数値型(numeric)に統一
y <- n_temp %>% mutate_if(is.character, as.numeric)

# ラベルと結合
label <- c(n_lab)
dataset <- cbind(label, y)

dep_2017 <- dataset %>% mutate(
  `Dep1（全学電力量）` = 東ｷｬﾝﾊﾟｽ受電電力量 + 西ｷｬﾝﾊﾟｽ受電電力量,
  `Dep2（研究推進機構（東））` = `23号館(ﾘｻｰﾁｾﾝﾀｰ)電力量` + `16号館方面電力量`,
  `Dep3（事務）` = ﾒﾓﾘｱﾙﾎｰﾙ電力量 + `2号館電力量 ` + `1号館電力量 `,
  `Dep4（学生教育推進機構）` = `第一学生ﾎｰﾙ･9号館電力量 ` + `10号館電力量` + `ｷｬﾝﾊﾟｽﾌﾟﾗｻﾞ･15号館電力量` +`体育･文化ｾﾝﾀｰ電力量 `,
  `Dep5（不言実行館）` = 不言実行館電力量,
  `Dep6（経営情報学部）` = `22号館(東)電力量` + `22号館(西)電力量` + `21号館電力量`,
  `Dep7（国際関係学部）` = `19･20号館電力量 `,
  `Dep8（応用生物学部）` = `17･11号館電力量 ` + `33号館電力量` + `32号館電力量` + `30･31号館電力量 `,
  `Dep9（工学部）` = `6号館方面電力量 ` + `5号館電力量 ` + `5号館屋外電力量 ` + `3･7･8号館電力量 `,
  `Dep10（学生推進機構）` = ﾃﾆｽｺｰﾄ電力量 + `ｸﾞﾗｳﾝﾄﾞ電力量 ` + 武道体育館電力量 + 体育館電力量 + `29号館(ｸﾗﾌﾞ･ｻｰｸﾙﾌﾟﾗｻﾞ)電力量`,
  `Dep11（研究推進機構（西））` = 超伝導実験棟電力量 + 高電圧実験室電力量,
  `Dep12（実験動物教育研究センター）` = `53号館(地上)電力量` + `53号館(屋上)電力量`,
  `Dep13（人文学部）` = `25号館電力量`,
  `Dep14（現代教育学部）` = `70･71号館電力量 ` + `72号館電力量`,
  `Dep15（生命健康科学部）` = `55号館電力量` + `52号館電力量` + `50･51号館電力量 `,
  `Dep16（図書館）` = 図書館電力量 + 図書館新館電力量 + `図書館(空調)電力量`,
  `Dep17（教育支援機構）` = `24号館(総合情報ｾﾝﾀｰ)電力量`,
  date = substr(dataset$label, 1, 10),
  time = substr(dataset$label, 12, 20)
) %>% select(label, date, time, starts_with("Dep"))
# labelの型をPOSIXctに変換
dep_2017$label <- as.POSIXct(dep_2017$label, tz = "Japan")


# データの保存
save_dir <- paste0(user.dir, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\")
write_excel_csv(dep_2017, file.path(save_dir, "Dataset2017.csv"))
write_rds(dep_2017, file.path(save_dir, "Dataset2017.rds"))
