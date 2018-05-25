### パッケージの読み込み ###
library(tidyverse)

### ディレクトリ設定 ###
user.dir <- Sys.getenv("USERPROFILE")
docu <- paste0(user.dir, "\\Documents\\")
dirname <- paste0(docu, "2017\\")
filenames <- dir(dirname)

### 結合処理とデータセットの整形 ###

# 時刻ラベルの作成
hour_lab <- c("00:00:00", "00:00:00", "01:00:00", "01:00:00", "02:00:00",
              "02:00:00", "03:00:00", "03:00:00", "04:00:00", "04:00:00",
              "05:00:00", "05:00:00", "06:00:00", "06:00:00", "07:00:00",
              "07:00:00", "08:00:00", "08:00:00", "09:00:00", "09:00:00",
              "10:00:00", "10:00:00", "11:00:00", "11:00:00", "12:00:00",
              "12:00:00", "13:00:00", "13:00:00", "14:00:00", "14:00:00",
              "15:00:00", "15:00:00", "16:00:00", "16:00:00", "17:00:00",
              "17:00:00", "18:00:00", "18:00:00", "19:00:00", "19:00:00",
              "20:00:00", "20:00:00", "21:00:00", "21:00:00", "22:00:00",
              "22:00:00", "23:00:00", "23:00:00" )

### forループブロック ##################################################
for(i in 1:length(filenames)) {
  # ファイルから日付の取得
  temp_date <- read_csv(paste0(dirname, filenames[i]), col_names = F,
                        locale = locale(encoding = "SJIS"),
                        skip = 0, n_max = 1)
  # 日付ラベルの作成
  date_lab <- paste0(substr(temp_date$X2, 1, 4), "-", 
                     substr(temp_date$X3, 1, 2), "-",
                     substr(temp_date$X4, 1, 2))
  
  # データを取得 
  temp <- read_csv(paste0(dirname, filenames[i]), 
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

# 列名の追加
names(n_temp) <- cnames

### 異常値処理 ###
# 型を数値型(numeric)に統一
y <- n_temp %>% mutate_if(is.character, as.numeric)

# ラベルと結合
label <- c(n_lab)
dataset <- cbind(label, y)
ds1 <- dataset[, -(56:57)] %>% group_by(label) %>% summarise_each(funs(sum))
ds2 <- cbind(label, dataset[, 56:57]) %>% group_by(label) %>% summarise_each(funs(mean)) 
ds_sum <- cbind(ds1, ds2[, -1])
# フロンティア研究棟は、すべて欠損しているので列ごと削除
ds_sum <- ds_sum[, -39]
# 検討データの保存
ds_name <- paste0("dataset_2017", ".csv")
write_excel_csv(ds_sum, file.path(docu, ds_name))
