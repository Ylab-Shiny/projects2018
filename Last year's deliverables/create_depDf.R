### パッケージの読み込み ###
library(tidyverse)

### ディレクトリ設定 ###
user.dir <- Sys.getenv("USERPROFILE")
docu <- paste0(user.dir, "\\Documents\\")
setwd(docu)

# データの読み込み
dataset <- read_csv("analysis_df_2017.csv")

### 部局データセットの構築 ###
dep_df <- dataset %>% mutate(
  Dep1 = dataset$`東ｷｬﾝﾊﾟｽ受電電力量　　　　　　　` + dataset$`西ｷｬﾝﾊﾟｽ受電電力量　　　　　　　`,
  Dep2 = dataset$`23号館(ﾘｻｰﾁｾﾝﾀｰ)電力量　　　　　` + dataset$`16号館方面電力量　　　　　　　　`,
  Dep3 = dataset$`ﾒﾓﾘｱﾙﾎｰﾙ電力量　　　　　　　　　` + dataset$`2号館電力量 　　　　　　　　　　` +
    dataset$`1号館電力量 　　　　　　　　　　`,
  Dep4 = dataset$`第一学生ﾎｰﾙ･9号館電力量 　　　　` + dataset$`10号館電力量　　　　　　　　　　` +
    dataset$`ｷｬﾝﾊﾟｽﾌﾟﾗｻﾞ･15号館電力量　　　　` + dataset$`体育･文化ｾﾝﾀｰ電力量 　　　　　　`,
  Dep5 = dataset$`不言実行館電力量　　　　　　　　`,
  Dep6 = dataset$`22号館(東)電力量　　　　　　　　` + dataset$`22号館(西)電力量　　　　　　　　` +
    dataset$`21号館電力量　　　　　　　　　　`,
  Dep7 = dataset$`19･20号館電力量 　　　　　　　　`,
  Dep8 = dataset$`17･11号館電力量 　　　　　　　　` + dataset$`33号館電力量　　　　　　　　　　` +
    dataset$`32号館電力量　　　　　　　　　　` + dataset$`30･31号館電力量 　　　　　　　　`,
  Dep9 = dataset$`6号館方面電力量 　　　　　　　　` + dataset$`5号館電力量 　　　　　　　　　　` +
    dataset$`5号館屋外電力量 　　　　　　　　` + dataset$`3･7･8号館電力量 　　　　　　　　`,
  Dep10 = dataset$`ﾃﾆｽｺｰﾄ電力量　　　　　　　　　　` + dataset$`ｸﾞﾗｳﾝﾄﾞ電力量 　　　　　　　　　` +
    dataset$`武道体育館電力量　　　　　　　　` + dataset$`体育館電力量　　　　　　　　　　` +
    dataset$`29号館(ｸﾗﾌﾞ･ｻｰｸﾙﾌﾟﾗｻﾞ)電力量　　`,
  Dep11 = dataset$`超伝導実験棟電力量　　　　　　　` + dataset$`高電圧実験室電力量　　　　　　　`,
  Dep12 = dataset$`53号館(地上)電力量　　　　　　　` + dataset$`53号館(屋上)電力量　　　　　　　`,
  Dep13 = dataset$`25号館電力量　　　　　　　　　　`,
  Dep14 = dataset$`70･71号館電力量 　　　　　　　　` + dataset$`72号館電力量　　　　　　　　　　`,
  Dep15 = dataset$`55号館電力量　　　　　　　　　　` + dataset$`52号館電力量　　　　　　　　　　` +
    dataset$`50･51号館電力量 　　　　　　　　`,
  Dep16 = dataset$`図書館電力量　　　　　　　　　　` + dataset$`図書館新館電力量　　　　　　　　` +
    dataset$`図書館(空調)電力量　　　　　　　`,
  Dep17 = dataset$`24号館(総合情報ｾﾝﾀｰ)電力量　　　`,
  date = substr(label, 1, 10),
  hour = substr(label, 12, 19)
) %>% select(
  label, date, hour, starts_with("Dep")
)

# dep_dfの保存
write_excel_csv(dep_df, file.path(docu, "dep_df_2017.csv"))
