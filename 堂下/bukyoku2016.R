### パッケージの読み込み ###
library(tidyverse)

### ディレクトリ設定 ###
user.dir <- Sys.getenv("USERPROFILE")
docu <- paste0(user.dir, "\\Documents\\projects2018\\")
setwd(docu)

# データの読み込み
dataset <- read_rds("analysis_df_2016.rds")

### 部局データセットの構築 ###
mutate_df <- dataset %>% mutate(
  Dep1 = dataset$`東キャンパス受電電力量　　　　　` + dataset$`西キャンパス受電電力量　　　　　`,
  Dep2 = dataset$`２３号館（リサーチｾﾝﾀｰ）電力量　` + dataset$`１６号館方面　電力量　　　　　　`,
  Dep3 = dataset$`メモリアルホール　電力量　　　　` + dataset$`２号館　電力量　　　　　　　　　` +
    dataset$`１号館　電力量　　　　　　　　　`,
  Dep4 = dataset$`第一学生ホール・９号館　電力量　` + dataset$`１０号館　電力量　　　　　　　　` +
    dataset$`キャンパスプラザ・１５号館電力量` + dataset$`体育・文化センター　電力量　　　`,
  Dep5 = dataset$`不言実行館　電力量　　　　　　　`,
  Dep6 = dataset$`２２号館（東）　電力量　　　　　` + dataset$`２２号館（西）　電力量　　　　　` +
    dataset$`２１号館　電力量　　　　　　　　`,
  Dep7 = dataset$`１９・２０号館　電力量　　　　　`,
  Dep8 = dataset$`１７・１１号館　電力量　　　　　` + dataset$`３３号館　電力量　　　　　　　　` +
    dataset$`３２号館　電力量　　　　　　　　` + dataset$`３０・３１号館　電力量　　　　　`,
  Dep9 = dataset$`６号館方面　電力量　　　　　　　` + dataset$`５号館　電力量　　　　　　　　　` +
    dataset$`５号館屋外　電力量　　　　　　　` + dataset$`３・７・８号館　電力量　　　　　`,
  Dep10 = dataset$`テニスコート　電力量　　　　　　` + dataset$`グラウンド　電力量　　　　　　　` +
    dataset$`武道体育館　電力量　　　　　　　` + dataset$`体育館　電力量　　　　　　　　　` + 
    dataset$`２９号館(ｸﾗﾌﾞ･ｻｰｸﾙﾌﾟﾗｻﾞ)電力量　`,
  Dep11 = dataset$`超伝導実験棟　電力量　　　　　　` + dataset$`高電圧実験室　電力量　　　　　　`,
  Dep12 = dataset$`５３号館（地上）　電力量　　　　` + dataset$`５３号館（屋上）　電力量　　　　`,
  Dep13 = dataset$`２５号館　電力量　　　　　　　　`,
  Dep14 = dataset$`７０・７１号館　電力量　　　　　` + dataset$`７２号館　電力量　　　　　　　　`,
  Dep15 = dataset$`５５号館　電力量　　　　　　　　` + dataset$`５２号館　電力量　　　　　　　　` +
    dataset$`５０・５１号館　電力量　　　　　`,
  Dep16 = dataset$`図書館　電力量　　　　　　　　　` + dataset$`図書館新館　電力量　　　　　　　` +
    dataset$`図書館（空調）　電力量　　　　　`,
  Dep17 = dataset$`２４号館（総合情報ｾﾝﾀｰ）電力量　`,
  Temperture = dataset$`外気温度　　　　　　　　　　　　`,
  Humidity = dataset$`外気湿度　　　　　　　　　　　　`
)

## 1時間ごとに集計
# 加算
sum_df <- mutate_df %>% group_by(label) %>% 
  select(-c(Temperture, Humidity)) %>% summarise_each(funs(sum)) %>% 
  select(label, starts_with("Dep"))
# 平均
mean_df <- mutate_df %>% group_by(label) %>% 
  select(Temperture, Humidity) %>% summarise_each(funs(mean))
# 結合
dep_df <- left_join(mean_df, sum_df, by="label")


# dep_dfの保存
write_rds(dep_df, file.path(docu, "dep_df_2016.rds"))
