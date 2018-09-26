### パッケージの読み込み ###
library(tidyverse)

### ディレクトリ設定 ###
user.dir <- Sys.getenv("USERPROFILE")
dir2012 <- paste0(user.dir, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\2012\\")
filenames <- dir(dir2012)

# データを取得
data <- readr::read_csv(paste0(dir2012, filenames[1]), col_names = T) %>% mutate(
  label = data$日時,
  date = as.Date(substr(data$日時, 1, 10))
)
# 2012年度のみ取り出す
dataset <- data %>% filter(date < as.Date("2013-04-01"))

# 部局データのみ切り出し
dep_2012 <- dataset %>% mutate(
  `Dep1（全学電力量）` = 工学部の使用電力 + 経営情報学部の使用電力 + 国際関係学部の使用電力 +
    人文学部の使用電力 + 応用生物学部の使用電力 + 生命健康科学部の使用電力 + 
    現代教育学部の節電目標値 + 全学共同施設の使用電力,
  `Dep6（経営情報学部）` = 経営情報学部の使用電力,
  `Dep7（国際関係学部）` = 国際関係学部の使用電力,
  `Dep8（応用生物学部）` = 応用生物学部の使用電力,
  `Dep9（工学部）` = 工学部の使用電力,
  `Dep13（人文学部）` = 人文学部の使用電力,
  `Dep14（現代教育学部）` = 現代教育学部の使用電力,
  `Dep15（生命健康科学部）` = 生命健康科学部の使用電力,
  time = substr(dataset$label, 12, 20)
) %>% select(label, date, time, starts_with("Dep"))

# データの保存
save_dir <- paste0(user.dir, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\")
write_excel_csv(dep_2012, file.path(save_dir, "Dataset2012.csv"))
write_rds(dep_2012, file.path(save_dir, "Dataset2012.rds"))
