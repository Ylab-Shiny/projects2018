# ライブラリーの読み込み
library(AnomalyDetection)
library(tidyverse)
library(anomalize)
library(zoo)

# ユーザープロファイルの取得
user <- Sys.getenv("USERPROFILE")
# 2017
path2017 <- paste0(user, "\\Dropbox\\Yamaha-lab\\user files\\Sahashi\\smartBEMS_Data\\Datasets\\Dataset2017.rds")

# データセットの読み込み
dataset <- read_rds(path2017)
# labelの型をPOSIXctに変換
dataset$label <- as.POSIXct(dataset$label, "%Y-%m-%d %H:%M:%S", tz = "GMT")

## 部局毎に抽出 ##
Dep1 <- dataset %>% select(label, `Dep1（全学電力量）`) %>% tbl_df()
Dep1$`Dep1（全学電力量）` <- na.approx(Dep1$`Dep1（全学電力量）`) # 行方向の値で補完

Dep2 <- dataset %>% select(label, `Dep2（研究推進機構（東））`) %>% tbl_df()
Dep2$`Dep2（研究推進機構（東））` <- na.approx(Dep2$`Dep2（研究推進機構（東））`)

Dep3 <- dataset %>% select(label, `Dep3（事務）`) %>% tbl_df()
Dep3$`Dep3（事務）` <- na.approx(Dep3$`Dep3（事務）`)

## 異常値検出を適用して、その結果を描画 ##
# 95%の信頼区間, 検出する異常値の最大数17520*0.001 = 17個
outliers_Dep1 <- AnomalyDetectionTs(Dep1, direction = "both", plot = T, alpha = 0.05, max_anoms = 0.001)
outliers_Dep1$plot
date_Dep1 <- outliers_Dep1$anoms

outliers_Dep2 <- AnomalyDetectionTs(Dep2, direction = "both", plot = T, alpha = 0.05, max_anoms = 0.001)
outliers_Dep2$plot
date_Dep2 <- outliers_Dep2$anoms

outliers_Dep3 <- AnomalyDetectionTs(Dep3, direction = "both", plot = T, alpha = 0.05, max_anoms = 0.001)
outliers_Dep3$plot
date_Dep3 <- outliers_Dep3$anoms
