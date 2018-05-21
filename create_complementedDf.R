### パッケージの読み込み ###
library(tidyverse)
library(ruler)
library(mice)

### ディレクトリ設定 ###
user.dir <- Sys.getenv("USERPROFILE")
dropbox <- paste0(user.dir, 
                  "\\Dropbox\\Yamaha-lab\\0_semi\\2018Temp\\卒研ミニプロジェクト\\Shinyチーム\\")
setwd(dropbox)

# データの読み込み
ds <- read_csv("dataset_2017.csv", col_names = F, skip = 1)
ds2 <- read_csv("dataset_2017.csv")
cnames <- read_csv("dataset_2017.csv", col_names = F, n_max = 1)
### 列ごとの平均と標準偏差を求める ###
ms <- ds %>% summarise_each(funs(mean(., na.rm = T), sd(., na.rm = T)))


# zスコア
data_z <- ds %>% mutate(
  z2 = (X2 - ms$X2_mean) / ms$X2_sd,
  z3 = (X3 - ms$X3_mean) / ms$X3_sd,
  z4 = (X4 - ms$X4_mean) / ms$X4_sd,
  z5 = (X5 - ms$X5_mean) / ms$X5_sd,
  z6 = (X6 - ms$X6_mean) / ms$X6_sd,
  z7 = (X7 - ms$X7_mean) / ms$X7_sd,
  z8 = (X8 - ms$X8_mean) / ms$X8_sd,
  z9 = (X9 - ms$X9_mean) / ms$X9_sd,
  z10 = (X10 - ms$X10_mean) / ms$X10_sd,
  z11 = (X11 - ms$X11_mean) / ms$X11_sd,
  z12 = (X12 - ms$X12_mean) / ms$X12_sd,
  z13 = (X13 - ms$X13_mean) / ms$X13_sd,
  z14 = (X14 - ms$X14_mean) / ms$X14_sd,
  z15 = (X15 - ms$X15_mean) / ms$X15_sd,
  z16 = (X16 - ms$X16_mean) / ms$X16_sd,
  z17 = (X17 - ms$X17_mean) / ms$X17_sd,
  z18 = (X18 - ms$X18_mean) / ms$X18_sd,
  z19 = (X19 - ms$X19_mean) / ms$X19_sd,
  z20 = (X20 - ms$X20_mean) / ms$X20_sd,
  z21 = (X21 - ms$X21_mean) / ms$X21_sd,
  z22 = (X22 - ms$X22_mean) / ms$X22_sd,
  z23 = (X23 - ms$X23_mean) / ms$X23_sd,
  z24 = (X24 - ms$X24_mean) / ms$X24_sd,
  z25 = (X25 - ms$X25_mean) / ms$X25_sd,
  z26 = (X26 - ms$X26_mean) / ms$X26_sd,
  z27 = (X27 - ms$X27_mean) / ms$X27_sd,
  z28 = (X28 - ms$X28_mean) / ms$X28_sd,
  z29 = (X29 - ms$X29_mean) / ms$X29_sd,
  z30 = (X30 - ms$X30_mean) / ms$X30_sd,
  z31 = (X31 - ms$X31_mean) / ms$X31_sd,
  z32 = (X32 - ms$X32_mean) / ms$X32_sd,
  z33 = (X33 - ms$X33_mean) / ms$X33_sd,
  z34 = (X34 - ms$X34_mean) / ms$X34_sd,
  z35 = (X35 - ms$X35_mean) / ms$X35_sd,
  z36 = (X36 - ms$X36_mean) / ms$X36_sd,
  z37 = (X37 - ms$X37_mean) / ms$X37_sd,
  z38 = (X38 - ms$X38_mean) / ms$X38_sd,
  z39 = (X39 - ms$X39_mean) / ms$X39_sd,
  z40 = (X40 - ms$X40_mean) / ms$X40_sd,
  z41 = (X41 - ms$X41_mean) / ms$X41_sd,
  z42 = (X42 - ms$X42_mean) / ms$X42_sd,
  z43 = (X43 - ms$X43_mean) / ms$X43_sd,
  z44 = (X44 - ms$X44_mean) / ms$X44_sd,
  z45 = (X45 - ms$X45_mean) / ms$X45_sd,
  z46 = (X46 - ms$X46_mean) / ms$X46_sd,
  z47 = (X47 - ms$X47_mean) / ms$X47_sd,
  z48 = (X48 - ms$X48_mean) / ms$X48_sd,
  z49 = (X49 - ms$X49_mean) / ms$X49_sd,
  z50 = (X50 - ms$X50_mean) / ms$X50_sd,
  z51 = (X51 - ms$X51_mean) / ms$X51_sd,
  z52 = (X52 - ms$X52_mean) / ms$X52_sd,
  z53 = (X53 - ms$X53_mean) / ms$X53_sd,
  z54 = (X54 - ms$X54_mean) / ms$X54_sd,
  z55 = (X55 - ms$X55_mean) / ms$X55_sd,
  z56 = (X56 - ms$X56_mean) / ms$X56_sd
)

df_z <- data_z %>% select(starts_with("z"))

# 信頼区間の設定
within_range <- function(x, na.rm = T) {
  -5 < x & x < 10 
}

row_packs_isnt_out <- row_packs(
  # 列に基づく非外れ値
  column = . %>% transmute_if(is.numeric, within_range)
)
# 適用されるルールの合計数を計算するため、ルールに従うものを残す
full_report <- df_z %>% 
  expose(row_packs_isnt_out, 
         .remove_obeyers = F) %>% 
  get_report()

breaker_report <- full_report %>% 
  filter(!(value %in% T))

num_outliers <- breaker_report %>% 
  filter(value == F)


### 外れ値の箇所を取り出す ###
for(i in 2:56){
  eval(parse(
    text = paste0("id_", i, "<- num_outliers %>% filter(rule == 'z", i, "') %>% select(id)")
  ))
}

### 外れ値部分0未満のものをNAとする ###
modified_ds <- ds

for(i in 2:56){
  eval(parse(
    text = paste0("modified_ds$X", i, "[id_", i, "$id] <- NA")
  ))
  eval(parse(
    text = paste0("modified_ds$X", i, "[which(modified_ds$X", i, "<= 0)] <- NA")
  ))
}

### 欠損値の補完 ###
No_label_df <- modified_ds[, -1]
# 多重代入法の適用
imp <- mice(No_label_df, m = 5, meth = "pmm")
# 補定済みデータセットの生成
impdf <- complete(imp, include = F)
complemented_df <- cbind(label = modified_ds$X1, impdf)
names(complemented_df) <- cnames

# 解析用データの保存
write_excel_csv(modified_ds, file.path(docu, "modified_df_2017.csv"))
write_excel_csv(complemented_df, file.path(docu, "analysis_df_2017.csv"))
