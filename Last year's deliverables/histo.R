### パッケージの読み込み ###
library(tidyverse)
library(gridExtra)

### ディレクトリ設定 ###
user.dir <- Sys.getenv("USERPROFILE")
docu <- paste0(user.dir, "\\Documents\\")
setwd(docu)

# データの読み込み
ds <- read_csv("dataset_2017.csv", col_names = F, skip = 1)
ds <- ds[, colnames(ds) != "X1"]
cnames <- read_csv("dataset_2017.csv", col_names = F, n_max = 1)
cnames <- cnames[, colnames(cnames) != "X1" ]
t_cnames <- as.data.frame(t(cnames))
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

{
  p1 <- ggplot(df_z, aes_string(x = names(df_z)[1])) + geom_histogram() + xlab(t_cnames$V1[1])
  p2 <- ggplot(df_z, aes_string(x = names(df_z)[2])) + geom_histogram() + xlab(t_cnames$V1[2])
  p3 <- ggplot(df_z, aes_string(x = names(df_z)[3])) + geom_histogram() + xlab(t_cnames$V1[3])
  p4 <- ggplot(df_z, aes_string(x = names(df_z)[4])) + geom_histogram() + xlab(t_cnames$V1[4])
  p5 <- ggplot(df_z, aes_string(x = names(df_z)[5])) + geom_histogram() + xlab(t_cnames$V1[5])
  p6 <- ggplot(df_z, aes_string(x = names(df_z)[6])) + geom_histogram() + xlab(t_cnames$V1[6])
  p7 <- ggplot(df_z, aes_string(x = names(df_z)[7])) + geom_histogram() + xlab(t_cnames$V1[7])
  p8 <- ggplot(df_z, aes_string(x = names(df_z)[8])) + geom_histogram() + xlab(t_cnames$V1[8])
  p9 <- ggplot(df_z, aes_string(x = names(df_z)[9])) + geom_histogram() + xlab(t_cnames$V1[9])
  p10 <- ggplot(df_z, aes_string(x = names(df_z)[10])) + geom_histogram() + xlab(t_cnames$V1[10])
  p11 <- ggplot(df_z, aes_string(x = names(df_z)[11])) + geom_histogram() + xlab(t_cnames$V1[11])
  p12 <- ggplot(df_z, aes_string(x = names(df_z)[12])) + geom_histogram() + xlab(t_cnames$V1[12])
  p13 <- ggplot(df_z, aes_string(x = names(df_z)[13])) + geom_histogram() + xlab(t_cnames$V1[13])
  p14 <- ggplot(df_z, aes_string(x = names(df_z)[14])) + geom_histogram() + xlab(t_cnames$V1[14])
  p15 <- ggplot(df_z, aes_string(x = names(df_z)[15])) + geom_histogram() + xlab(t_cnames$V1[15])
  p16 <- ggplot(df_z, aes_string(x = names(df_z)[16])) + geom_histogram() + xlab(t_cnames$V1[16])
  p17 <- ggplot(df_z, aes_string(x = names(df_z)[17])) + geom_histogram() + xlab(t_cnames$V1[17])
  p18 <- ggplot(df_z, aes_string(x = names(df_z)[18])) + geom_histogram() + xlab(t_cnames$V1[18])
  p19 <- ggplot(df_z, aes_string(x = names(df_z)[19])) + geom_histogram() + xlab(t_cnames$V1[19])
  p20 <- ggplot(df_z, aes_string(x = names(df_z)[20])) + geom_histogram() + xlab(t_cnames$V1[20])
  p21 <- ggplot(df_z, aes_string(x = names(df_z)[21])) + geom_histogram() + xlab(t_cnames$V1[21])
  p22 <- ggplot(df_z, aes_string(x = names(df_z)[22])) + geom_histogram() + xlab(t_cnames$V1[22])
  p23 <- ggplot(df_z, aes_string(x = names(df_z)[23])) + geom_histogram() + xlab(t_cnames$V1[23])
  p24 <- ggplot(df_z, aes_string(x = names(df_z)[24])) + geom_histogram() + xlab(t_cnames$V1[24])
  p25 <- ggplot(df_z, aes_string(x = names(df_z)[25])) + geom_histogram() + xlab(t_cnames$V1[25])
  p26 <- ggplot(df_z, aes_string(x = names(df_z)[26])) + geom_histogram() + xlab(t_cnames$V1[26])
  p27 <- ggplot(df_z, aes_string(x = names(df_z)[27])) + geom_histogram() + xlab(t_cnames$V1[27])
  p28 <- ggplot(df_z, aes_string(x = names(df_z)[28])) + geom_histogram() + xlab(t_cnames$V1[28])
  p29 <- ggplot(df_z, aes_string(x = names(df_z)[29])) + geom_histogram() + xlab(t_cnames$V1[29])
  p30 <- ggplot(df_z, aes_string(x = names(df_z)[30])) + geom_histogram() + xlab(t_cnames$V1[30])
  p31 <- ggplot(df_z, aes_string(x = names(df_z)[31])) + geom_histogram() + xlab(t_cnames$V1[31])
  p32 <- ggplot(df_z, aes_string(x = names(df_z)[32])) + geom_histogram() + xlab(t_cnames$V1[32])
  p33 <- ggplot(df_z, aes_string(x = names(df_z)[33])) + geom_histogram() + xlab(t_cnames$V1[33])
  p34 <- ggplot(df_z, aes_string(x = names(df_z)[34])) + geom_histogram() + xlab(t_cnames$V1[34])
  p35 <- ggplot(df_z, aes_string(x = names(df_z)[35])) + geom_histogram() + xlab(t_cnames$V1[35])
  p36 <- ggplot(df_z, aes_string(x = names(df_z)[36])) + geom_histogram() + xlab(t_cnames$V1[36])
  p37 <- ggplot(df_z, aes_string(x = names(df_z)[37])) + geom_histogram() + xlab(t_cnames$V1[37])
  p38 <- ggplot(df_z, aes_string(x = names(df_z)[38])) + geom_histogram() + xlab(t_cnames$V1[38])
  p39 <- ggplot(df_z, aes_string(x = names(df_z)[39])) + geom_histogram() + xlab(t_cnames$V1[39])
  p40 <- ggplot(df_z, aes_string(x = names(df_z)[40])) + geom_histogram() + xlab(t_cnames$V1[40])
  p41 <- ggplot(df_z, aes_string(x = names(df_z)[41])) + geom_histogram() + xlab(t_cnames$V1[41])
  p42 <- ggplot(df_z, aes_string(x = names(df_z)[42])) + geom_histogram() + xlab(t_cnames$V1[42])
  p43 <- ggplot(df_z, aes_string(x = names(df_z)[43])) + geom_histogram() + xlab(t_cnames$V1[43])
  p44 <- ggplot(df_z, aes_string(x = names(df_z)[44])) + geom_histogram() + xlab(t_cnames$V1[44])
  p45 <- ggplot(df_z, aes_string(x = names(df_z)[45])) + geom_histogram() + xlab(t_cnames$V1[45])
  p46 <- ggplot(df_z, aes_string(x = names(df_z)[46])) + geom_histogram() + xlab(t_cnames$V1[46])
  p47 <- ggplot(df_z, aes_string(x = names(df_z)[47])) + geom_histogram() + xlab(t_cnames$V1[47])
  p48 <- ggplot(df_z, aes_string(x = names(df_z)[48])) + geom_histogram() + xlab(t_cnames$V1[48])
  p49 <- ggplot(df_z, aes_string(x = names(df_z)[49])) + geom_histogram() + xlab(t_cnames$V1[49])
  p50 <- ggplot(df_z, aes_string(x = names(df_z)[50])) + geom_histogram() + xlab(t_cnames$V1[50])
  p51 <- ggplot(df_z, aes_string(x = names(df_z)[51])) + geom_histogram() + xlab(t_cnames$V1[51])
  p52 <- ggplot(df_z, aes_string(x = names(df_z)[52])) + geom_histogram() + xlab(t_cnames$V1[52])
  p53 <- ggplot(df_z, aes_string(x = names(df_z)[53])) + geom_histogram() + xlab(t_cnames$V1[53])
  p54 <- ggplot(df_z, aes_string(x = names(df_z)[54])) + geom_histogram() + xlab(t_cnames$V1[54])
  p55 <- ggplot(df_z, aes_string(x = names(df_z)[55])) + geom_histogram() + xlab(t_cnames$V1[55])
}

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,
             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
             p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,
             p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,
             p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,
             p51,p52,p53,p54,p55)
