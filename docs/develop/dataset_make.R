library(tidyverse)

# # ファイル名のリストを作成します。
# file_list <- c("J5S10", "J12S5000", "J35S515","J15S500","J20S400")
#
#
# # purrr::walk() を使って、各ファイルを読み込み、.RDataファイルとして保存します。
# walk(file_list, ~{
#   df <- read_csv(paste0("develop/sampleData/", .x, ".csv"))
#   assign(.x, df) # 変数名を設定します
#   save(list = .x, file = paste0("data/", .x, ".RData")) # save関数を使います
# })

J5S10 <- read.csv("develop/sampledata/J5S10.csv")
J12S5000 <- read.csv("develop/sampledata/J12S5000.csv")
J35S515 <- read.csv("develop/sampledata/J35S515.csv")
J15S500 <- read.csv("develop/sampledata/J15S500.csv")
J20S400 <- read.csv("develop/sampledata/J20S400.csv")

save(J5S10, file="data/J5S10.rda")
save(J12S5000, file="data/J12S5000.rda")
save(J35S515, file="data/J35S515.rda")
save(J15S500, file="data/J15S500.rda")
save(J20S400, file="data/J20S400.rda")
