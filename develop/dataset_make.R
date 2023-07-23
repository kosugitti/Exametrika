library(tidyverse)

# ファイル名のリストを作成します。
file_list <- c("J5S10", "J12S5000", "J35S515","J15S500","J20S400")


# purrr::walk() を使って、各ファイルを読み込み、.RDataファイルとして保存します。
walk(file_list, ~{
  df <- read_csv(paste0("develop/sampleData/", .x, ".csv"))
  assign(.x, df) # 変数名を設定します
  save(list = .x, file = paste0("data/", .x, ".RData")) # save関数を使います
})
