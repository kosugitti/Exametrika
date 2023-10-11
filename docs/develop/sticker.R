library(hexSticker)
library(ggplot2)
library(Exametrika)
library(magick)

# setwd("develop/")
result.IRT <- IRT(J15S500, model = 3)
png("example_IRT.png", width = 800, height = 800)
plot(result.IRT, type = "ICC", items = 1:6, nc = 2, nr = 3)
dev.off()


result.LRA <- LRA(J15S500, ncls = 6)
png("example_LRA.png", width = 600, height = 600)
plot(result.LRA, type = "TRP", bg="transparent")
dev.off()

result.Ranklusteing <- Biclustering(J35S515, nfld = 5, ncls = 6, method = "R")
png("example_Rcl.png", width = 800, height = 800)
plot(result.Ranklusteing, type = "Array", bg="transparent")
dev.off()


imgurl <-"teacher_saiten_man.png"
# 画像を読み込む
img <- image_read(imgurl)
# 画像を縮小
small_img <- image_resize(img, "200x200")
# 縮小した画像を保存
image_write(small_img, "sticker_img.png")


sticker("example_Rcl.png",package="Exametrika", p_size=20, s_x=1, s_y=.71, s_width=0.5, s_height=0.5,
        h_fill="#39B900", h_color="#D3Dc12",
        filename="exametrika_1.png")

sticker("sticker_img.png",package="Exametrika", p_size=20, s_x=1, s_y=.71, s_width=0.5, s_height=0.5,
        h_fill="#39B900", h_color="#D3Dc12",
        filename="exametrika_2.png")



# Network Graph ---------------------------------------------------

# 必要なパッケージをインストールと読み込み
pacman::p_load(GGally,igraph,intergraph,ggraph,tidygraph,tidyverse)

# ノードとエッジのデータフレームを作成
nodes <- data.frame(id = 1:3, label = c("A", "B", "C"), x = c(1, 1, 1), y = c(1, 2, 3))
edges <- data.frame(from = c(1, 2), to = c(2, 3))

# tbl_graphオブジェクトを作成（有向グラフとして）
graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# ggraphでプロット
ggraph(graph) +
  geom_edge_link() +
  geom_node_point(aes(x = x, y = y)) +
  geom_node_text(aes(x = x, y = y, label = label), vjust = -1) +
  coord_fixed()


# graph renshu ----------------------------------------------------

## URL https://briatte.github.io/ggnet/

library(network)
library(sna)
library(ggplot2)
library(GGally)
# random graph
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

# vertex names
network.vertex.names(net) = letters[1:10]
ggnet2(net)
