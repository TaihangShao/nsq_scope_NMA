
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/network-plot")

rm(list = ls())

library(tidygraph)
library(ggplot2)
library(ggraph)
library(gridExtra)

nodes <- data.frame(
  point = c("che","cam+che","niv+ipi","niv+ipi+che","ate+che","bev+che","ate+bev+che","pem+che","sin+che","niv+bev+che"),
  logrankP = c(1723,205,419,246,1093,613,359,410,266,275)
  # cluster = c("che","cam+che","niv+ipi","niv+ipi+che","ate+che","bev+che","ate+bev+che","pem+che","sin+che","niv+bev+che")
  )

edges <- data.frame(
  from = c("cam+che","niv+ipi","niv+ipi+che","ate+che","ate+che","ate+che","ate+bev+che","ate+bev+che","pem+che","sin+che","niv+bev+che"),
  to = c("che","che","che","che","che","bev+che","bev+che","ate+che","che","che","bev+che")
)

graph <- tbl_graph(nodes = nodes, edges = edges, directed = F) # 转换为合适的类型

# 使用ggraph画图

t1<-ggraph(graph, layout = 'linear', circular = T) +
  geom_edge_fan(strength=3) + # 画线，添加属性
  # scale_edge_width(range = c(1,2)) + # 设置线的粗细
  # scale_edge_color_manual(values = c('#C9DBED','#FFA2A4')) + # 改变颜色
  geom_node_point(aes(size = logrankP,fill="purple"),shape=21) + # 画点，添加属性
  # scale_fill_manual(values = c('#3A76A9','#D13021','#FDBF7B','#804B23','#81D8DD')) + # 改变颜色
  scale_size(range = c(8,18)) + # 设置点的大小
  # geom_node_point(aes(color=factors),size=5) + # 再叠加一层点！
  theme_graph() # 改变主题

grid.arrange(t1)

