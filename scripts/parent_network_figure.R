## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Helene Cecilia
##
## Date Created: 2021-11-18

rm(list=ls())

## Loading Packages  ------------------
library(ggplot2)
library(dplyr)
library(GGally)
library(network)
library(sna)
library(RColorBrewer)
library(intergraph)
library(tidyverse)
library(igraph)
library(wesanderson)

## Set Work Directory ------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location 
getwd()

## Load source files ------------------
# source('.R')
# Tutorial : https://briatte.github.io/ggnet/
# https://www.jessesadler.com/post/network-analysis-with-r/
## -------------------------------
`%notin%` <- Negate(`%in%`)

parent_link <- read.csv("../input/parent_network_edges.csv",header=F)
colnames(parent_link) <- c("parent","son")
nodes <- read.csv("../input/parent_network_nodes.csv")

# Some customing of the labels for a better rendering
nodes[is.na(nodes$FOI),]$FOI <- "NA"
nodes$label <- str_replace(nodes$label, "_20", " ")
nodes$label <- str_replace(nodes$label, "_", ",")

parent_link$parent <- str_replace(parent_link$parent, "_20", " ")
parent_link$parent <- str_replace(parent_link$parent, "_", ",")
parent_link$son <- str_replace(parent_link$son, "_20", " ")
parent_link$son <- str_replace(parent_link$son, "_", ",")

parent_link$son[parent_link$son == "Pedro 16b"] <- "Pedro 16*"
parent_link$parent[parent_link$parent == "Pedro 16b"] <- "Pedro 16*"
nodes$label[nodes$label == "Pedro 16b"] <- "Pedro 16*"

parent_link$son[parent_link$son == "Xue 13b"] <- "Xue 13**"
parent_link$parent[parent_link$parent == "Xue 13b"] <- "Xue 13**"
nodes$label[nodes$label == "Xue 13b"] <- "Xue 13**"

network <- graph_from_data_frame(d = parent_link, vertices = nodes, directed = T)

pal <- c("FR" = "#66C2A5", "FI" = "#FC8D62", "MA" = "#8DA0CB",
         "Hybrid1" = "#E78AC3", "NA" = "darkgrey")

p <- ggnet2(network, label = V(network)$name, color = V(network)$FOI, palette = pal,
       shape = V(network)$category, mode = "fruchtermanreingold", #layout.par = list(cell.jitter = 0.75),
       size = 7, label.size = 3, label.alpha = 0.85, edge.size = 1, edge.alpha = 0.75,
       arrow.size = 4, arrow.gap = 0.03, edge.color = "lightgrey") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8)) 
# "kamadakawai"
# "circle" : no
# "target" : no
# "fruchtermanreingold"

# each run will create a new network with a different positioning of nodes, so keep trying if you're not satisfied
tiff(filename = "../figures/Fig2.tiff", width = 2000, height = 1380, res = 300)
print(p)
dev.off()

