## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Helene Cecilia
##
## Date Created: 2021-04-19

rm(list=ls())

## Loading Packages  ------------------
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(treemap)
library(forcats) # for fct_reorder

## Set Work Directory ------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location 
getwd()

## Load source files ------------------
# source('.R')

## -------------------------------
articles <- read.csv("../input/grid_for_scripts.csv")

FOI <- as.data.frame(articles[,c("FOI","FOI_justif")])
colnames(FOI) <- c("type","justif")

FOI[FOI$type == "MA_pathogen",]$type <- "MA"
FOI <- FOI[FOI$type != "not_app",]

# FOI <- as.factor(FOI)
# levels(FOI)

FOI.stat <- FOI %>% group_by(type) %>% summarise(n = n(),
                                                 n_justif = sum(justif))
FOI.stat$prop <- FOI.stat$n/sum(FOI.stat$n)

p <- ggplot(FOI.stat) +
  geom_bar(aes(x=factor(type,levels=c("Hybrid3","Hybrid2","Hybrid1","MA","FI","FR")), fill = FALSE, y=n),stat="identity") +
  geom_bar(aes(x=factor(type,levels=c("Hybrid3","Hybrid2","Hybrid1","MA","FI","FR")), fill = TRUE, y=n_justif),stat="identity") +
  coord_flip() +
  scale_fill_manual(name = "Choice justified",
                    labels = c("No","Yes"),
                    values = c("pink","#69b3a2")) +
  scale_y_continuous(breaks = seq(0,12,3),
                     minor_breaks = seq(13)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = c(0.8,0.25),
    legend.title = element_text(size = 7.5),
    legend.text = element_text(size = 7)
  ) +
  xlab("FOI functional form") +
  ylab("Number of models")

tiff(file = "../figures/Fig5.tiff", width = 1200, height = 750, res = 300)
print(p)
dev.off()
