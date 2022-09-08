## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Helene Cecilia
##
## Date Created: 2021-07-19

rm(list=ls())

## Loading Packages  ------------------
library(ggplot2)
library(dplyr)
library(ggalluvial)

## Set Work Directory ------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location 
getwd()

## Load source files ------------------
# source('.R')

## -------------------------------
`%notin%` <- Negate(`%in%`)

articles <- read.csv("../input/grid_for_scripts.csv")

obj_cat <- articles[articles$main_output != "math_properties",] %>% group_by(primary_obj,theor_appli,main_output) %>% summarise(Freq = n())
# obj_cat2 <- articles %>% group_by(primary_obj,theor_appli,main_output, secondary_obj) %>% summarise(Freq = n())

obj_cat[obj_cat$theor_appli == "appli",]$theor_appli <- "applied"
obj_cat[obj_cat$theor_appli == "theor",]$theor_appli <- "theoretical"
obj_cat[obj_cat$main_output == "map_risk",]$main_output <- "risk map"

is_alluvia_form(obj_cat)

p <- ggplot(obj_cat,
            aes(y = Freq,
                axis3 = theor_appli, axis2 = primary_obj, axis1 = main_output)) +
  geom_alluvium(aes(fill = theor_appli),
                width = 1/6, knot.pos = 0, reverse = FALSE) +
  # guides(fill = FALSE) +
  labs(fill = "", y = "Number of models") +
  geom_stratum(width = 1/6, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE, size = 3) +
  scale_x_continuous(breaks = 1:3, labels = c("Main\noutput","Primary\nobjective","Model\ncategory")) +
  scale_fill_viridis_d(option = "D") +
  theme_classic() +
  coord_flip(ylim = c(-0.5,48), expand = F) +
  theme(legend.position = "top",
        legend.text = element_text(size = 8),
        axis.text.y = element_text(size = 8.5),
        axis.text.x = element_text(size = 8.5),
        axis.title.x = element_text(size = 8.5),
        plot.margin = margin(0,0,0,0))

tiff(filename = "../figures/Fig3.tiff", width = 2200, height = 880, res = 300)
print(p)
dev.off()

# png(filename = "../figures/model_category_objective_output_full_word.png", width = 1200, height = 500)
# print(p)
# dev.off()
