## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Helene Cecilia
##
## Date Created: 2021-07-13

rm(list=ls())

## Loading Packages  ------------------
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(treemap)
library(forcats) # for fct_reorder
library(ggrepel)
library(patchwork)
# remotes::install_github("coolbutuseless/ggpattern")
# library(ggpattern)

## Set Work Directory ------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location 

## -------------------------------
`%notin%` <- Negate(`%in%`)
world <- ne_countries(scale = "medium", returnclass = "sf")
spdf_tiny_countries <- ne_countries(type = 'tiny_countries', scale = 50, returnclass = "sf") 

articles <- read.csv("../input/grid_for_scripts.csv")

# Stat spatial models ----
summary(articles$spatial_model)
summary(as.factor(articles[articles$spatial_model == TRUE,]$theor_appli))
summary(as.factor(articles[articles$spatial_model == TRUE,]$scale))

summary(as.factor(articles[articles$spatial_model == TRUE & articles$theor_appli == "appli",]$scale))
summary(as.factor(articles[articles$spatial_model == TRUE & articles$theor_appli == "grey",]$scale))

summary(articles[articles$external_inf == TRUE,]$spatial)
summary(as.factor(articles[articles$external_inf == TRUE & articles$spatial_model == TRUE,]$specify_move))

# Map model locations ----
zone_appli <- articles[articles$theor_appli=="appli",]$zone
zone_grey <- articles[articles$theor_appli=="grey",]$zone

# Change some names to be recognized by ne_countries()
zone_appli <- replace(zone_appli, zone_appli == "Mayotte", "Comoros")
zone_appli <- replace(zone_appli, zone_appli == "Comoros archipelago", "Comoros")
zone_appli <- zone_appli[!zone_appli %in% c("Kenya, Tanzania","East African Community")]
zone_appli <- c(zone_appli,c("Burundi","Kenya","Rwanda","United Republic of Tanzania",
                             "Kenya","United Republic of Tanzania"))

zone_appli <- table(zone_appli)
zone_appli <- as.data.frame(zone_appli)

# télécharger les zones d'application des modèles
model_locations <- ne_countries(country = unique(zone_appli$zone_appli), returnclass = "sf")
# récupérer les exceptions (Comores pour Mayotte)
exception <- zone_appli[zone_appli$zone_appli %notin% model_locations$geounit,]$zone_appli
tiny <- spdf_tiny_countries[spdf_tiny_countries$name %in% exception,]
model_grey <- ne_countries(country = c("Egypt","Sudan"), returnclass = "sf")

model_locations$nb_model_appli <- NA
tiny$nb_model_appli <- NA
zone_appli$Freq <- as.factor(zone_appli$Freq)
for(z in c(model_locations$admin, tiny$admin)){
  f <- zone_appli[zone_appli$zone_appli == z,]$Freq
  if(z %in% model_locations$admin){
    model_locations[model_locations$admin == z,]$nb_model_appli <- f
  }else if(z %in% tiny$admin){
    tiny[tiny$admin == z,]$nb_model_appli <- f
  }
}

model_locations$nb_model_appli <- factor(model_locations$nb_model_appli, levels = levels(zone_appli$Freq))
tiny$nb_model_appli <- factor(tiny$nb_model_appli, levels = levels(zone_appli$Freq))

map <- ggplot() + geom_sf(data = world, fill = "antiquewhite", alpha = 0.5, size = 0.3 ) +
  geom_sf(data = model_locations, aes(fill = nb_model_appli)) +
  geom_sf(data = tiny, aes(color = nb_model_appli)) +
  geom_sf(data = model_grey, fill = "grey") +
  scale_color_viridis_d(direction = -1, drop = F) +
  scale_fill_viridis_d(direction = -1, drop = F) +
  guides(fill = guide_legend(nrow = 2, byrow = T),
         color = guide_legend(nrow = 2, byrow = T)) +
  labs(fill = "Number of\napplied models", color = "Number of\napplied models") +
  coord_sf(ylim = c(-35,70), xlim = c(-120,55)) +
  annotate(geom = "text", x = 51, y = -8, label = "Comoros", color = "#440154", size = 3) + 
  # ggtitle("A") +
  # coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") + # for round eart-like map
  theme_void() +
  theme(legend.position = c(0.1,0.18),
        text = element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.margin = margin(t=0,l=0,r=0,b=0))
# print(map)

# png(filename = "../paper/fig/map_models_text.png", width = 600, height = 400)
# plot(map)
# dev.off()

# Model scale ----
scales <- articles
scales[scales$scale == "sub",]$scale <- "sub national"
scales$dummy_y <- 0
scales[scales$scale == "international",]$dummy_y <- 4
scales[scales$scale == "national",]$dummy_y <- 3
scales[scales$scale == "sub national",]$dummy_y <- 2
scales[scales$scale == "local",]$dummy_y <- 1
scales$scale <- factor(scales$scale, levels = c("local","sub national","national","international")) #,"national","sub national","local"
scales[scales$zone == "United States of America",]$zone <- "USA"
scales[scales$theor_appli == "appli",]$theor_appli <- "applied"
scales[scales$zone == "Kenya, Tanzania",]$zone <- "Kenya and Tanzania"
scales[scales$zone == "Sudan, Egypt",]$zone <- "Egypt and Sudan"

# repeat several times if position of labels is not satisfying
plot_scale <- ggplot(scales[scales$theor_appli != "theor" & !is.na(scales$scale),],aes(x = theor_appli, y = scale)) + # y = dummy_y
  geom_text_repel(aes(label = zone,
                      color = scale),
                  max.overlaps = 20,
                  segment.color = 'white', size = 3) +
  # labs(y = "", x = "Model category", color = "Model scale") +
  labs(color = "", x = "Model category", y = "Model scale") +
  coord_cartesian(ylim = c(0.5,4.5)) +
  # ggtitle("B") +
  theme_classic() + theme(axis.text.x = element_text(size = 8),
                          axis.text.y = element_text(size = 8),
                          axis.title.x = element_text(size = 8),
                          axis.title.y = element_text(size = 8),
                          legend.title = element_text(size = 8),
                          legend.text = element_text(size = 8),
                          legend.position = "none", #right",
                          plot.title = element_text(size = 8),
                          plot.margin = margin(t=0,l=0,r=0,b=0))
# png(filename = "../paper/fig/scales_models.png", width = 650, height = 400)
# plot(plot_scale)
# dev.off()

# p <- map / plot_scale
# p <- p + plot_layout(heights = c(1,1.5))

p <- map | plot_scale
p <- p + plot_layout() + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 8))


tiff(filename = "../figures/Fig4.tiff", width = 2600, height = 900, res = 300)
plot(p)
dev.off()

# p <- p + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 13))
# png(filename = "../figures/models_map_and_scales.png", width = 1200, height = 400)
# plot(p)
# dev.off()

