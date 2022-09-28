## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Helene Cecilia
##
## Date Created: 2021-11-15

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
dim(articles[articles$year >= 2015,]) # denominator for number of papers with public code
summary(as.factor(articles$theor_appli))
summary(as.factor(articles[articles$theor_appli == "appli",]$scale))
summary(as.factor(articles[articles$theor_appli == "grey",]$scale))
summary(articles$spatial_model)
summary(as.factor(articles[articles$spatial_model==TRUE,]$theor_appli))
summary(as.factor(articles[articles$spatial_model==TRUE,]$specify_move))
summary(as.factor(articles[articles$scale=="international",]$spatial_model))
summary(as.factor(articles[articles$scale=="international" & articles$spatial_model == TRUE & articles$specify_move!="",]$theor_appli))
summary(as.factor(articles[articles$spatial_model == FALSE,]$external_inf))

summary(articles$data)
summary(articles[articles$theor_appli == "grey",]$data)
summary(as.factor(articles$nb_host))
summary(as.factor(articles$host_taxa))
summary(as.factor(articles$nb_vector))
summary(as.factor(articles$vector_taxa))
summary(as.factor(articles$primary_obj))
summary(as.factor(articles$model_obj))
summary(as.factor(articles$main_output))
summary(as.factor(articles[articles$theor_appli == "theor",]$primary_obj))
summary(as.factor(articles[articles$theor_appli == "appli",]$primary_obj))
summary(as.factor(articles[articles$theor_appli == "grey",]$primary_obj))
summary(as.factor(articles[articles$main_output == "sensitivity",]$theor_appli))
summary(as.factor(articles[articles$main_output == "estimation",]$theor_appli))
summary(as.factor(articles[articles$main_output == "estimation",]$secondary_obj))
summary(as.factor(articles$r0))
summary(as.factor(articles$FOI))
summary(as.factor(articles[articles$FOI != "not_app",]$FOI_justif))
articles %>% group_by(FOI,FOI_justif) %>% summarise(count = n())

# Connected papers ------
articles$author_list <- NA
authors <- list(c("Metras","Fournie","Dommergues","Camacho","Cavalerie","Merot","Keeling","Cetre-Sossah","Cardinale","Edmunds"),
                c("Metras","Edmunds","Youssoufi","Dommergues","Fournie","Camacho","Funk","Cardinale","Le Godais","Combo","Filleul","Youssouf","Subiros"),
                c("Cecilia","Metras","Fall","Lo","Lancelot","Ezanno"),
                c("Durand", "Lo", "Tran", "Ba", "Sow", "Belkhiria", "Fall", "Biteye", "Grosbois", "Chevalier"),
                c("Leedale", "Jones", "Caminade", "Morse"),
                c("Taylor", "Hagenlocher", "Jones", "Kienberger", "Leedale", "Morse"),
                c("Miron", "Giordano", "Kealey", "Smith"),
                c("Gaff", "Hartley", "Leahy"),
                c("Mpeshe", "Haaario", "Tchuenche"),
                c("Pedro", "Abelman", "Tonnang"),
                c("McMahon", "Manore", "Hyman", "LaBute", "Fair"),
                c("Gil", "Qualls", "Cosner", "DeAngelis", "Hassan", "Gad", "Ruan", "Cantrell", "Beier"),
                c("Gaff", "Burgess", "Jackson", "Niu", "Papelis", "Hartley"),
                c("Mpeshe", "Luboobi", "Nkansah-Gyekye"),
                c("Sumaye", "Jansen", "Berkvens", "De Baets", "Geubels", "Thiry", "Krit"),
                c("Fischer", "Boender", "Nodelijk", "de Koeijer", "van Roermund"),
                c("Nicolas", "Chevalier", "Tantely", "Fontenille", "Durand"),
                c("Barker", "Niu", "Reisen", "Hartley"),
                c("Niu", "Gaff", "Papelis", "Hartley"),
                c("Gachohi", "Njenga", "Kitala", "Bett"),
                c("Adongo", "Fister", "Gaff", "Hartley"),
                c("Gao", "Cosner", "Cantrell", "Beier","Ruan"),
                c("Pedro", "Abelman", "Tonnang"),
                c("Chamchod", "Cantrell", "Cosner", "Hassan", "Beier", "Ruan"),
                c("Chamchod", "Cosner", "Cantrell", "Beier", "Ruan"),
                c("Cavalerie", "Charron", "Ezanno", "Dommergues", "Zumbo", "Cardinale"),
                c("Xiao", "Beier","Cantrell","Cosner","DeAngelis","Ruan"),
                c("Chitnis", "Hyman", "Manore"),
                c("Paul", "Ndiaye", "Bah","Dione", "Ndione"),
                c("Bicout", "Sabatier"),
                c("Tuncer", "Gulbudak", "Cannataro", "Martcheva"),
                c("Scoglio", "Bosca", "Riad", "Sahneh", "Britch", "Cohnstaedt", "Linthicum"),
                c("Yang", "Nie"),
                c("Xue", "Scott", "Cohnstaedt", "Scoglio"),
                c("Xue", "Scoglio"),
                c("Manore", "Beechler"),
                c("Xue", "Cohnstaedt", "Scott", "Scoglio"),
                c("Pedro", "Abelman", "Ndjomatchoua", "Sang", "Tonnang"),
                c("Beechler", "Manore", "Reininghaus", "O'Neal", "Gorsich", "Ezenwa", "Jolles"),
                c("Sekamatte", "Riad","Tekleghiorghis","Linthicum","Britch","Richt","Gonzalez","Scoglio"),
                c("Nielsen", "Alvarez", "Bicout", "Calistri", "Depner", "Drewe", "Garin-Bastuji", "Gonzales Rojas", "Gortázar Schmidt", "Herskin", "Michel", "Miranda Chueca", "Pasquali", "Roberts", "Sihvonen", "Stahl", "Calvo", "Viltrop", "Winckler", "Gubbins", "Antoniou", "Broglia", "Abrahantes", "Dhollander", "Van der Stede"),
                c("Nielsen", "Alvarez", "Bicout", "Calistri", "Depner", "Drewe", "Garin-Bastuji", "Gonzales Rojas", "Gortázar Schmidt", "Herskin", "Michel", "Miranda Chueca", "Pasquali", "Roberts", "Sihvonen", "Stahl", "Calvo", "Viltrop", "Winckler", "Gubbins", "Antoniou", "Broglia", "Abrahantes", "Dhollander", "Van der Stede"),
                c("Tennant","Cardinale","Cetre-Sossah","Moutroifi","Le Godais","Colombi","Spencer","Tildesley","Keeling","Charafouddine","Colizza","Edmunds","Metras"),
                c("Lo Iacono","Cunningham","Bett","Grace","Redding","Wood"),
                c("Pedro","Tonnang","Abelman"),
                c("Wen","Teng","Liu"),
                c("Xue","Scoglio"),
                c("Pedro"),
                c("Mpeshe")
)

articles$author_list <- authors
with_parent <- articles[which(articles$parent1 != ""),]

category_parent_son <- NULL
for(m in 1:nrow(with_parent)){
  cat_son <- with_parent[m,"theor_appli"]
  parent_id <- with_parent[m,"parent1"]
  if(parent_id %in% articles$paper_id){
    cat_parent <- articles[articles$paper_id == parent_id,"theor_appli"]
    cat <- paste(cat_parent,cat_son,sep="-")
    category_parent_son <- c(category_parent_son,cat)
    print(paste(parent_id,with_parent[m,"paper_id"],cat, paste = ";"))
  }
}
summary(as.factor(category_parent_son))

# Do not count FOI = NA
parent_son_same_FOI <- NULL
parent_son_justif <- NULL
with_parent_noNA <- with_parent[with_parent$FOI != "not_app",]
for(m in 1:nrow(with_parent_noNA)){
  foi_son <- with_parent_noNA[m,"FOI"]
  justif_son <- with_parent_noNA[m,"FOI_justif"]
  parent_id <- with_parent_noNA[m,"parent1"]
  if(parent_id %in% articles$paper_id){
    foi_parent <- articles[articles$paper_id == parent_id,"FOI"]
    justif_parent <- articles[articles$paper_id == parent_id,"FOI_justif"]
    foi <- paste(foi_parent,foi_son,sep="-")
    justif <- paste(justif_parent,justif_son,sep="-")
    parent_son_same_FOI <- c(parent_son_same_FOI, foi_parent == foi_son)
    parent_son_justif <- c(parent_son_justif, justif)
    print(paste(parent_id,with_parent_noNA[m,"paper_id"],foi,justif, paste = ";"))
  }
}
summary(parent_son_same_FOI)
summary(as.factor(parent_son_justif))

zone_parent_son <- NULL
scale_parent_son <- NULL
for(m in 1:nrow(with_parent)){
  zone_son <- with_parent[m,"zone"]
  scale_son <- with_parent[m,"scale"]
  parent_id <- with_parent[m,"parent1"]
  if(parent_id %in% articles$paper_id){
    zone_parent <- articles[articles$paper_id == parent_id,"zone"]
    scale_parent <- articles[articles$paper_id == parent_id,"scale"]
    zones <- paste(zone_parent,zone_son,sep="-")
    scales <- paste(scale_parent,scale_son,sep="-")
    zone_parent_son <- c(zone_parent_son,zones)
    scale_parent_son <- c(scale_parent_son,scales)
    print(paste(parent_id,with_parent[m,"paper_id"],zones,scales, paste = ";"))
  }
}

share_author <- 0
parent_not_known <- 0
for(m in 1:nrow(with_parent)){
  #print(m)
  author_son <- unlist(with_parent[m,"author_list"])
  parent_id <- with_parent[m,"parent1"]
  if(parent_id %in% articles$paper_id){
    author_parent <- unlist(articles[articles$paper_id == parent_id,"author_list"])
    #i <- 0
    for(a in author_parent){
      #i <- i+1
      #print(i)
      #print(a)
      if(a %in% author_son){
        #print("Common author found")
        share_author <- share_author + 1
        break
      }
    }
  }else{
    parent_not_known <- parent_not_known + 1
  }
}
nb_with_parent_rvf <- nrow(with_parent) - parent_not_known
share_author/nb_with_parent_rvf
#-----

test <- articles %>% group_by(year,FOI) %>% summarize(count = n())
ggplot(test) + geom_point(aes(x = as.factor(FOI), y = year, size = count))

test <- articles %>% group_by(year,FOI_justif) %>% summarize(count = n())
ggplot(test) + geom_point(aes(x = as.factor(FOI_justif), y = year, size = count))
