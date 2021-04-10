
library("lubridate")
library("tidyverse")
library("rtf")
library("ggmap")
library("ggrepel")
library("ggpubr")
library("latex2exp")
library("broom")

#-------------------------------
# set options
#-------------------------------
# set encoding
options(encoding="UTF-8")

#-------------------------------
# load maps
#-------------------------------

if(!exists("map.Welt"))        load("data/Maps/map.Welt.Rdata")
if(!exists("Ports"))           load("data/Maps/Ports.Rdata")
if(!exists("Regions"))         load("data/Maps/Regions.Rdata")

#-------------------------------
# define usefull functions
#-------------------------------

# Berechnet die Anzahl der Tage aus der Länge der Spalte Day (fortlaufend)
# Die Funktion macht nur Sinn bei den Datensätze ds und shiplog.day
nrDays        <- function(x) {return(length(unique(x)))}
nrDays.Region <- function(x) {return(length(x))}

#------------------------------------
