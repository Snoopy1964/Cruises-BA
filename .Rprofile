
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
nrDays         <- function(x) {return(length(unique(x)))}
nrDays.Region <- function(x) {return(length(x))}

# Zusammenhang cumulated Incidence CI im Zeitraum Delta (in Tagen) und 
# Incidencerate ID (berechnet in Tagen)
# Kreienbrock et. al, S.28
CI1000 <- function(ID,Delta) {
  return((1 - exp(-ID*Delta)) * 1000)
}
#------------------------------------
# Das ist offensichtlich flasch, wenn man sich den Plot pro Woche anschaut!
# CI1000 <- function(ID,Delta) {
#   # Zusammenhang cumulated Incidence CI und Incidence-density ID
#   # Kreienbrock et. al, S.28
#   # (1 - exp(-ID*Delta)) * 1000
#   #
#   # Umrechnng auf den Zeitraum ein Jahr, Regionentage sind immer unterschiedlich
#   (1 - exp(-ID*Delta)) * 1000 *365/Delta 
# }
