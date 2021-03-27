##############################################################################################
#
# Definitionen
#
# - Schiffslogbuch (shiplog)
#     Das Schiffslogbuch gibt an, wo sich welches Schiff an welchem Tag befunden hat.
#     Der Ort des Schiffes entspricht den Geo-Koordinaten des Hafens.
#     An Seetagen wird hier die Geo-Koordinate des letzte Hafen benutzt.
#
# - Region:
#     frei definiertes Polygon, welches den Fahrgebieten der Schiffe entspricht.
#     Die Region ist somit eine Gruppierung von angefahrenen Häfen. An den Häfen ist 
#     die Geo-Koordinaten festgelegt. Alle Häfen werden einer Region zugeordnet. Dieser 
#     wiederum wird ein zentraler Punkt (geschätzt) als Mittelpunkt des Polygons 
#     einer Region als Geo-Koordinaten zugeordnet.
#
# - Fall (cases):
#   Ein Fall beschreibt das Auftreten einer nach ICD-10 GM kategorisierten Krankheit.
#   Dieser wird von einem Arzt im Bordhospital angelegt, sobald ein Patient 
#   im Hospital untersucht wurde (Erst-Diagnose). 
#   Eine weitere Konsultation aufgrund des gleichen ICD-10 Codes innerhalb einer Reise
#   wird der Erstdiagnose zugeordnet und führt nicht zu einem weiteren Fall.
#
#------------------------------------------------------------------------
# (0) Initialisierung 
#
# (1) Einlesen der Daten
#     - Fälle              aus File "data/Input/cases.all.csv" 
#     - Schiffslogbuch     aus File "data/Input/ships.log.csv
# (1-1) Iteration aufgrund fehlender Diagnose Daten im Zeitraum 1.1.2015 - 22.5.2015)
#    -> filtere den Schiffs-Log in diesem Zeitraum heraus, um bei der 
#       Erzeugung der Zeitstrahlen und der Bestimmung der Personentage
#       in einer Region die Ergebnisse nicht zu verfälschen
#
# (2) Berechnen der fortlaufenden Tage/Wochen/Monate/Jahre für den Schiffsfahrplan
#     Dies wird benötigt, um Aggregationen der Daten einfacher zu berechnen
#
# (3) Schiffsdaten (aus Internet)
#
#
#
#
##############################################################################################

#------------------------------------------------
# (0) Initialisierung 
#------------------------------------------------
source(".Rprofile")

#------------------------------------------------
# (1) Einlesen der Daten
#------------------------------------------------
# Cases (incidents) 
cases.all <- read_delim("data/Input/cases.all.csv",";", 
                        escape_double = FALSE, 
                        col_types = cols(
                          Datum = col_date(format = "%Y-%m-%d"),
                          Schiff = col_factor(levels = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6"))
                        ), 
                        trim_ws = TRUE)

# Schiffsfahrplan
shiplog.day <- read_delim(
  "data/Input/ships.log.csv",
  ";",
  escape_double = FALSE,
  col_types = cols(
    Datum  = col_date(format = "%Y-%m-%d"),
    Schiff = col_factor(levels = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6"))
  ),
  trim_ws = TRUE
)
#------------------------------------------------------------------------------------
# (1-1) Iteration aufgrund fehlender Diagnose Daten im Zeitraum 1.1.2015 - 22.5.2015)
#------------------------------------------------------------------------------------
shiplog.day <- shiplog.day %>% dplyr::filter(Schiff %in% c("MS1", "MS2", "MS4", "MS5", "MS6") |
                                (Schiff == "MS3" & Datum > as.Date("2015-05-22")))

#------------------------------------------------
# (2) Berechnen der fortlaufenden Tage/Wochen/Monate/Jahre für das Schiffslogbuch
#------------------------------------------------
shiplog.day <- shiplog.day %>% 
  mutate( Year  = year(Datum), 
          Month = (year(Datum) - year(date("2015-01-01")))*12 + month(Datum),
          Day   = as.numeric(Datum - date("2015-01-01")) + 1,
          Week  = trunc(Day/7) + 1
  )

#------------------------------------------------
# (3) Schiffsdaten (aus Internet)
#------------------------------------------------
ships <- tribble(~Schiff, ~ComissioningDate, ~CrewNr, ~MaxPaxNr, ~DecommisioningYear,
                 "MS1"  ,      "2009-05-15",     850,      1924,                2018,
                 "MS2"  ,      "2011-05-14",     850,      1912,                  NA,
                 "MS3"  ,      "2014-05-22",    1040,      2506,                  NA,
                 "MS4"  ,      "2015-05-15",    1040,      2506,                  NA,
                 "MS5"  ,      "2016-06-24",    1040,      2534,                  NA,
                 "MS6"  ,      "2017-05-12",    1040,      2534,                  NA) %>%
  mutate( 
    Schiff           = as.factor(Schiff),
    CrewNr           = as.integer(CrewNr),
    MaxPaxNr         = as.integer(MaxPaxNr),
    ComissioningDate = as.Date(ComissioningDate)
  )


