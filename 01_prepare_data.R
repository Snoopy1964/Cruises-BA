##################################################################################################
# Daten für Analyse vorbereiten
#
# (1) filtere cases
#     Data Samples:
#     -------------------------------------------------------------------------------------------------
#     cases.all ...........: alle cases im Analysezeitraum [2015-01-01, 2017-12-31]
#     cases.infect.chapters: alle infect cases (chapter 01 & 10) im Analysezeitraum  [2015-01-01, 2017-12-31]
#     cases.infect.codes...: für die Analyse ausgewählten infect.codes im Analysezeitraum [2015-01-01, 2017-12-31]
#
# (2) Vorbereitung der ausgewählten Infektions und Schiffslog Data Samples
#     - PaxStatus (Flag für Crew oder Passagier) in 2 Spalten aufteilen,
#       -> Tidying data (https://r4ds.had.co.nz/tidy-data.html#spreading)
#     - auf Tage aggregieren
#     - Bestimmung der Personen pro Tag auf den Schiffen
#
# (3) Verknüpfe Data Samples
#
# (4) Descriptive Statistik der Data Samples (Word Tabellen)
#     - Schiffslog
#     - Medizinisches Log plus kombinierte Daten
#
# (5) Plots der Data Samples
#
#
##################################################################################################

#----------------------------------------------------------
# (1) filtere cases
#----------------------------------------------------------

# all infect cases (chapter 01 & 10)
infect.chapters <- c(
  "01",   # Kapitel: 01 - Bestimmte infektiöse und parasitäre Krankheiten, Codes A00 - B99
  "10"    # Kapitel: 10 - Krankheiten des Atmungssystems                 , Codes J00 - J99
)

cases.infect.chapters <- cases.all %>% dplyr::filter(Kapitel.ID %in% infect.chapters )

# selected infect.codes for analysis
infect.codes <- c(
  "A09", # (Gastroenteritis)
  "B01", # (Varizellen)
  "B02", # (zoster)
  "J11", # (Grippe)
  "A16"  # (Tuberkulose)
  #-----------------------------------------------------------------------#
  # 2019.02.09 - rausgenommen, da sonst zu viel Daten für Bachelor Arbeit #
  #-----------------------------------------------------------------------#
  # "J00", # (Akute Rhinopharyngitis [Erkältungsschnupfen])
  # "J06", # (Akute Infektionen an mehreren oder nicht näher bezeichneten Lokalisationen der oberen Atemwege)
  # "J40"  # (Bronchitis, nicht als akut oder chronisch bezeichnet)
)

# filter auf infect codes, die in der Arbeit analysiert werden
cases.infect.codes <- cases.all %>% dplyr::filter(Code.ID %in% infect.codes )

#--------------------------------------------------------------------------
# (2) Vorbereitung der ausgewählten Infektions und Schiffslog Data Samples
#--------------------------------------------------------------------------

# Aggregation der Fälle auf Tages Ebene
cases.day <- cases.infect.codes               %>%
  group_by(Datum, Schiff, Code.ID, PaxStatus) %>%
  summarize(Anzahl = n())                     %>%
  # Tidying data (https://r4ds.had.co.nz/tidy-data.html#spreading), 
  # PaxStatus splitted die Observations in 2 Zeilen (Anzahl Crew, Anzahl Passagiere)
  # -> diese müssen zusammengeführt werden in eine Zeile
  spread(PaxStatus, value = Anzahl, fill = 0, sep=NULL)

# Bestimmung der Personen pro Tag auf den Schiffen
personNr.ship.day <-
  shiplog.day %>%
  select(Datum,
         Schiff,
         Region,
         PaxNr,
         CrewNr,
         PersNr,
         Year,
         Month,
         Day,
         Week)


#----------------------------------------------------------
# (3) Verknüpfe Data Samples
#----------------------------------------------------------

source("02_combine_data.R")

#----------------------------------------------------------
# (4) Descriptive Statistik der Data Samples (Word Tabellen)
#----------------------------------------------------------
#     - Schiffslog
source("03_statistics_shiplog.R")

#     - Medizinisches Log plus kombinierte Daten
source("04_statistics_cases.R")

