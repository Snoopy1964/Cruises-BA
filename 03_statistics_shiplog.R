#-------------------------------------------------------------------------
# Analyse des ship logs bzgl. Regionen, Häfen, etc.
#
# (1) Ergebisse als Word Tabellen mit R-Package rtf
# (2) plots Region map inkl. Häfen
#
#
#-------------------------------------------------------------------------
fileDir <-
  "./Results/WordDocs-doNotChange/"

#----------------------------------------------------------------
# (1) writing output of tables into Word File using package rtf
#----------------------------------------------------------------
rtffile <- RTF(file = str_c(fileDir, "Statistik_Schiffslog.doc"))
# Schiffsdaten
addParagraph(rtffile, "Schiffsdaten\n")
addTable(rtffile, ships)
         

# Indienststellung der Schiff
addParagraph(rtffile, "\n\n\nErste Fahrt eines Schiffes im Analysezeitraum\n")
addTable(rtffile,
         shiplog.day %>%
           group_by(Schiff) %>%
           summarize(min.date = min(Datum)))

# Erstes Datum, wann ein Schiff in einer Region war
addParagraph(rtffile, "\n\n\nErstes Datum wann ein Schiff in einer Region war\n")
addTable(
  rtffile,
  shiplog.day %>%
    group_by(Schiff, Region) %>%
    summarize(min.date = min(Datum)) %>%
    spread(Schiff, value = min.date)
)

# Passagierzahlen pro Schiff pro Jahr
addParagraph(rtffile,
             "\n\n\nDurchschnittliche Passagierzahlen pro Schiff pro Jahr\n")
addTable(
  rtffile,
  personNr.ship.day %>%
    group_by(Schiff, Year) %>%
    summarize(paxNrAvg = sum(PaxNr) / nrDays(Day)) %>%
    spread(Year, value = paxNrAvg)
)

# Anzahl Regionentage pro Schiff
addParagraph(rtffile, "\n\n\nAnzahl Regionentage pro Schiff\n")
addTable(
  rtffile,
  shiplog.day %>%
    group_by(Schiff, Region) %>%
    summarize(Anzahl = n()) %>%
    spread(Schiff, value = Anzahl, fill = 0) %>% 
    mutate(Sum.Region = MS1 + MS2 + MS3 + MS4 + MS5 + MS6)
)

# Anzahl Häfen insgesamt
addParagraph(rtffile, "\n\n\nAnzahl Häfen insgesamt\n")
addTable(rtffile,
         shiplog.day %>% summarize(Anzahl = n_distinct(`Port Name`)))


# Anzahl Häfen pro Region
addParagraph(rtffile, "\n\n\nAnzahl Häfen pro Region\n")
addTable(
  rtffile,
  shiplog.day %>%
    select(Region, `Port Name`) %>%
    group_by(Region) %>%
    summarize(Anzahl = n_distinct(`Port Name`))
  
)


# Anzahl Häfen pro Schiff
# --> das ist eine Tabelle mit 6 Spalten und 190 Zeilen. Viele Einträge sind 0
# print(
#   shiplog.day %>%
#     group_by(Schiff, `Port Name`) %>%
#     summarize(Anzahl = n()) %>%
#     spread(Schiff, value = Anzahl, fill = 0)
#
# )

# Sample Data Ship Log
addParagraph(rtffile, "\n\n\nBeispiel Daten für das Schiffs-Logbuch\n")
addTable(
  rtffile,
  shiplog.day %>% select(-c("Year", "Month", "Day", "Week")) %>% sample_n(10)
)


done(rtffile)

#
