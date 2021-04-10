#
# (1) Data Sample cases
# (2) Anzahl Fälle pro Schiff
#
#
#
#

fileDir <- "./Results/WordDocs-doNotChange/"

#----------------------------------------------------------------
# (1) writing output of tables into Word File using package rtf
#----------------------------------------------------------------
rtffile <- RTF(file = str_c(fileDir, "Statistik_cases.doc"))

# Aggregation der Erstdiagnosen auf Tagesbasis (erste 10 Eintrtäge)
addParagraph(rtffile, "\n\n\nDie ersten 10 auf Tagesbasis aggregierten Erstdiagnosen von Varizellen")
addTable(rtffile,
         cases.day %>%
           dplyr::filter(Code.ID == "B02" & 
                         Schiff  == "MS1" &
                         Datum   >= "2016-02-12" & 
                         Datum   <= "2016-06-30" )
)


# Data Sample cases
addParagraph(rtffile, "\n\n\nBeispiel Daten für die Einzelfälle (cases)\n")
addTable(rtffile,
         cases.all %>%
           select(-c(
             "Code.Titel", "Gruppen.Titel", "Kapitel.Titel"
           )) %>%
           dplyr::filter(!is.na(Diagnose)) %>%
           sample_n(10))

# Anzahl Fälle für Crew und Passagiere
addParagraph(rtffile, "\n\n\nAnzahl Fälle (cases) für Crew und Passagiere\n")
addTable(rtffile,
         cases.all %>% group_by(PaxStatus) %>% summarize(Anzahl = n()))

# Anzahl Fälle für Crew und Passagiere unterschieden nach Geschlecht
addParagraph(rtffile, "\n\n\nAnzahl Fälle (cases) für Crew und Passagiere unterschieden nach Geschlecht\n")
addTable(rtffile,
         cases.all %>% group_by(PaxStatus, Geschlecht) %>% summarize(Anzahl = n()))

# Anzahl Fälle pro Schiff unterschieden nach Crew und Passagiere
addParagraph(
  rtffile,
  "\n\n\nAnzahl Fälle (cases) pro Schiff unterschieden nach Crew und Passagiere\n"
)
addTable(
  rtffile,
  cases.all %>% group_by(Schiff, PaxStatus) %>% summarize(
    Anzahl = n(),
    period.start = min(Datum),
    period.end = max(Datum)
  ) %>% spread(
    PaxStatus,
    value = Anzahl,
    fill = 0,
    sep = NULL
  ) %>%
    left_join(ships %>% select(Schiff, ComissioningDate),
              by = "Schiff") %>%
    select(Schiff, ComissioningDate, Crew, Pax, period.start, period.end)
  
)

# Altersverteilung der untersuchten Infektions-Fälle (Personen = Crew + Pax)
addParagraph(rtffile,
             "\n\nAltersverteilung der untersuchten Infektions-Fälle\n")
addTable(
  rtffile,
  cases.infect.codes %>%
    group_by(Code.ID) %>%
    summarize(
      Anzahl = n(),
      minAge = min(Alter),
      p25 = quantile(Alter, 0.25, na.rm = TRUE),
      p50 = quantile(Alter, 0.5, na.rm = TRUE),
      p75 = quantile(Alter, 0.75, na.rm = TRUE),
      maxAge = max(Alter)
    )
)

# Altersverteilung der untersuchten Infektions-Fälle (Pax)
addParagraph(rtffile,
             "\n\nAltersverteilung der untersuchten Infektions-Fälle Passagiere\n")
addTable(
  rtffile,
  cases.infect.codes %>% 
    dplyr::filter(PaxStatus == "Pax") %>%
    group_by(Code.ID) %>%
    summarize(
      Anzahl = n(),
      minAge = min(Alter),
      p25 = quantile(Alter, 0.25, na.rm = TRUE),
      p50 = quantile(Alter, 0.5, na.rm = TRUE),
      p75 = quantile(Alter, 0.75, na.rm = TRUE),
      maxAge = max(Alter)
    )
)

# Altersverteilung der untersuchten Infektions-Fälle (Crew)
addParagraph(rtffile,
             "\n\nAltersverteilung der untersuchten Infektions-Fälle Crew\n")
addTable(
  rtffile,
  cases.infect.codes %>% 
    dplyr::filter(PaxStatus == "Crew") %>%
    group_by(Code.ID) %>%
    summarize(
      Anzahl = n(),
      minAge = min(Alter),
      p25 = quantile(Alter, 0.25, na.rm = TRUE),
      p50 = quantile(Alter, 0.5, na.rm = TRUE),
      p75 = quantile(Alter, 0.75, na.rm = TRUE),
      maxAge = max(Alter)
    )
)

#-------------------------------------------------
# Vergleich Seekrankheit T75
#-------------------------------------------------
addParagraph(rtffile,
             "\n\nAnzahl Fälle Seekrankheit (T75.3)\n")
addTable(
  rtffile,
  cases.all %>%
    dplyr::filter(Code.ID == "T75" & !is.na(Geschlecht)) %>%
    group_by(PaxStatus, Geschlecht) %>%
    summarize(Cases = n())
)
#-------------------------------------------------
# combined data ds
#-------------------------------------------------

addParagraph(rtffile,
             "\n\n\nBeispiel Daten für die Cases incl. GeoInformationen\n")
addTable(rtffile,
         ds %>% dplyr::filter(Code.ID == "A09") %>% sample_n(10))


done(rtffile)
