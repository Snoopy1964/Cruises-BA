#--------------------------------------------------------------------------
#
# Verknüpfe Schiffslog mit Cases zu dem Analyse Datensatz ds
#   für jedes Schiff und jeden Tag muss ein Eintrag in cases.day sein, 
#   deshalb muss für fehlende Einträge in cases.day (es gibt Tage, an denen niemand erkrankt!)
#   für jeden Tag ohne Fall eine Zeile mit der Anzahl Cases = 0 erzeugt werden: 
# (1) Bilde eine fortlaufende TimeSeries (pro Tag) für jeden Infect (A09, A16, B0, B16, J11)
#       Join shiplog.day auf cases.day für jeden einzelnen Infect A09, etc.
#       -> left_join, damit es für jeden Tag einen Eintrag für A09, etc.
#       hierdurch wird die TimeSeries/Infect generiert.
#       Für Tage an denen es keine Fälle (unterschieden nach Crew und Passagiere gab)
#       ersetze bei der Anzahl der Fälle den NA Eintrag durch 0
# (2) Bilde einen Datensatz ds aus den einzelnen Infect Daten
# (3) Berechne die Inzidenzdichte pro ICD10-Code und Region
#       Dies ist das zentrale Tibble für alle Disease-Maps, chi^2 Tests,... 
# (4) Aufräumen!
#
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# (1) Bilde eine fortlaufende TimeSeries (pro Tag) für jeden Infect 
#--------------------------------------------------------------------------

ts.A09 <- shiplog.day                          %>%
  left_join(cases.day %>% dplyr::filter(Code.ID == "A09"), by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew    = if_else(is.na(Crew), 0, Crew), 
    Pax     = if_else(is.na(Pax), 0, Pax),
    Code.ID = "A09"
  )

ts.A16 <- shiplog.day                          %>%
  left_join(cases.day %>% dplyr::filter(Code.ID == "A16"), by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew    = if_else(is.na(Crew), 0, Crew), 
    Pax     = if_else(is.na(Pax), 0, Pax),
    Code.ID = "A16"
  )

ts.B01 <- shiplog.day                          %>%
  left_join(cases.day %>% dplyr::filter(Code.ID == "B01"), by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew    = if_else(is.na(Crew), 0, Crew), 
    Pax     = if_else(is.na(Pax), 0, Pax),
    Code.ID = "B01"
  )

ts.B02 <- shiplog.day                          %>%
  left_join(cases.day %>% dplyr::filter(Code.ID == "B02"), by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew    = if_else(is.na(Crew), 0, Crew), 
    Pax     = if_else(is.na(Pax), 0, Pax),
    Code.ID = "B02"
  )

ts.J11 <- shiplog.day                          %>%
  left_join(cases.day %>% dplyr::filter(Code.ID == "J11"), by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew    = if_else(is.na(Crew), 0, Crew), 
    Pax     = if_else(is.na(Pax), 0, Pax),
    Code.ID = "J11"
  )

#--------------------------------------------------------------------------
# (2) Bilde einen Datensatz ds aus den einzelnen Infect Daten
#--------------------------------------------------------------------------

ds <- rbind(ts.A09,ts.A16, ts.B01, ts.B02, ts.J11)

#--------------------------------------------------------------------------
# (3a) Berechne Inzidenzdichte pro ICD10-Code und Region
#---------------------------------------------------------------------------

ds.region <- ds %>%
  group_by(Region, Code.ID) %>%
  summarize(
    Nr.Days          = nrDays(Day),
    Nr.Cases.Crew    = sum(Crew),
    dNr.Cases.Crew   = sqrt(Nr.Cases.Crew),
    Nr.Cases.Pax     = sum(Pax),
    dNr.Cases.Pax    = sqrt(Nr.Cases.Pax),
    Nr.Cases         = Nr.Cases.Crew + Nr.Cases.Pax,
    dNr.Cases        = sqrt(Nr.Cases.Crew) + sqrt(Nr.Cases.Pax),
    PD.Pax           = sum(PaxNr),
    PD.Crew          = sum(CrewNr),
    PD.Pers          = PD.Pax + PD.Crew,
    ID.Pax           = Nr.Cases.Pax / PD.Pax ,
    ID1000.year.Pax  = ID.Pax*1000*365,
    ID.Crew          = Nr.Cases.Crew / PD.Crew ,
    ID1000.year.Crew = ID.Crew*1000*365,
    ID.Pers          = Nr.Cases / (PD.Pax + PD.Crew) ,
    ID1000.year.Pers = ID.Pers*1000*365
  )
#--------------------------------------------------------------------------
# (3b) Berechne Inzidenzdichte pro ICD10-Code und Schiff
#---------------------------------------------------------------------------

ds.ship <- ds %>%
  group_by(Schiff, Code.ID) %>%
  summarize(
    Nr.Days          = nrDays(Day),
    Nr.Cases.Crew    = sum(Crew),
    dNr.Cases.Crew   = sqrt(Nr.Cases.Crew),
    Nr.Cases.Pax     = sum(Pax),
    dNr.Cases.Pax    = sqrt(Nr.Cases.Pax),
    Nr.Cases         = Nr.Cases.Crew + Nr.Cases.Pax,
    dNr.Cases        = sqrt(Nr.Cases.Crew) + sqrt(Nr.Cases.Pax),
    PD.Pax           = sum(PaxNr),
    PD.Crew          = sum(CrewNr),
    PD.Pers          = PD.Pax + PD.Crew,
    ID.Pax           = Nr.Cases.Pax / PD.Pax ,
    ID1000.year.Pax  = ID.Pax*1000*365,
    ID.Crew          = Nr.Cases.Crew / PD.Crew ,
    ID1000.year.Crew = ID.Crew*1000*365,
    ID.Pers          = Nr.Cases / (PD.Pax + PD.Crew) ,
    ID1000.year.Pers = ID.Pers*1000*365
  )
#--------------------------------------------------------------------------
# (3c) Berechne Inzidenzdichte pro ICD10-Code, Region und Schiff
#---------------------------------------------------------------------------

ds.ship.region <- ds %>%
  group_by(Region, Schiff, Code.ID) %>%
  summarize(
    Nr.Days          = nrDays(Day),
    Nr.Cases.Crew    = sum(Crew),
    dNr.Cases.Crew   = sqrt(Nr.Cases.Crew),
    Nr.Cases.Pax     = sum(Pax),
    dNr.Cases.Pax    = sqrt(Nr.Cases.Pax),
    Nr.Cases         = Nr.Cases.Crew + Nr.Cases.Pax,
    dNr.Cases        = sqrt(Nr.Cases.Crew) + sqrt(Nr.Cases.Pax),
    PD.Pax           = sum(PaxNr),
    PD.Crew          = sum(CrewNr),
    PD.Pers          = PD.Pax + PD.Crew,
    ID.Pax           = Nr.Cases.Pax / PD.Pax ,
    ID1000.year.Pax  = ID.Pax*1000*365,
    ID.Crew          = Nr.Cases.Crew / PD.Crew ,
    ID1000.year.Crew = ID.Crew*1000*365,
    ID.Pers          = Nr.Cases / (PD.Pax + PD.Crew) ,
    ID1000.year.Pers = ID.Pers*1000*365
  )
#--------------------------------------------------------------------------
# (4) Aufräumen!
#--------------------------------------------------------------------------

rm(list = ls(pattern="ts\\.[ABJ]{0-9}{0-9}"))


