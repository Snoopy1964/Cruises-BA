#--------------------------------------------------------------------------------------
#
# Plots für die Arbeit, geordnet nach Kapiteln
#
#--------------------------------------------------------------------------------------

fileDir <-
  "C:/Users/user/OneDrive/Bachelorarbeit/Diagramme/"


#----------------------------------------------------------------------
# set some defaults
#----------------------------------------------------------------------
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))


#----------------------------------
# Kapitel 1
#----------------------------------




#--------------------------------------------------
# Kapitel 2
# (1) plot Region map incl. Ports
# (2) plot Personenanzahl pro Tag der Gesamtflotte
#-------------------------------------------------


# (1) plot Region map incl. Ports
gg.region <- ggmap(map.Welt) +
  geom_polygon(aes(long, lat, group = group, fill = region),
               data = Regions.Polygon,
               alpha = 1 / 3) +
  geom_point(
    data = Ports,
    mapping = aes(x = lng, y = lat),
    color = "dark blue",
    alpha = 1 / 3
  ) +
  geom_text(
    data = Regions,
    mapping = aes(x = lng, y = lat, label = Region),
    hjust = 0,
    nudge_x = -12,
    nudge_y =  2,
    size  = 3
  )

gg.region +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top")
  ) +
  labs(x    = "Geographische Länge (longitude)",
       y    = "Geographische Breite  (latitude)",
       fill = "Fahrgebiete")


print(str_c(fileDir, "Abb.2-1 Regions.jpeg"))
print(getwd())

file.name.tmp <- str_c(fileDir, "Abb.2-2 Regions.jpeg")
ggsave(
  file.name.tmp,
  device = "jpeg",
  width  = 210,
  height = 160,
  units  = "mm"
)
print(file.name.tmp)

#----------------------------------
# Kapitel 3
#----------------------------------


#----------------------------------
# Kapitel 4
#----------------------------------


#----------------------------------
# Kapitel 5
#----------------------------------

# Beispielberechnung Personenzeit
df <-
  tibble(
    start = c(1, 2, 1, 0),
    end = c(6, 7, 4, 3),
    P.Nr = c(1, 2, 3, 4)
  )

p1 <- ggplot(df) +
  # geom_segment(aes(
  #   x = start,
  #   xend = end,
  #   y = P.Nr,
  #   yend = P.Nr,
  #   size = 1
  # ), color = "blue", alpha=1/2) +
  # Do that in Word and group
  # geom_rect(aes(xmin=0.98, xmax=2.02, ymin=0.6, ymax=4.5), color = "red", fill = NA, size=1, alpha = 6/6) +
  # geom_rect(aes(xmin=0, xmax=7, ymin=2.8, ymax=3.2), color = "blue", fill =NA, size=1, alpha = 6/6) +
  geom_vline(xintercept = 0:7,
             linetype = "dashed",
             color = "dark gray") +
  geom_errorbarh(
    aes(y = P.Nr, xmin = start, xmax = end),
    height = 0.3,
    color = "black",
    size = 0.7
  ) +
  xlab("\t Tag in Woche") +
  ylab("Person") +
  ylim(0.7, 4.3) +
  annotate(
    "text",
    x = 3.5,
    y = 1.25,
    label = TeX("$\\Delta t_1$"),
    size = 5
  ) +
  annotate(
    "text",
    x = 4.5,
    y = 2.25,
    label = TeX("$\\Delta t_2$"),
    size = 5
  ) +
  annotate(
    "text",
    x = 2.5,
    y = 3.25,
    label = TeX("$\\Delta t_3$"),
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 4.25,
    label = TeX("$\\Delta t_4$"),
    size = 5
  ) +
  # ylab("Person (blau) / Personentage (rot)") +
  # xlim(0,8) +
  # geom_text(data=tibble(x=c(0.5), y=(3.5), tex="$\\sum^{day 1}$"), aes(x=x, y=y, label=TeX(tex))) +
  theme(legend.position = "none",
        axis.title.x = element_text(vjust = -0.5, hjust = 0.6)) # +
# theme(legend.position = "none", plot.margin = margin(1, 1, 2, 2, "cm")) # +
# geom_bar(data=tibble(x=0.5:6.5, y=c(1,3,4,3,2,2,1)), aes(x=x, y=y), fill="red", stat = "identity", alpha=1/5)

p1
file.name.tmp <- str_c(fileDir, "Abb.5-2 PersonIntervals.old.png")
ggsave(
  file.name.tmp,
  device = "png",
  height = 140 * 4 / 7,
  width  = 200 * 2 / 3,
  units  = "mm"
)
print(file.name.tmp)


p2 <- ggplot(df) +
  geom_vline(xintercept = 1:4,
             linetype = "dashed",
             color = "dark gray") +
  ylab("Summe über Tage/Person") +
  # ylab(TeX("\\sum(Tage/Person)")) +
  xlab("") +
  xlim(0.7, 4.3) +
  coord_flip() +
  # xlim(0,8) +
  # geom_text(data=tibble(x=c(0.5), y=(3.5), tex="$\\sum^{day 1}$"), aes(x=x, y=y, label=TeX(tex))) +
  theme(legend.position = "none",
        axis.title.x = element_text(vjust = -0.5)) +
  geom_bar(
    data = tibble(x = 1:4, y = c(5, 5, 3, 3)),
    aes(x = x, y = y),
    width = 0.45,
    fill = "blue",
    stat = "identity",
    alpha = 1 / 2
  )

p3 <- ggplot(df) +
  geom_vline(xintercept = 0:7,
             linetype = "dashed",
             color = "dark gray") +
  xlab("Tag in Woche") +
  scale_y_reverse() +
  ylab("Summe über Personen/Tag") +
  # ylab(TeX("\\sum(Personen/Tag)")) +
  # xlim(0,8) +
  # geom_text(data=tibble(x=c(0.5), y=(3.5), tex="$\\sum^{day 1}$"), aes(x=x, y=y, label=TeX(tex))) +
  scale_x_continuous(position = "top") +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  geom_bar(
    data = tibble(x = 0.5:6.5, y = c(1, 3, 4, 3, 2, 2, 1)),
    aes(x = x, y = y),
    width = 0.5,
    fill = "red",
    stat = "identity",
    alpha = 1 / 2
  )

# p4 <- ggplot() +
#   geom_blank() +
#   annotate(geom = "text", x=0, y=0, label = TeX("$\\sum Anzahl Personen/Tag$")) +
#   theme(axis.title = element_blank(), axis() = element_blank())

file.name.tmp <- str_c(fileDir, "Abb.5-2 PersonIntervals.png")
ggarrange(
  p1,
  p2,
  ncol = 2,
  nrow = 1,
  widths = c(2, 1)
)

ggsave(
  file.name.tmp,
  device = "png",
  height = 140 * 4 / 7,
  units  = "mm"
)
print(file.name.tmp)


file.name.tmp <- str_c(fileDir, "Abb.5-3 Person-Pax-Days.png")
ggarrange(
  p1,
  p2,
  p3,
  ncol = 2,
  nrow = 2,
  widths = c(2, 1),
  heights = c(2, 1.5)
)


ggsave(
  file.name.tmp,
  device = "png",
  height = 140,
  width  = 200,
  units  = "mm"
)
print(file.name.tmp)

#----------------------------------
# Kapitel 6
#----------------------------------


# plot Personenanzahl pro Tag der Gesamtflotte
# # Durchschnittliche Personenanzahl der Flotte
# personNr.ship.day %>% summarize(paxNrAvg = sum(PaxNr) / nrDays(Day))
# 
# # Durchschnittliche Personenanzahl der Flotte pro Jahr
# personNr.ship.day %>% group_by(Year) %>% summarize(paxNrAvg = sum(PaxNr) / nrDays(Day))
# 
# # Durchschnittliche Personenanzahl der Schiffe
# personNr.ship.day %>% group_by(Schiff) %>% summarize(paxNrAvg = sum(PaxNr) / nrDays(Day))
# 
# # Durchschnittliche Personenzahl der Schiffe pro Jahr
# personNr.ship.day %>% group_by(Schiff, Year) %>% summarize(paxNrAvg = sum(PaxNr) / nrDays(Day))

# Passagier-, Crew- und Personenanzahl pro Tag (Personen Tage), aus shiplog.day
gg <- personNr.ship.day %>%
  group_by(Day, Datum)  %>%
  summarize(PaxNr = sum(PaxNr) / nrDays(Day),
            CrewNr = sum(CrewNr) / nrDays(Day)) %>%
  ggplot()

gg +
  theme(
    legend.position = c(0.98, 0.98),
    legend.title = element_blank(),
    legend.justification = c("right", "top"),
    text = element_text(size = 16)
  ) +
  # geom_smooth(aes(x = Datum, y = PaxNr)) +
  geom_line(aes(x = Datum, y = PaxNr), color = "black")   +
  geom_line(aes(x = Datum, y = CrewNr), color = "red") +
  ylim(0, 16000) +
  labs(x    = "Datum",
       y    = "Anzahl Passagiere/Crew")
  

file.name.tmp <- str_c(fileDir, "Abb.6-1 PersonNr.day.png")
ggsave(
  file.name.tmp,
  device = "png",
  width  = 210,
  height = 160,
  units  = "mm"
)
print(file.name.tmp)

# Passagier-, Crew- und Personenanzahl pro Tag pro Schiff (Personen Tage), aus shiplog.day
gg <- personNr.ship.day %>%
  group_by(Day, Datum, Schiff)  %>%
  summarize(PaxNr = sum(PaxNr) / nrDays(Day),
            CrewNr = sum(CrewNr) / nrDays(Day)) %>%
  ggplot()

gg +
  # geom_smooth(aes(x = Datum, y = PaxNr)) +
  geom_line(aes(x = Datum, y = PaxNr), color = "black")   +
  # geom_line(aes(x = Datum, y = CrewNr), color = "red") +
  facet_wrap( ~ Schiff) +
  ylim(0, 3000)

file.name.tmp <- str_c(fileDir, "Abb.6-1-a PersonNr.day.ship.png")
ggsave(
  file.name.tmp,
  device = "png",
  width  = 210,
  height = 160,
  units  = "mm"
)
print(file.name.tmp)

# Altersverteilung des untersuchten Samples (Histogramm)
gg <-
  cases.infect.codes %>% dplyr::filter(Alter < 100) %>% group_by(PaxStatus) %>% ggplot()
gg  +
  theme(
    legend.position = c(0.98, 0.98),
    legend.title = element_blank(),
    legend.justification = c("right", "top"),
    text = element_text(size = 16)
  ) +
  labs(x    = "Alter in Jahren",
       y    = "Anzahl",
       fill = " ") +
  geom_histogram(aes(x = Alter, fill = PaxStatus), bins = 99) +
  ggtitle("Altersverteilung (Histogramm)")


file.name.tmp <- str_c(fileDir, "Abb.6-3 Age.infect.histogram.png")
ggsave(
  file.name.tmp,
  device = "png",
  width  = 210,
  height = 160,
  units  = "mm"
)
print(file.name.tmp)

# Altersverteilung der untersuchten Infektions-Fälle (Boxplot pro ICD10-Code)
gg +
  theme(
    legend.position = c(0.92, 0.98),
    legend.title = element_blank(),
    legend.justification = c("center", "top"),
    text = element_text(size = 16)
  ) +
  labs(x    = "ICD-10 GM Code",
       y    = "Alter",
       fill = "NONE")+
  geom_boxplot(aes(x = Code.ID, y = Alter, fill = PaxStatus))  +
  # coord_flip()+
  ggtitle("Altersverteilung pro ICD10-Code (Boxplot)")

file.name.tmp <-
  str_c(fileDir, "Abb.6-4 Age.infect.ICD10.boxplot.png")
ggsave(
  file.name.tmp,
  device = "png",
  width  = 210,
  height = 160,
  units  = "mm"
)
print(file.name.tmp)



#----------------------------------
# A09 - Gastroenteritis
#----------------------------------

#----------------------------------------------------------------------
# Summe aller cases in einer Woche pro Schiff, aggregiert über Regionen
#----------------------------------------------------------------------
ds.A09.tmp <- ds                                    %>%
  dplyr::filter(Code.ID == "A09")  %>%
  group_by(Week, Schiff)                            %>%
  summarize(
    Nr.Cases.Crew    = sum(Crew),
    Nr.Cases.Pax     = sum(Pax),
    Nr.Cases         = Nr.Cases.Crew + Nr.Cases.Pax,
    Nr.Days          = nrDays(Day),
    PD.Pax           = sum(PaxNr),
    PD.Crew          = sum(CrewNr),
    PD.Pers          = PD.Pax + PD.Crew,
    ID.Pax           = Nr.Cases.Pax / PD.Pax ,
    ID.Crew          = Nr.Cases.Crew / PD.Crew ,
    ID.Pers          = Nr.Cases / (PD.Pax + PD.Crew) ,
    ID1000.week.Pers = ID.Pers * 1000 * 7
  )

gg <- ggplot(ds.A09.tmp, aes(x = Week))

# Plotte Neuerkrankungen pro 1000 Personen pro Woche 
gg +
  # geom_step(aes(y=Nr.Cases), alpha = 1/2) +
  geom_bar(aes(y = ID1000.week.Pers), stat = "identity", alpha = 3 / 4) +
  # geom_smooth(aes(x = Week, y = ID1000.week.Pers),
  #             span = 6 / 12,
  #             fullrange = FALSE) +
  theme(
    legend.position = c(0.92, 0.98),
    legend.title = element_blank(),
    legend.justification = c("center", "top"),
    text = element_text(size = 12)
  ) +
  facet_wrap(~ Schiff, scales = NULL) +
  ggtitle("7-Tage Inzidenz pro 1000 Personen für Gastroenteritis (A09) je Schiff") +
  xlab("Woche ab 1.1.2015 bis 31.12.2017") +
  ylab("Neuerkrankungen pro 1000 Personen pro Woche") +
  geom_hline(yintercept = 10, color = "yellow", alpha = 2/4)       +
  geom_text(aes(150,11, label="OPP 1"), size = 3, color = "darkgrey")     +
  geom_hline(yintercept = 15, color = "orange", alpha = 2/4)       +
  geom_text(aes(150,16, label="OPP 2"), size = 3, color = "darkgrey")     +
  geom_hline(yintercept = 20, color = "red", alpha = 2/4)          +
  geom_text(aes(150,21, label="OPP 3"), size = 3, color = "darkgrey")
  
file.name.tmp <-
  str_c(fileDir, "Abb.6-5 epi_curve.A09.ships.ID1000.week.png")
ggsave(
  file.name.tmp,
  device = "png",
  width  = 210,
  height = 160,
  units  = "mm"
)
print(file.name.tmp)

#----------------------------------------------------------------------
# Summe aller cases in einer Woche pro Region, aggregiert über Schiffe
#----------------------------------------------------------------------

# Plotte Neuerkrankungen pro 1000 Personen pro Woche Fälle
ds.A09.tmp <- ds                                    %>%
  dplyr::filter(Code.ID == "A09")  %>%
  group_by(Week, Region)                            %>%
  summarize(
    Nr.Cases.Crew    = sum(Crew),
    Nr.Cases.Pax     = sum(Pax),
    Nr.Cases         = Nr.Cases.Crew + Nr.Cases.Pax,
    Nr.Days          = nrDays(Day),
    PD.Pax           = sum(PaxNr),
    PD.Crew          = sum(CrewNr),
    PD.Pers          = PD.Pax + PD.Crew,
    ID.Pax           = Nr.Cases.Pax / PD.Pax ,
    ID.Crew          = Nr.Cases.Crew / PD.Crew ,
    ID.Pers          = Nr.Cases / (PD.Pax + PD.Crew) ,
    ID1000.week.Pers = ID.Pers * 1000 * 7
  )

gg <- ggplot(ds.A09.tmp, aes(x = Week))

gg +
  geom_bar(aes(y = ID1000.week.Pers), stat = "identity", alpha = 3 / 4) +
  # macht hier keinen Sinn, da es sehr viele Lücken zwischen den Zeiträumen gibt
  # geom_smooth(aes(x=Week, y=CI1000), span = 6/12, fullrange = FALSE) +
  theme(
    legend.position = c(0.92, 0.98),
    legend.title = element_blank(),
    legend.justification = c("center", "top"),
    text = element_text(size = 12)
  ) +
  facet_wrap(~ Region, scales = NULL) +
  ggtitle("7-Tage Inzidenz pro 1000 Personen für Gastroenteritis (A09) je Region") +
  xlab("Woche ab 1.1.2015 bis 31.12.2017") +
  ylab("Neuerkrankungen pro 1000 Personen pro Woche")

file.name.tmp <-
  str_c(fileDir, "Abb.6-6 epi_curve.A09.regions.ID1000.week.png")
ggsave(
  file.name.tmp,
  device = "png",
  width  = 210,
  height = 160,
  units  = "mm"
)
print(file.name.tmp)

#------------------------------------------------
# Ansätze für Korrelationsanalyse
#------------------------------------------------

# Plotte Neuerkrankungen pro 1000 Personen pro Woche pro Schiff und pro Region
ds.A09.tmp <- ds                                    %>%
  dplyr::filter(Code.ID == "A09")  %>%
  group_by(Week, Region, Schiff)                            %>%
  summarize(
    Nr.Cases.Crew = sum(Crew),
    Nr.Cases.Pax  = sum(Pax),
    Nr.Cases      = Nr.Cases.Crew + Nr.Cases.Pax,
    Nr.Days       = nrDays(Day),
    PD.Pax        = sum(PaxNr),
    PD.Crew       = sum(CrewNr),
    PD.Pers       = PD.Pax + PD.Crew,
    ID.Pax        = Nr.Cases.Pax / PD.Pax ,
    ID.Crew       = Nr.Cases.Crew / PD.Crew ,
    ID.Pers       = Nr.Cases / (PD.Pax + PD.Crew) ,
    ID1000.year.Pers = ID.Pers * 1000 * 365,
    CI1000.Pax    = CI1000(ID.Pax, Nr.Days),
    # CI1000.Pax_n  = Nr.Cases.Pax/(PD.Pax/Nr.Days) * 1000, # zum Vergleichen später in Kapitel Diskussion???
    CI1000.Crew   = CI1000(ID.Crew, Nr.Days),
    CI1000        = CI1000(ID.Pers, Nr.Days)
  )

gg <- ggplot(ds.A09.tmp, aes(y = Region, x = Schiff))

gg +
  # geom_step(aes(y=Nr.Cases), alpha = 1/2) +
  geom_jitter(aes(size = ID.Pers*7*1000), alpha = 2 / 4, color = "blue") +
  ggtitle("7-Tage Inzidenz pro 1000 Personen für Gastroenteritis (A09)") +
  theme(
    # legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top")
  ) +
  labs(x    = " ",
       y    = " ",
       size = "7-Tage Inzidenz:\nAnzahl Fälle\npro 1000 Personen\npro Woche")

file.name.tmp <-
  str_c(fileDir, "Abb.7-1a epi_curve.A09.ships.regions.ID.Pers1000.week.png")
ggsave(
  file.name.tmp,
  device = "png",
  width = 210,
  height = 160,
  units = "mm"
)
print(file.name.tmp)

# Plotte Neuerkrankungen pro 1000 Personen pro Personen Jahr pro Schiff und pro Region
df <- ds.ship.region %>% dplyr::filter(Code.ID == "A09" & Nr.Days>=28) %>% select(Schiff, Region, ID1000.year.Pers)
gg <- ggplot(data=df, aes(x=Schiff, y=Region, size=ID1000.year.Pers))
gg + 
  geom_point(alpha=1/2, color = "blue") + scale_size_continuous(range=c(1,24)) +
  theme(
    # legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top")
  ) +
  labs(x    = " ",
       y    = " ",
       size = "Inzidenz:\nAnzahl Fälle\npro 1000 Personen\npro Jahr")


file.name.tmp <-
  str_c(fileDir, "Abb.7-1b epi_curve.A09.ships.regions.ID.Pers1000.year.png")
ggsave(
  file.name.tmp,
  device = "png",
  width = 210,
  height = 160,
  units = "mm"
)
print(file.name.tmp)




#----------------------------------------------------------------------------------------------
# Kapitel 6.2.2 -> Disease Map für Regionen

#----------------------------------------------------------------------------------------------
# join CI auf Regionen Tabelle für CI Zahlen als Text (bezogen auf den Mittelpunkt der Region)
#----------------------------------------------------------------------------------------------
# relative Häufigkeit pro Infektions Code und pro Region im gesamten Zeitraum
# join CI auf Regionen Tabelle für CI Werte am Mittelpunkt der Region
ds.region.tmp <- ds.region %>%
  # select(Region, Code.ID, CI1000.Crew, CI1000.Pax, CI1000, Nr.Days) %>%
  select(Region, Code.ID, ID1000.year.Crew, ID1000.year.Pax, ID1000.year.Pers) %>%
  left_join(Regions, by = "Region")

# join CI auf Regionen.Polygons Tabelle für Füllfarbe der Regionen nach CI
ds.polygon <- ds.region %>%
  # select(Region, Code.ID, CI1000.Crew, CI1000.Pax, CI1000, Nr.Days) %>%
  select(Region, Code.ID, ID1000.year.Crew, ID1000.year.Pax, ID1000.year.Pers) %>%
  left_join(Regions.Polygon, by = c("Region" = "region"))
#-----------------------
# A09 - Gastroenteritis
#-----------------------

# Disease Map Regionen für Passagiere
ggmap(map.Welt) +
  ggtitle("Disease Map - Gastroenteritis (A09) im Zeitraum 2015 - 2017 Passagiere") +
  geom_polygon(
    data = ds.polygon %>% dplyr::filter(Code.ID == "A09"),
    aes(long, lat, group = group, fill = ID1000.year.Pax),
    alpha = 0.7
  ) +
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,220)) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    text = element_text(size = 13)
  ) +
  labs(x    = "Geographische Länge (longitude)",
       y    = "Geographische Breite  (latitude)",
       fill = "Inzidenz:\nAnzahl Fälle\npro 1000 Pers.\npro Jahr") +
  geom_text(
    data = Regions,
    mapping = aes(x = lng,
                  y = lat,
                  label = Region),
    nudge_x = 0,
    nudge_y = -2,
    angle = 0,
    size = 3.5
  ) +
  geom_text(
    data = ds.region.tmp %>% dplyr::filter(Code.ID == "A09"),
    mapping = aes(
      x = lng,
      y = lat,
      label = sprintf("%.1f", ID1000.year.Pax)
    ),
    nudge_x = 0,
    nudge_y = 2,
    size = 4
  )

file.name.tmp <-
  str_c(fileDir, "Abb.6-7-a diseaseMap.A09.region.pax.jpeg")
ggsave(
  file.name.tmp,
  device = "jpeg",
  width  = 210,
  height = 160,
  units  = "mm"
)
print(file.name.tmp)

# Disease Map Regionen für Crew
ggmap(map.Welt) +
  ggtitle("Disease Map - Gastroenteritis (A09) im Zeitraum 2015 - 2017 Crew") +
  geom_polygon(
    data = ds.polygon %>% dplyr::filter(Code.ID == "A09"),
    aes(long, lat, group = group, fill = ID1000.year.Crew),
    # aes(long, lat, group = group, fill = CI1000.Crew),
    alpha = 0.7
  ) +
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,220)) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    text = element_text(size = 13)
  ) +
  labs(x    = "Geographische Länge (longitude)",
       y    = "Geographische Breite  (latitude)",
       fill = "Inzidenz:\nAnzahl Fälle\npro 1000 Pers.\npro Jahr") +
  geom_text(
    data = Regions,
    mapping = aes(x = lng,
                  y = lat,
                  label = Region),
    nudge_x = 0,
    nudge_y = -2,
    angle = 0,
    size = 3.5
  ) +
  geom_text(
    data = ds.region.tmp %>% dplyr::filter(Code.ID == "A09"),
    mapping = aes(
      x = lng,
      y = lat,
      label = sprintf("%.1f", ID1000.year.Crew)
    ),
    nudge_x = 0,
    nudge_y = 2,
    size = 4
  )

file.name.tmp <-
  str_c(fileDir, "Abb.6-7-b diseaseMap.A09.region.crew.jpeg")
ggsave(
  file.name.tmp,
  device = "jpeg",
  width  = 210,
  height = 160,
  units  = "mm"
)
print(file.name.tmp)

#----------------------------------------------
# Kapitel 6.2.3 
# ---------------------------------------------
# Residuen des chi2 tests - inkl. Nordamerika
x.A09.NA %>%
  ggplot(aes(x = Region, y = Residuen)) + geom_bar(stat = "identity") + 
  labs(
    x = " ",
    y = "Residuen") +
  coord_flip()
# ggplot(aes(x=Region, y=Chi2)) + geom_bar(stat="identity") + coord_flip()

rtffile <- RTF(file = str_c(fileDir, "\\chi2-odds-ratio.doc"))

# Tabelle mit Chi2
addParagraph(rtffile, "\n\n\nErgebnis Chi2 Test\n")
addTable(rtffile,
         x.A09.NA %>%
           select(c(
             "Region",
             "Nr.Cases",
             "Nr.not.Cases",
             "Residuen",
             "Chi2"
           )))

file.name.tmp <-
  str_c(fileDir, "Abb.6-8 residuals chi2 A09 per region.png (inkl. Nordamerika).jpeg")
ggsave(file.name.tmp,
       device  = "png",
       width  = 210,
       height = 100,
       units  = "mm")
print(file.name.tmp)

# Residuen des chi2 tests - ohne Nordamerika
x.A09 %>%
  ggplot(aes(x = Region, y = Residuen)) + geom_bar(stat = "identity") + 
  labs(
    x = " ",
    y = "Residuen") +
  coord_flip()
# ggplot(aes(x=Region, y=Chi2)) + geom_bar(stat="identity") + coord_flip()

rtffile <- RTF(file = str_c(fileDir, "\\chi2-odds-ratio.doc"))

# Tabelle mit Chi2
addParagraph(rtffile, "\n\n\nErgebnis Chi2 Test\n")
addTable(rtffile,
         x.A09 %>%
           select(c(
             "Region",
             "Nr.Cases",
             "Nr.not.Cases",
             "Residuen",
             "Chi2"
           )))

file.name.tmp <-
  str_c(fileDir, "Abb.6-9 residuals chi2 A09 per region.png (ohne Nordamerika).jpeg")
ggsave(file.name.tmp,
       device  = "png",
       width  = 210,
       height = 100,
       units  = "mm")
print(file.name.tmp)

# Odds-Ratios
x.A09 %>%
  #dplyr::filter(Region != "Westeuropa") %>%
  ggplot(aes(x = Region, y = Odds.Ratio)) +
  geom_point()                       +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.4,
    color = "black",
    size = .7
  ) +
  geom_text(
    aes(label = Odds.Ratio),
    nudge_x = 0.4,
    nudge_y = 0,
    angle = 0,
    size = 3.5
  ) +
  xlab("") +
  ylab("Odds-Ratio") +
  ggtitle("Odds-Ratio A09 pro Region im Vergleich zu Westeuropa \n(95% Confidence Intervalle)") +
  coord_flip()

file.name.tmp <-
  str_c(fileDir, "Abb.6-10 odds-ratio A09 per region.png")
ggsave(file.name.tmp,
       device = "png",
       width = 210,
       units = "mm")
print(file.name.tmp)

addParagraph(rtffile,
             "\n\n\nOdds-ratios der Regionen im Vergleich zu Westeuropa\n")
addTable(rtffile,
         x.A09 %>%
           select(c(
             "Region",
             "Odds.Ratio",
             "p.value",
             "conf.low",
             "conf.high"
           )))

done(rtffile)

#---------------------
# A16, B01, B02, J11
#---------------------

# Disease Map für Regionen Crew + Passagiere
ggmap(map.Welt) +
  ggtitle("Disease Map - Infektionskrankheiten II im Zeitraum 2015-2017") +
  geom_polygon(
    data = ds.polygon %>% dplyr::filter(Code.ID %in% c("A16", "B01", "B02", "J11")),
    aes(long, lat, group = group, fill = ID1000.year.Pers),
    alpha = 0.7
  ) +
  facet_wrap( ~ Code.ID) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme(
    legend.position = c(0.994, 0.99),
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 8)
  ) +
  labs(x    = "Geographische Länge (longitude)",
       y    = "Geographische Breite  (latitude)",
       fill = "Inzidenz:\nAnzahl Fälle\npro 1000 Personen\npro Jahr") +
  geom_text(
    data = Regions,
    mapping = aes(x = lng,
                  y = lat,
                  label = Region),
    nudge_x = 10,
    nudge_y = -3,
    angle = 0,
    size = 2.5
  ) +
  geom_text(
    data = ds.region.tmp %>% dplyr::filter(Code.ID %in% c("A16", "B01", "B02", "J11")),
    mapping = aes(
      x = lng,
      y = lat,
      label = sprintf("%.2f", ID1000.year.Pers)
    ),
    nudge_x = 0,
    nudge_y = 1,
    size = 3
  )

file.name.tmp <-
  str_c(fileDir,
        "Abb.6-11 diseaseMap.A16+B01+B02+J11.region.all.jpeg")
ggsave(
  file.name.tmp,
  device = "jpeg",
  width  = 210,
  height = 160,
  units  = "mm"
)
print(file.name.tmp)


#----------------------------------
# Aufräumen
#----------------------------------

rm(list = ls(pattern = "p[0-9]"))
rm(list = ls(pattern = ".*\\.tmp"))

