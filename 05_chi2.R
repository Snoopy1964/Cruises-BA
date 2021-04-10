#--------------------------------------------------------------
#
# (1) Berechne chi^2
#
# (2) Berechne Odds-Ratios inkl. 95% confidence level 
#     mit Hilfe von Exact Fisher Test
#
#--------------------------------------------------------------

#--------------------------------------------------------------
# (1) Berechne chi^2
#--------------------------------------------------------------

# Chi^2 pro Region mit absoluten Zahlen
# Problem:
#   mittlere Anzahl Passagiere bleibt gleich, aber absolute 
#   Anzahl Fälle ist für unterschiedliche Zeiträume

# Chi^2 basierend auf Inzidenzen pro 1000 Personen und pro Jahr mit Nordamerika
x.A09.NA <- ds.region                                        %>% 
  dplyr::filter(Code.ID=="A09")                           %>%
  group_by(Region)                                        %>%
  mutate(
    Nr.Cases     = I1000.year.Pers,
    Nr.not.Cases = 1000 - Nr.Cases )                      %>%
  select(Region, Nr.Cases, Nr.not.Cases)

x.A09.cs2 <- as.data.frame((x.A09.NA[,2:3]))
dimnames(x.A09.cs2) <- list(pull(x.A09.NA[,1]), c("A09", "!A09"))
cs2 <- chisq.test(x.A09.cs2[])
print(cs2)
x.A09.NA <- x.A09.NA %>% 
  add_column(Residuen = cs2$residuals[,1]) %>% 
  add_column(Chi2 = (cs2$residuals[,1])**2)

# Chi^2 basierend auf Inzidenzen pro 1000 Personen und pro Jahr ohne Nordamerika
x.A09 <- ds.region                                        %>% 
  dplyr::filter(Code.ID=="A09" & Region != "Nordamerika") %>%
  group_by(Region)                                        %>%
  mutate(
    Nr.Cases     = I1000.year.Pers,
    Nr.not.Cases = 1000 - Nr.Cases )                      %>%
  select(Region, Nr.Cases, Nr.not.Cases)

x.A09.cs2 <- as.data.frame((x.A09[,2:3]))
dimnames(x.A09.cs2) <- list(pull(x.A09[,1]), c("A09", "!A09"))
cs2 <- chisq.test(x.A09.cs2[])
print(cs2)
x.A09 <- x.A09 %>% 
  add_column(Residuen = cs2$residuals[,1]) %>% 
  add_column(Chi2 = (cs2$residuals[,1])**2)

#--------------------------------------------------------------
# Chi^2 pro Schiff
x.A09.ship <- ds.ship                                     %>% 
  dplyr::filter(Code.ID=="A09")                           %>%
  group_by(Schiff)                                        %>%
  mutate(
    # Nr.avg.Crew  = PD.Crew,
    # Nr.avg.Pax   = PD.Pax,
    # Nr.Cases     = Nr.Cases.Crew + Nr.Cases.Pax,
    # Nr.not.Cases = (Nr.avg.Crew + Nr.avg.Pax)-Nr.Cases  ) %>%
    Nr.Cases     = I1000.year.Pers,
    Nr.not.Cases = 1000 - Nr.Cases )                      %>%
  select(Schiff, Nr.Cases, Nr.not.Cases)

x.A09.ship.cs2 <- as.data.frame((x.A09.ship[,2:3]))
dimnames(x.A09.ship.cs2) <- list(pull(x.A09.ship[,1]), c("A09", "!A09"))
cs2 <- chisq.test(x.A09.ship.cs2[])
print(cs2)
x.A09.ship <- x.A09.ship %>% 
  add_column(Residuen = cs2$residuals[,1]) %>% 
  add_column(Chi2 = (cs2$residuals[,1])**2)

#--------------------------------------------------------------
# (2) Berechne Odds-Ratios inkl. 95% confidence level 
#     mit Hilfe von Exact Fisher Test
#     ohne Nordamerika
#--------------------------------------------------------------

# Region
nr.regions <- length(x.A09$Region)
for (i in 1:nr.regions) {
  print(x.A09$Region[i])
  df = matrix(c(round(x.A09.cs2[i,1]),
                round(x.A09.cs2[nr.regions,1]),
                round(x.A09.cs2[i,2]),
                round(x.A09.cs2[nr.regions,2])), 
              nrow=2, ncol=2)
  print(df)
  if (i==1) {
    ft <- tidy(fisher.test(df, alternative = "two.sided", conf.level = 0.95))
    print(ft)
  } else {
    ft <- rbind(ft, tidy(fisher.test(df, alternative = "two.sided", conf.level = 0.95)))
    print(ft)
  }
}

x.A09 <- bind_cols(x.A09, ft)                     %>%
  # round numbers for plots and tables
  mutate(
    Nr.Cases     = round(Nr.Cases, digits = 0),
    Nr.not.Cases = round(Nr.not.Cases, digits = 0),
    Residuen     = round(Residuen, digits = 3),
    Chi2         = round(Chi2, digits = 3),
    estimate     = round(estimate, digits = 2),
    p.value      = round(p.value, digits = 2),
    conf.low     = round(conf.low, digits = 2),
    conf.high    = round(conf.high, digits = 2)
  )                                               %>%
  rename( Odds.Ratio = estimate)

# Schiff
nr.ships <- length(x.A09.ship$Schiff)
for (i in 1:nr.ships) {
  print(x.A09.ship$Schiff[i])
  df = matrix(c(round(x.A09.ship.cs2[i,1]),
                round(x.A09.ship.cs2[nr.ships,1]),
                round(x.A09.ship.cs2[i,2]),
                round(x.A09.ship.cs2[nr.ships,2])), 
              nrow=2, ncol=2)
  if (i==1) {
    ft <- tidy(fisher.test(df, alternative = "two.sided", conf.level = 0.95))
  } else {
    ft <- rbind(ft, tidy(fisher.test(df, alternative = "two.sided", conf.level = 0.95)))
  }
}

x.A09.ship <- bind_cols(x.A09.ship, ft)                     %>%
  # round numbers for plots and tables
  mutate(
    Nr.Cases     = round(Nr.Cases, digits = 0),
    Nr.not.Cases = round(Nr.not.Cases, digits = 0),
    Residuen     = round(Residuen, digits = 3),
    Chi2         = round(Chi2, digits = 3),
    estimate     = round(estimate, digits = 2),
    p.value      = round(p.value, digits = 2),
    conf.low     = round(conf.low, digits = 2),
    conf.high    = round(conf.high, digits = 2)
  )                                               %>%
  rename( Odds.Ratio = estimate)

#----------------------------
# Aufräumen!
#----------------------------

rm(x.A09.cs2, cs2, i, df, ft)

