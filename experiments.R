#---------------------------------------------------------------------------
#
# Experimente
# (1) Abhängigkeit A09 von Schiff
# (2) Abhängigkeit A09 von Region und Schiff
#     Correlations?
#
#
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
# (1) Abhängigkeit A09 von Schiff
#     Berechne kumulative Inzidenz pro ICD10-Code und Schiff
#---------------------------------------------------------------------------

ds.ship <- ds %>%
  group_by(Schiff, Code.ID) %>%
  summarize(
    Nr.Days       = nrDays(Day),
    Nr.Cases.Crew = sum(Crew),
    Nr.Cases.Pax  = sum(Pax),
    Nr.Cases      = Nr.Cases.Crew + Nr.Cases.Pax,
    PD.Pax        = sum(PaxNr),
    PD.Crew       = sum(CrewNr),
    PD.Pers       = PD.Pax + PD.Crew,
    ID.Pax        = Nr.Cases.Pax / PD.Pax ,
    ID.Crew       = Nr.Cases.Crew / PD.Crew ,
    ID.Pers       = Nr.Cases / (PD.Pax + PD.Crew) ,
    CI1000.Pax    = CI1000(ID.Pax, Nr.Days),
    # CI1000.Pax_n  = Nr.Cases.Pax/(PD.Pax/Nr.Days) * 1000, # zum Vergleichen später in Kapitel Diskussion???
    CI1000.Crew   = CI1000(ID.Crew, Nr.Days),
    CI1000        = CI1000(ID.Pers, Nr.Days)
  )

#--------------------------------------------------------------
# (1a) Berechne chi^2
#--------------------------------------------------------------

x.A09.ship <- ds.ship                                        %>% 
  # dplyr::filter(Code.ID=="A09" & Region != "Nordamerika") %>%
  dplyr::filter(Code.ID=="A09")                           %>%
  group_by(Schiff)                                        %>%
  mutate(
    Nr.avg.Crew  = PD.Crew/Nr.Days,
    Nr.avg.Pax   = PD.Pax/Nr.Days,
    Nr.Cases     = Nr.Cases.Crew + Nr.Cases.Pax,
    Nr.not.Cases = (Nr.avg.Crew + Nr.avg.Pax)-Nr.Cases  ) %>%
  select(Schiff, Nr.Cases, Nr.not.Cases)

x.A09.cs2 <- as.data.frame((x.A09.ship[,2:3]))
dimnames(x.A09.cs2) <- list(pull(x.A09.ship[,1]), c("A09", "!A09"))
cs2 <- chisq.test(x.A09.cs2[])
x.A09.ship <- x.A09.ship %>% 
  add_column(Residuen = cs2$residuals[,1]) %>% 
  add_column(Chi2 = (cs2$residuals[,1])**2)

ds.ship %>% dplyr::filter(Code.ID == "A09") %>% ggplot(aes(x=Schiff, y=CI1000)) + geom_point()

#--------------------------------------------------------------
# (1b) Berechne Odds-Ratios inkl. 95% confidence level 
#     mit Hilfe von Exact Fisher Test
#--------------------------------------------------------------

i_comp <- 4
for (i in 1:length(x.A09.ship$Schiff)) {
  print(x.A09.ship$Schiff[i])
  df = matrix(c(x.A09.cs2[i,1],x.A09.cs2[i_comp,1],as.integer(x.A09.cs2[i,2]),as.integer(x.A09.cs2[i_comp,2])), nrow=2, ncol=2)
  if (i==1) {
    ft <- tidy(fisher.test(df, alternative = "two.sided", conf.level = 0.95))
  } else {
    ft <- rbind(ft, tidy(fisher.test(df, alternative = "two.sided", conf.level = 0.95)))
  }
}

x.A09.ship.tmp <- bind_cols(x.A09.ship, ft)                     %>%
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

#--------------------------------------------------------------------------
# (2) Abhängigkeit A09 von Region und Schiff
#     Correlations?
#     Berechne kumulative Inzidenz pro ICD10-Code,Region und Schiff
#---------------------------------------------------------------------------

ds.region.ship <- ds %>%
  group_by(Region, Schiff, Code.ID) %>%
  summarize(
    Nr.Days       = nrDays(Day),
    Nr.Cases.Crew = sum(Crew),
    Nr.Cases.Pax  = sum(Pax),
    Nr.Cases      = Nr.Cases.Crew + Nr.Cases.Pax,
    PD.Pax        = sum(PaxNr),
    PD.Crew       = sum(CrewNr),
    PD.Pers       = PD.Pax + PD.Crew,
    ID.Pax        = Nr.Cases.Pax / PD.Pax ,
    ID.Crew       = Nr.Cases.Crew / PD.Crew ,
    ID.Pers       = Nr.Cases / (PD.Pax + PD.Crew) ,
    CI1000.Pax    = CI1000(ID.Pax, Nr.Days),
    # CI1000.Pax_n  = Nr.Cases.Pax/(PD.Pax/Nr.Days) * 1000, # zum Vergleichen später in Kapitel Diskussion???
    CI1000.Crew   = CI1000(ID.Crew, Nr.Days),
    CI1000        = CI1000(ID.Pers, Nr.Days)
  )

#--------------------------------------------------------------
# (2a) Berechne chi^2
#--------------------------------------------------------------

x.A09.region.ship <- ds.region.ship                                        %>% 
  # dplyr::filter(Code.ID=="A09" & Region != "Nordamerika") %>%
  dplyr::filter(Code.ID=="A09")                           %>%
  group_by(Schiff)                                        %>%
  mutate(
    Nr.avg.Crew  = PD.Crew/Nr.Days,
    Nr.avg.Pax   = PD.Pax/Nr.Days,
    Nr.Cases     = Nr.Cases.Crew + Nr.Cases.Pax,
    Nr.not.Cases = (Nr.avg.Crew + Nr.avg.Pax)-Nr.Cases  ) %>%
  select(Region, Schiff, Nr.Cases, Nr.not.Cases)

x.A09.cs2 <- as.data.frame((x.A09.region.ship[,2:3]))
dimnames(x.A09.cs2) <- list(pull(x.A09.ship[,1]), c("A09", "!A09"))
cs2 <- chisq.test(x.A09.cs2[])
x.A09.region.ship <- x.A09.region.ship %>% 
  add_column(Residuen = cs2$residuals[,1]) %>% 
  add_column(Chi2 = (cs2$residuals[,1])**2)

#--------------------------------------------------------------
# (2b) Berechne Odds-Ratios inkl. 95% confidence level 
#      mit Hilfe von Exact Fisher Test
#--------------------------------------------------------------

i_comp <- 4
for (i in 1:length(x.A09.region.ship$Schiff)) {
  print(x.A09.ship$Schiff[i])
  df = matrix(c(x.A09.cs2[i,1],x.A09.cs2[i_comp,1],as.integer(x.A09.cs2[i,2]),as.integer(x.A09.cs2[i_comp,2])), nrow=2, ncol=2)
  if (i==1) {
    ft <- tidy(fisher.test(df, alternative = "two.sided", conf.level = 0.95))
  } else {
    ft <- rbind(ft, tidy(fisher.test(df, alternative = "two.sided", conf.level = 0.95)))
  }
}

x.A09.region.ship.tmp <- bind_cols(x.A09.region.ship, ft)                     %>%
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


#--------------------------------------------------------------
# (2c) Plots
#--------------------------------------------------------------
ds.region.ship %>% dplyr::filter(Code.ID == "A09") %>% ggplot(aes(x=Schiff, y=CI1000, color=Region)) + geom_point()
ds.region.ship %>% dplyr::filter(Code.ID == "A09") %>% ggplot(aes(x=Region, y=CI1000, color=Schiff)) + geom_point(size=2)


#--------------------------------------------------------------
# Wahrscheinlichkeitsverteilungen

# Binomialverteilung

plot_binom <- function(n, p) {
  x.tmp = 0:n
  return(
    tibble(x = x.tmp, y = dbinom(x.tmp, n, p)) %>%
      ggplot(aes(x = x.tmp, y = y)) +
      geom_bar(width = 0.05, stat = "identity")
  )
}

# Normalverteilung
gg <- ggplot(tibble(x=0), aes(x=x)) + xlim(0,30) + ylim(0,0.5)

for (i in 1:100) { 
  gg <- gg + stat_function(fun=dchisq, args=list(df=i))
}
gg

gg + 
  stat_function(fun = function(x) dchisq(x,1), color="blue") +
  stat_function(fun = function(x) dchisq(x,2), color="red") +
  stat_function(fun = function(x) dchisq(x,3), color="green") +
  stat_function(fun = function(x) dchisq(x,4), color="black") +
  stat_function(fun = function(x) dchisq(x,5), color="brown")
  

