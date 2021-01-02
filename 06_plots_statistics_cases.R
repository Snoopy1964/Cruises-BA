###################################################################
#
# Plots mit absoluten Cases
#
###################################################################


#--------------------------------------------------
#
#--------------------------------------------------

ds.A09 <-
  # ds %>% dplyr::filter(Code.ID == "A09" & Schiff == "MS4") %>%
  # ds %>% dplyr::filter(Code.ID == "A09" & Region == "Asien") %>%
  ds %>% dplyr::filter(Code.ID == "A09") %>%
  select(-c(`Port Name`, lng, lat))      %>%
  group_by(Day, Region)

gg <- ds.A09 %>% ggplot(aes(x=Day))

gg + 
  theme(legend.position = "bottom") +
  geom_bar(aes(y=Crew+Pax, fill = Schiff), stat="identity") +
  facet_wrap(~ Year, nrow=3, scales="free_x")

#------------------------------------------------------------------
ds.A09 <-
  # ds %>% dplyr::filter(Code.ID == "A09" & Schiff == "MS4") %>%
  ds %>% dplyr::filter(Code.ID == "A09" & Region == "Orient") %>%
  # ds %>% dplyr::filter(Code.ID == "A09") %>%
  select(-c(`Port Name`, lng, lat))      %>%
  group_by(Day, Region)

gg <- ds.A09 %>% ggplot(aes(x=Day))

gg + 
  theme(legend.position = "bottom") +
  geom_bar(aes(y=Crew+Pax, fill = Schiff), stat="identity") +
  facet_wrap(~ Year, nrow=3, scales="free_x")

#------------------------------------------------------------------
ds.A09 <-
  # ds %>% dplyr::filter(Code.ID == "A09" & Schiff == "MS4") %>%
  ds %>% dplyr::filter(Code.ID == "A09" & Region == "Kanaren") %>%
  # ds %>% dplyr::filter(Code.ID == "A09") %>%
  select(-c(`Port Name`, lng, lat))      %>%
  group_by(Day, Region)

gg <- ds.A09 %>% ggplot(aes(x=Day))

gg + 
  theme(legend.position = "bottom") +
  geom_bar(aes(y=Crew+Pax, fill = Schiff), stat="identity") +
  facet_wrap(~ Year, nrow=3, scales="free_x")

