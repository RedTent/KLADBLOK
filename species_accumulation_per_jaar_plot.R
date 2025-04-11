library(HHSKwkl)
library(tidyverse)
library(glue)
library(twn)
library(vegan)

theme_set(hhskthema())

# library(readxl)
# library(openxlsx)
# library(sf)
# library(leaflet)

# fys_chem <- data_online("fys_chem.rds")
meetpunten <- data_online("meetpunten.rds")
# parameters <- data_online("parameters.rds")
# normen <- data_online("normen.rds")
biologie <- data_online("biologie.rds")

# normen <- import_normen_rivm("P:/Dawaco/FME/normen.txt", parameters)
# bio_km <- read_csv2("P:/Dawaco/FME/biologie_kenmerken.csv")
# bio <- read_csv2("P:/Dawaco/FME/biologie.csv")
# ws_grens <- sf::st_read("data/ws_grens.gpkg", crs = 28992)

biologie %>% 
  mutate(naam = increase_taxonlevel(naam, "Species")) %>% 
  # filter(twn_taxonlevel(naam) == "Species") %>% 
  add_jaar() %>% 
  # group_by(jaar) %>% 
  summarise(n_taxa = n_distinct(naam)) %>% 
  arrange(desc(jaar))
  
data <- 
  biologie %>%  
  semi_join(filter(meetpunten, gebied == "KRIWA"), by = "mp") %>% 
  filter(methode == "MAFA") %>% 
  mutate(naam = increase_taxonlevel(naam, "Species")) %>% 
  group_by(mp, datum, naam) %>% 
  summarise(waarde = sum(waarde_stadium)) %>% 
  ungroup() %>% 
  mutate(naam = str_replace(naam, " ", "_")) %>% 
  add_jaar() %>% 
  select(mp, datum, jaar, naam, waarde) %>% 
  pivot_wider(names_from = naam, values_from = waarde, values_fill = 0) %>% 
  arrange(datum, mp)
  
accum_mafa <- 
  data %>% 
  filter(jaar > 2008) %>%
  select(-datum, -mp) %>% 
  group_by(jaar) %>% 
  nest() %>% 
  mutate(accum = map(data, specaccum),
         richness = map(accum, "richness")) %>% 
  unnest(richness) %>% 
  mutate(nr = row_number())

accum_mafa2 <- 
  accum_mafa %>% 
  rename(jaar2 = jaar) %>% 
  filter(nr <= 15)
  
  
accum_mafa %>% 
  filter(nr <= 15) %>%
  ggplot() + 
  geom_line(aes(nr, richness, group = jaar2), colour = "grey60", alpha = 0.6, data = accum_mafa2) + 
  geom_line(aes(nr, richness), colour = "red", size = 1.5) + 
  facet_wrap(~jaar, scales = "free_x", ncol = 5) + 
  scale_y_continuous(expand = expansion(c(0, 0.05)), limits = c(0, NA)) +
  labs(title = "Accumulatie van macrofaunasoorten",
       y = "Aantal soorten",
       x = "Aantal monsters",
       caption = "De lijn geeft aan hoeveel verschillende soorten worden gevonden in een bepaald aantal monsters.
       Hoe hoger de lijn hoe groter de biodiversiteit.")

