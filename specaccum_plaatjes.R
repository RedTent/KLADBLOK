library(HHSKwkl)
library(tidyverse)
library(twn)
library(vegan)


bio <- data_online("biologie.rds") %>% 
  filter(str_detect(mp, "^KRW"), methode == "MAFA") %>% 
  select(-contains("stadium")) %>% 
  distinct() %>% 
  # rename(waarde = waarde_totaal) %>% 
  add_jaar() %>% 
  mutate(wl_code = str_remove(mp, "_\\d\\d$"))


# accum_kp <- 
#   kp_fyto %>%   
#   mutate(naam = increase_taxonlevel(naam, "Species")) %>% 
#   group_by(datum, naam) %>% 
#   summarise(waarde = sum(waarde_totaal)) %>% 
#   mutate(naam = str_replace(naam, " ", "_")) %>% 
#   select(datum, naam, waarde) %>% 
#   pivot_wider(names_from = naam, values_from = waarde, values_fill = 0) %>% 
#   arrange(datum) %>% 
#   add_jaar() %>% 
#   # filter(jaar > 2014, jaar < 2020) %>% 
#   group_by(jaar) %>% 
#   select(-datum) %>% 
#   nest() %>% 
#   mutate(accum = map(data, specaccum),
#          richness = map(accum, "richness")) %>% 
#   unnest(richness) %>% 
#   mutate(nr = row_number())

accum <-
  bio %>% 
  mutate(naam = increase_taxonlevel(naam, "Species")) %>% 
  group_by(wl_code, mp, jaar, naam) %>%
  summarise(waarde = sum(waarde_totaal)) %>% 
  mutate(naam = str_replace(naam, " ", "_")) %>%
  # select(datum, naam, waarde) %>%
  pivot_wider(names_from = naam, values_from = waarde, values_fill = 0) %>% 
  arrange(mp, jaar) %>% 
  group_by(wl_code, jaar) %>% 
  select(-mp) %>% 
  nest() %>% 
  mutate(accum = map(data, specaccum),
         richness = map(accum, "richness")) %>%
  unnest(richness) %>%
  mutate(nr = row_number()) %>% 
  ungroup()

waterlichaam <- "KRW_BIP"

pdf("specaccum_mafa.pdf", width = 8, height = 6)
for (waterlichaam in sort(unique(accum$wl_code))) {

accum2 <- accum %>% rename(jaar2 = jaar) %>% filter(wl_code == waterlichaam)

plot_accum <- 
  accum %>% 
  filter(wl_code == waterlichaam) %>% 
  ggplot() + 
  geom_line(aes(nr, richness, group = jaar2), colour = "grey60", alpha = 0.6, data = accum2) + 
  geom_line(aes(nr, richness), colour = blauw, size = 1.5) + 
  geom_point(aes(nr, richness), colour = blauw, size = 2) + 
  facet_wrap(~jaar, scales = "free_x", ncol = 3) + 
  scale_y_continuous(expand = expansion(c(0, 0.05)), limits = c(0, NA)) + 
  scale_x_continuous(labels = scales::label_number(accuracy = 1), breaks = scales::breaks_pretty(3)) +
  labs(title = "Accumulatie van macrofaunasoorten",
       subtitle = waterlichaam,
       y = "Aantal soorten",
       x = "Aantal monsters",
       caption = "De lijn geeft aan hoeveel verschillende soorten worden gevonden in een bepaald aantal monsters.
       Hoe hoger de lijn hoe groter de biodiversiteit.
       De grijze lijnen geven de andere jaren weer.")  +
  HHSKwkl::thema_line_facet

print(plot_accum)  
  
}
dev.off()
