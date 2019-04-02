#library(httr)
library(tidyverse)
library(sf)
library(HHSKwkl)
library(lubridate)

source("copy_data.R")
my_data_copy("meetpunten.csv")
source("functies_data_pdok.R")

meetpunten <- import_meetpunten() %>% st_as_sf(coords = c("x", "y"), crs = 28992, remove = FALSE)


# mpkm_gemeenten.csv ------------------------------------------------------

meetpunten %>% 
  st_join(get_gemeenten()) %>% 
  as_tibble() %>% 
  transmute(code_meetpunt = mp,
            code_kenmerk = "C02",
            datum = format(today(), "%d-%m-%Y"),
            tekst = gemeentenaam) %>% 
  write_delim("mpkm_gemeenten.csv", delim = ";", na = "")

# mpkm_plaatsnamen.csv ------------------------------------------------------

meetpunten %>% 
  st_join(get_woonplaatsen()) %>% 
  as_tibble() %>% 
  transmute(code_meetpunt = mp,
            code_kenmerk = "C03",
            datum = format(today(), "%d-%m-%Y"),
            tekst = woonplaats) %>% 
  write_delim("mpkm_plaatsnamen.csv", delim = ";", na = "")

  

# mpkm_bodem.csv ----------------------------------------------------------

# vertaling van bodemcode naar betekenisvolle info.
# meetpunten %>% 
#   st_join(get_bodem()) %>% 
#   as_tibble() %>% 
#   transmute(code_meetpunt = mp,
#             code_kenmerk = "XXX",
#             datum = format(today(), "%d-%m-%Y"),
#             tekst = woonplaats) %>% 
#   write_delim("mpkm_bodem.csv", delim = ";", na = "")
# 


# Adresinformatie ---------------------------------------------------------


meetpunten %>% #filter(mp == "00018A") %>% 
  filter(x != 0, y != 0) %>% 
  mutate(adresinfo = map2(x,y, get_adress2), 
         ADRES = map_chr(adresinfo, "adres"), 
         ADRES_PC = map_chr(adresinfo, "postcode"),
         ADRES_AFST = map_chr(adresinfo, "adres_afstand"),
         adresinfo = NULL) %>% 
  as.data.frame() %>%
  select(mp, ADRES, ADRES_PC, ADRES_AFST) %>% 
  gather(key = code_kenmerk, value = tekst, -mp) %>% 
  transmute(code_meetpunt = mp,
          code_kenmerk = code_kenmerk,
          datum = format(today(), "%d-%m-%Y"),
          waarde_tekst_getal = tekst) %>% 
  write_delim("adressen.csv", delim = ";", na = "")


# Bodemgebruik 2015 CBS -------------------------------------------------------

meetpunten %>% 
  st_join(get_bbg_2015()) %>% 
  as_tibble() %>% 
  filter(!is.na(Hoofdgroep)) %>% 
  transmute(code_meetpunt = mp,
            code_kenmerk = "BBG_2015",
            datum = format(today(), "%d-%m-%Y"),
            tekst = paste(Hoofdgroep,"|", Omschrijvi)) %>% 
  write_delim("mpkm_bbg_2015.csv", delim = ";", na = "")
