library(tidyverse)


toelatingen_db <- tempfile(fileext = ".xls")
download.file("https://toelatingen.ctgb.nl/documents/public-authorisations-report.xls", toelatingen_db, method = "curl")

readxl::read_excel(toelatingen_db, guess_max = 5000) %>% glimpse()

toelatingen <- 
  readxl::read_excel(toelatingen_db, guess_max = 5000) %>%
  select(middelnaam, startdatum, expiratiedatum, toepassing, biogewas, 
         aard_werking = `aard werking`, werkzame_stoffen = `werkzamestoffen NL`,
         casnr = `CASnummer(s) van werkzame stof(fen)`) %>%
  filter(!is.na(werkzame_stoffen),
         !toepassing == "Niet Professioneel",
         biogewas == "gewasbescherming") %>% 
  mutate(status = ifelse(expiratiedatum > Sys.Date(), "Toegelaten", "Verlopen")) %>% 
  arrange(desc(expiratiedatum))

library(HHSKwkl)
library(tidyverse)
library(glue)

theme_set(hhskthema())

# library(readxl)
# library(openxlsx)
# library(sf)
# library(leaflet)

fys_chem <- data_online("fys_chem.rds")
meetpunten <- data_online("meetpunten.rds")
parameters <- data_online("parameters.rds")
normen <- data_online("normen.rds")

# normen <- import_normen_rivm("P:/Dawaco/FME/normen.txt", parameters)
# bio_km <- read_csv2("P:/Dawaco/FME/biologie_kenmerken.csv")
# bio <- read_csv2("P:/Dawaco/FME/biologie.csv")
# ws_grens <- sf::st_read("data/ws_grens.gpkg", crs = 28992)

toelatingen_opgeschoond <- 
  toelatingen %>% 
  mutate(werkzame_stoffen_orig = werkzame_stoffen,
         aard_werking_orig = aard_werking) %>% 
  tidyr::separate_longer_delim(cols = c(werkzame_stoffen, aard_werking), delim = " # ") %>% 
  mutate(werkzame_stoffen = str_remove(werkzame_stoffen, " .*$")) %>% 
  mutate(werkzame_stoffen = str_remove(werkzame_stoffen, ",$")) %>% 
  mutate(werkzame_stoffen_case = werkzame_stoffen) %>% 
  mutate(werkzame_stoffen = str_to_lower(werkzame_stoffen))

obv_cas <- 
  parameters %>%
  filter(parnr > 999, parnr < 2000) %>%
  left_join(toelatingen_opgeschoond, by = "casnr") %>%
  filter(!is.na(middelnaam))

library(fuzzyjoin)



obv_fuzzy <- parameters %>%
  filter(parnr > 999, parnr < 2000) %>% 
  mutate(parnaamlang = str_to_lower(parnaamlang)) %>% 
  fuzzyjoin::stringdist_left_join(toelatingen_opgeschoond, by = c("parnaamlang" = "werkzame_stoffen"), max_dist = 1) %>% 
  rename(casnr = casnr.x) %>% 
  select(-casnr.y)
  # arrange(desc(afstand)) %>% 
  # relocate(parnaamlang, werkzame_stoffen) %>% 
  # filter(is.na(middelnaam)) %>% 
  # View()

# obv_fuzzy2 <- 
#   parameters %>%
#   filter(parnr > 999, parnr < 2000) %>% 
#   mutate(par = str_to_lower(par)) %>% 
#   left_join(toelatingen_opgeschoond, by = c("par" = "werkzame_stoffen"), keep = TRUE) %>% 
#   rename(casnr = casnr.x) %>% 
#   select(-casnr.y) %>%  
#   # arrange(desc(afstand)) %>% 
#   relocate(par, werkzame_stoffen) %>%
#   # filter(is.na(middelnaam)) %>%
#   View()

parameters_plus <- 
  obv_cas %>% 
  dplyr::bind_rows(obv_fuzzy) %>% 
  distinct() 


aangetroffen <- fys_chem %>% 
  filter(parnr > 999, parnr < 2000, is.na(detectiegrens)) %>% 
  left_join(parameters_plus) %>%  
  count(parnr)
  


parameters_plus %>% 
  select(parnr, par, parnaamlang, werkzame_stoffen_case) %>% 
  distinct() %>% 
  left_join(aangetroffen) %>% 
  openxlsx::write.xlsx("C:/R/kladblok/data/vertaaltabel_ctgb.xlsx")

toelatingen_opgeschoond %>% select(werkzame_stoffen_case) %>% distinct() %>% openxlsx::write.xlsx("data/werkzame_stoffen_ctgb.xlsx")
  


# werkt niet goed genoeg
parameters %>%
  filter(parnr > 999, parnr < 2000) %>%
  left_join(test, by = "casnr") %>%
  filter(is.na(middelnaam)) %>% View()



