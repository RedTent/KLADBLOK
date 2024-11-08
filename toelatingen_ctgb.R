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

vertaaltabel <- readxl::read_excel("data/gbm_vertaaltabel_ctgb.xlsx")


toelatingen_opgeschoond <- 
  toelatingen %>% 
  mutate(werkzame_stoffen_middel = werkzame_stoffen,
         aard_werking_middel = aard_werking) %>% 
  tidyr::separate_longer_delim(cols = c(werkzame_stoffen, aard_werking), delim = " # ") %>% 
  mutate(werkzame_stof_ctgb = str_remove(werkzame_stoffen, " .*$"),
         werkzame_stof_ctgb = str_remove(werkzame_stof_ctgb, ",$")) %>% 
  select(-werkzame_stoffen) %>% 
  left_join(vertaaltabel, by = "werkzame_stof_ctgb")






