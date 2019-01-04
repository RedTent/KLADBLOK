library(tidyverse)
library(modelr)
library(magrittr)
library(lubridate)

test <- data %>% filter(mp == "00005", parnr == 1) %>% arrange(mp, parnr, datum)


test_res <- trend::mk.test(test$waarde)

# nested_data <- data %>% 
#   filter(parnr < 100) %>%  
#   arrange(mp, parnr, datum) %>% 
#   group_by(mp, parnr, par, eenheid) %>% 
#   mutate(n = n()) %>% 
#   filter(n > 12) %>% 
#   nest()
# 
# 
# results <- nested_data %>% mutate(mk_result = map(data, ~trend::mk.test(.$waarde)), 
#                        p_waarde = map(mk_result, "p.value"), 
#                        z_stat = map(mk_result, "statistic")) %>% 
#   unnest(p_waarde, z_stat, .drop = TRUE) 



# test %>% 
#   mutate(maand = month(datum)) %>% 
#   group_by(mp, parnr, par, eenheid, maand) %>% 
#   summarise(S = trend::mk.test(waarde)[["estimates"]][["S"]],
#             varS = trend::mk.test(waarde)[["estimates"]][["varS"]])  
  
trendestimates <- function(waarde_vector){
  if (length(waarde_vector) < 3) {return(list(S = NA, varS = NA, tau = NA))}
  list(enframe(trend::mk.test(waarde_vector)[["estimates"]]))
}

probmk <- function(s, vars){ 
  z <- ifelse(s == 0, 0, s - sign(s)) / sqrt(vars)
  probtrend = min(pnorm(z), 1 - pnorm(z))
}

test %>% 
  mutate(maand = month(datum)) %>% 
  group_by(mp, parnr, par, eenheid, maand) %>% 
  summarise(maand_mk = trendestimates(waarde)) %>% 
  unnest() %>% 
  spread(name, value) %>% 
  group_by(mp, parnr, par, eenheid) %>% 
  summarise(totS = sum(S, na.rm = TRUE),
            totvarS = sum(varS, na.rm = TRUE)) %>% 
  mutate(richting = sign(totS),
         p_waarde = probmk(totS, totvarS))

bepaal_trend_seasonal <- function(df){
  df %>% 
    mutate(maand = month(datum)) %>% 
    group_by(mp, parnr, par, eenheid, maand) %>% 
    filter(n() > 2) %>% 
    summarise(maand_mk = trendestimates(waarde)) %>% 
    unnest() %>% 
    spread(name, value) %>% 
    group_by(mp, parnr, par, eenheid) %>% 
    summarise(totS = sum(S, na.rm = TRUE),
              totvarS = sum(varS, na.rm = TRUE)) %>% 
    mutate(richting = sign(totS),
           p_waarde = probmk(totS, totvarS))
  
}

trendresultaten <- bepaal_trend_seasonal(filter(data, parnr < 100))

trend_summary <- trendresultaten %>% 
  mutate(gebied = case_when(
    str_detect(mp, "^0") ~ "Schieland", 
    str_detect(mp, "^KOP") ~ "Krimpenerwaard", 
    TRUE ~ "Overig"
  )) %>% 
  group_by(gebied, parnr, par) %>% 
  summarise(n_stijgend = mean(richting == 1 & p_waarde < 0.05),
            n_dalend = mean(richting == -1 & p_waarde < 0.05),
            n_geentrend = mean(p_waarde >= 0.05)) %>% ungroup()

library(JT)

trend_summary %>% 
  filter(gebied != "Overig", parnr < 16) %>% 
  mutate(par = factor(par, levels = par[1:15])) %>% 
  ggplot(aes(x = par)) + 
  geom_col(aes(y = n_dalend), position = "dodge", fill = hhskblauw) + 
  facet_wrap(~gebied, ncol = 1) + 
  JT::hhskthema() + ggtitle("maanden")


x <- cut(c(1:12), breaks = c(0,3.5, 6.5, 9.5, 13), labels = c("Q1", "Q2", "Q3", "Q4"))

func_seizoen <- function(maandvector){
  cut(maandvector, breaks = c(0,3.5, 6.5, 9.5, 13), labels = c("Q1", "Q2", "Q3", "Q4"))
}
  
trend_res2 <- data %>% 
  filter(parnr < 16) %>% 
  mutate(maand = as.character(func_seizoen(month(datum))), jaar = year(datum)) %>% 
  group_by(mp, parnr, par, eenheid, jaar, maand) %>% summarise(waarde = mean(waarde, na.rm = TRUE)) %>% 
  group_by(mp, parnr, par, eenheid, maand) %>% 
  filter(n() > 2) %>% 
  summarise(maand_mk = trendestimates(waarde)) %>% 
  unnest() %>% 
  spread(name, value) %>% 
  group_by(mp, parnr, par, eenheid) %>% 
  summarise(totS = sum(S, na.rm = TRUE),
            totvarS = sum(varS, na.rm = TRUE)) %>% 
  mutate(richting = sign(totS),
         p_waarde = probmk(totS, totvarS))
                 

trend_sum2 <- trend_res2 %>% 
  filter(parnr < 16) %>% 
  mutate(gebied = case_when(
    str_detect(mp, "^0") ~ "Schieland", 
    str_detect(mp, "^KOP") ~ "Krimpenerwaard", 
    TRUE ~ "Overig"
  )) %>% 
  group_by(gebied, parnr, par) %>% 
  summarise(n_stijgend = mean(richting == 1 & p_waarde < 0.05),
            n_dalend = mean(richting == -1 & p_waarde < 0.05),
            n_geentrend = mean(p_waarde >= 0.05)) %>% ungroup()                 


trend_sum2 %>% 
    filter(gebied != "Overig", parnr < 16) %>% 
    mutate(par = factor(par, levels = par[1:15])) %>% 
    ggplot(aes(x = par)) + 
    geom_col(aes(y = n_dalend), position = "dodge", fill = hhskblauw) + 
    facet_wrap(~gebied, ncol = 1) + 
    JT::hhskthema() + ggtitle("kwartalen")   
