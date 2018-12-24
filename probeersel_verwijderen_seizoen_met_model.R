library(tidyverse)
library(modelr)
library(JT)

data_sel <- filter(data,  parnr == 13, datum > as.Date("2000-01-01")) %>% 
  group_by(mp) %>% 
  mutate(n = n()) %>% 
  filter(n > 12) %>% 
  ungroup()


data_sel <- data_sel %>% add_maand() 

kp_mod <- lm(waarde ~ factor(maand), data = data_sel)

data_sel <- data_sel %>% add_predictions(kp_mod) %>% add_residuals(kp_mod)

data_sel %>% ggplot(aes(datum, pred)) + geom_line()
data_sel %>% ggplot(aes(datum, resid)) + geom_line() + geom_smooth(se = FALSE) + geom_abline(slope = 0, intercept = 0, colour = "purple")
data_sel %>% ggplot(aes(datum, waarde)) + geom_line() + geom_smooth(se = FALSE) 

data_sel %>% summary

per_meetpunt <- data_sel %>% group_by(mp) %>% nest()

per_meetpunt_model <- function(df){
  lm((waarde) ~ factor(maand), data = df)
}

detrended <- per_meetpunt %>% mutate(
  model = map(data, per_meetpunt_model),
  resids = map2(data, model, add_residuals)
) %>% unnest(resids)

ggplot(data_sel, aes(datum, waarde)) + 
  geom_line(aes(group = mp), alpha = 1/10) + 
  geom_smooth(se = FALSE) #+ scale_y_log10()

ggplot(detrended, aes(datum, resid)) + 
  geom_line(aes(group = mp), alpha = 1/10) + 
  geom_smooth(se = FALSE)
