library(tidyverse)
library(gganimate)
library(lubridate)


library(gapminder)

test <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

test_data <- data %>% filter(mp == "00005", parnr == 1, datum > ymd(20160101))
test_data <- data %>% filter(mp == "00040", parnr == 7)

test_data %>% JT::add_jaar_maand() %>% ggplot(aes(datum, waarde, colour = waarde)) + geom_point(size = 2) + transition_time(datum) + 
  labs(title = 'Datum: {frame_time}') + ease_aes('cubic-in-out') + shadow_mark()

test_data %>% ggplot(aes(datum,waarde)) + geom_line()

actie <- test_data %>% JT::add_jaar_maand() %>% 
  ggplot(aes(datum, waarde)) + 
  geom_point(size = 3) + 
  geom_line() +
  gganimate::transition_reveal(datum) +
  #labs(title = 'Datum: {frame_time}') + 
  #shadow_wake(1, size = NULL, alpha = 0.5, exclude_phase = NULL, wrap = FALSE) +
  scale_color_viridis_c()
  

animate(actie, rewind = TRUE)  
animate(actie, end_pause = 10, duration = 20, fps = 20) 

