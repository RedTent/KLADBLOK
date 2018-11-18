#devtools::install_github("Redtent/JT")
library(JT)
library(dplyr)
library(zoo)
library(ggplot2)

data_ruw <- knmi_dag_ruw()
temp <- knmi_temperatuur_dag()
#temp %>% head(10) %>% select()
temp %>% mutate(yest_gem_temp = lag(gem_temp))
temp %>% mutate(gem_temp10 = my_roll_mean(vector = gem_temp)) %>% ggplot(aes(x = datum, y = gem_temp10)) + geom_line()

my_roll_mean <- function(vector, days = 10, lag_days = 0){
  zoo::rollapply(dplyr::lag(vector, lag_days), FUN = mean, na.rm = TRUE, fill = NA, width = days, align = "right")
}

library(purrr)
my_mean <- purrr::partial(mean, na.rm = TRUE)
