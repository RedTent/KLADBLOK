library(tidyverse)

test <- function(vec, fun) {
  res <- vector(length = length(vec))
  for (i in seq_along(vec)) {
    if (i == 1) { 
      res[1] <- vec[1]
      next()}
  
    res[i] <- vec[i] + fun(res[i-1])
  }
  res
}

decay <- function(x){
  x * runif(1, min = 0.95, max = 1)
  
}


lozingen <- function(n, hoogte = 60){
  
  index <- rpois(1, n) %>% 
    runif(min = 1, max =365) %>% 
    as.integer()
  
  out <- rep(0, 365)
  out[index] <- runif(n = length(index), min = 0.25 * hoogte, max = 2 * hoogte)
  
  out
}
  
  as.integer(runif(rpois(1, 4), min = 1, max = 365))


x <- test(lozingen(4), fun = decay)

df <- tibble(id = 1:100)
df %>% mutate(res = map_dbl(id, ~mean(test(lozingen(4), fun = decay)))) %>% ggplot(aes(res)) + geom_histogram()

df %>% mutate(res = map(id, ~test(lozingen(4), fun = decay))) %>% 
  mutate(gem_real = map_dbl(res, mean),
         gem_mon = map_dbl(res, ~mean(.x[seq(60,360, by = 60)])),
         verschil = gem_real - gem_mon) %>% 
  ggplot(aes(gem_mon)) + geom_histogram()

{my_data <- tibble(dag = 1:365, x = test(lozingen(4), fun = decay))
my_data %>% ggplot(aes(dag, x)) + geom_line() + geom_point(data = filter(my_data, dag %in% mon_dag), colour = "red", size = 3)}
              
