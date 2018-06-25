library(bubbles)
library(dplyr)

library(ggplot2)

values <- runif(40)^4 

bubbles(value = 1:26, label = letters, tooltip = LETTERS,
        color = rainbow(26, alpha=NULL)[sample(26)]
)

bubbles(value = values, label = round(values, 2), tooltip = "values",
        color = terrain.colors(length(values), alpha = NULL)[sample(length(values))]
)

test <- ggplot2::diamonds
test2 <- test %>% group_by(cut) %>% summarize(n = n())

bubbles(value = test2$n, label = test2$cut, 
        color = terrain.colors(nrow(test2), alpha = NULL)[sample(nrow(test2))])


#https://github.com/jcheng5/bubbles