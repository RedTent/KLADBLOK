#install.packages("packcircles")
#install.packages("ggrepel")
library(packcircles)
library(ggplot2)
library(ggrepel)
library(dplyr)

areas <- c(20, 10, 40, 5, 5, 5, 5, 5, 5, 5) # een kwanitatieve variabele

# Generate the layout 
packing <- circleProgressiveLayout(areas) 


round(packing, 2)


dat.gg <- circleLayoutVertices(packing, npoints=100)

ggplot(data = dat.gg) + geom_polygon(aes(x, y, group = id), colour = "black", 
                                     fill = "grey90", alpha = 0.7, show.legend = FALSE) +
  
  geom_text(data = packing, aes(x, y), label = 1:nrow(packing)) +
  
  coord_equal()

# mijntest

data <- mtcars %>% mutate(naam = rownames(.)) %>% select(naam, mpg, hp) #%>% arrange(desc(mpg))
data$id <- c(1:nrow(data))
data <- data[sample(nrow(data)),]

data <- cbind(circleProgressiveLayout(data$mpg),data) %>% rename(x_center = x, y_center = y)
polys <- circleProgressiveLayout(data$mpg) %>% circleLayoutVertices(npoints = 100, xysizecols = c(1,2,3))
data2 <- left_join(data, polys, by = "id")

ggplot(data2, aes(x = x, y = y, fill = hp)) + geom_polygon(aes(x, y, group = id), data = data2, 
                        colour = "black", alpha = 0.7) +
geom_text_repel(data = data, aes(x_center, y_center, label = naam)) +
  
  coord_equal() + theme_void()

# mijntest

data <- mtcars %>% mutate(naam = rownames(.)) %>% select(naam, mpg, cyl) #%>% arrange(desc(mpg))
data$id <- c(1:nrow(data))
data <- data[sample(nrow(data)),]

data <- cbind(circleProgressiveLayout(data$mpg),data) %>% rename(x_center = x, y_center = y)
polys <- circleProgressiveLayout(data$mpg) %>% circleLayoutVertices(npoints = 100, xysizecols = c(1,2,3))
data2 <- left_join(data, polys, by = "id")

ggplot(data2, aes(x = x, y = y, fill = factor(cyl))) + geom_polygon(aes(x, y, group = id), data = data2, 
                                                           colour = "black", alpha = 0.7) +
  geom_text_repel(data = data, aes(x_center, y_center, label = naam)) +
  
  coord_equal() + theme_void()


my_circles <- function(data, labels, values){
  require(dplyr)
  require(ggplot2)
  require(packcircles)
  
  data$id <- c(1:nrow(data))
  data <- data[sample(nrow(data)),]
  circle_layout <- cbind(circleProgressiveLayout(data[[values]]),data) %>% rename(x_center = x, y_center = y)
  circle_polys <- circle_layout %>% circleLayoutVertices(npoints = 100, xysizecols = c(1,2,3))
  label_text <- data[[labels]]
  
  plot_data <- left_join(data, circle_polys, by = "id")
  #print(plot_data)
  
  ggplot(plot_data, aes(x = x, y = y, fill = factor(id))) + 
    geom_polygon(aes(x, y, group = id), data = plot_data, colour = "black", alpha = 0.7, show.legend = FALSE) +
    geom_text_repel(data = circle_layout, aes(x_center, y_center), label = label_text) +
    coord_equal() + 
    theme_void()
  
}
