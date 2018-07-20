library(ggplot2)
library(packcircles)
library(dplyr)
library(ggrepel)

StatPackCircles <- ggproto("StatPackCircles", Stat,
                           
                           compute_layer = function(data, scales, ...){
                             data$id <- row(data)[,1]
                             circles <- packcircles::circleLayoutVertices(packcircles::circleProgressiveLayout(data$y), 
                                                                          npoints = 100)
                             data$y <- NULL
                             data$group <- NULL
                             data <- merge(circles, data, by = "id", sort = FALSE)
                             names(data)[names(data) == "id"] <- "group"
                             data
                           },
                           
                           required_aes = c("y")
)

stat_packcircles <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  
  ggplot2::layer(
    stat = StatPackCircles, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

randomize <- function(data){data[sample(nrow(data)),]}


StatPackcirclesLabel <- ggproto("StatPackcirclesLabel", Stat,
                                compute_layer = function(data, scales, ...){
                                  #print(data)
                                  #data$id <- row(data)[,1]
                                  circle_centers <- packcircles::circleProgressiveLayout(data$y)
                                  data$y <- NULL
                                  data <- cbind(circle_centers,data)
                                  #data$group <- NULL
                                  #data <- merge(circles, data, by = "id", sort = FALSE)
                                  #names(data)[names(data) == "id"] <- "group"
                                  print(data)
                                  data
                                  
                                }
                                
                                
                                                                )

stat_packcircles_label <- function(mapping = NULL, data = NULL, geom = "text",
                             position = "identity", na.rm = FALSE, show.legend = NA, 
                             inherit.aes = TRUE, ...) {
  
  ggplot2::layer(
    stat = StatPackcirclesLabel, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
