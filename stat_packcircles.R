library(ggplot2)

compute_circle_groups <- function(data, scales, ...){
  require(packcircles)
  
  data$id <- row(data)[,1]
  positions <- circleProgressiveLayout(data$y)
  circles <- circleLayoutVertices(positions, npoints = 100)
  #colnames(circles)[[3]] <- "group"
  #print(data)
  #print(circles)
  data$y <- NULL
  data$group <- NULL
  data <- merge(circles, data, by = "id", sort = FALSE)
  #print(data)
  names(data)[names(data) == "id"] <- "group"
  data
}


StatPackCircles <- ggproto("StatPackCircles", Stat,
                           
                           compute_layer = compute_circle_groups,
                           required_aes = c("y", "group")
)

stat_packcircles <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  
  layer(
    stat = StatPackCircles, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
