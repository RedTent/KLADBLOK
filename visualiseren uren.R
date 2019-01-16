library(tidyverse)
library(viridisLite)

tabel <- tribble(
  ~persoon, ~activiteit, ~uren,
  "a", "x1", 200,
  "a", "x2", 20,
  "a", "x3", 100,
  "a", "x4", 50,
  "a", "x5", 300,
  "b", "x1", 20,
  "b", "x2", 200,
  "b", "x3", 100,
  "b", "x4", 100,
  "b", "x5", 100,
  "b", "x6", 50,
  "c", "x1", 20,
  "c", "x2", 200,
  "c", "x3", 200,
  "c", "x4", 60,
  "d", "x1", 20,
  "d", "x2", 20,
  "d", "x3", 100,
  "d", "x4", 20,
  "d", "x5", 50,
  "d", "x6", 300

  
)

tabel %>% ggplot() + geom_tile(aes(persoon, activiteit, fill = uren)) + scale_fill_viridis_c()

pca <- tabel %>% spread(persoon, uren, fill = 0) %>% select(-activiteit) %>% prcomp()
biplot(pca)
pca <- tabel %>% spread(activiteit, uren, fill = 0) %>% select(-persoon) %>% prcomp()
biplot(pca)

tabel2 <- tabel %>% spread(activiteit, uren, fill = 0) 
tabel3 <- `row.names<-`(tabel2, tabel2$persoon)
tabel3 %>% select(-persoon) %>% as.matrix() %>% heatmap()

urenoverzicht <- read_excel("urenoverzicht.xlsx", sheet = "Blad2")
urenoverzicht  %>%  `row.names<-`(.$naam) %>% select(-naam) %>% as.matrix() %>% t() %>% heatmap(scale = "none", col = viridisLite::magma(10))
urenoverzicht  %>%  `row.names<-`(.$naam) %>% select(-naam) %>% as.matrix() %>% t() %>% heatmap(scale = "none", col = viridisLite::inferno(10))

pca_uren <- urenoverzicht %>% `row.names<-`(.$naam) %>% select(-naam) %>% prcomp()
summary(pca_uren)
biplot(pca_uren)
plot(pca_uren$x[,1],pca_uren$x[,2])
text(pca_uren$x[,1],pca_uren$x[,2], labels = rownames(pca_uren$x))

plot(pca_uren$x[,1],pca_uren$x[,2])
text(pca_uren$x[,1],pca_uren$x[,2], labels = rownames(pca_uren$x))


plot(pca_uren$rotation[,1],pca_uren$rotation[,2])
text(pca_uren$rotation[,1],pca_uren$rotation[,2], labels = rownames(pca_uren$rotation))


plot(pca_uren$x[,2],pca_uren$x[,3])
text(pca_uren$x[,2],pca_uren$x[,3], labels = rownames(pca_uren$x))

# als je de clustering en shuffle toepast voor plotten krijg je een vergelijkbaar resultaat als heatmap
urenoverzicht %>% 
  gather(activiteit, uren, -naam) %>% 
  ggplot(aes(naam, fct_inorder(activiteit, ordered = TRUE), fill = uren, col)) + 
  geom_tile() + 
  scale_fill_viridis_c(option = "B")

uren_preclus <- urenoverzicht  %>%  `row.names<-`(.$naam) %>% select(-naam) %>% as.matrix()
clusters_n <- hclust(dist(uren_preclus)) 

clusters_act <- hclust(dist(t(uren_preclus))) 
clusters_act %>% plot()

volgorde_n <- urenoverzicht$naam[clusters_n$order]
volgorde_act <- colnames(urenoverzicht[,-1])[clusters_act$order]


urenoverzicht %>% gather(activiteit, uren, -naam) %>% 
  mutate(naam = fct_relevel(naam, volgorde_n), activiteit = fct_relevel(activiteit, volgorde_act)) %>% 
  ggplot(aes(naam, activiteit, fill = uren, col)) + 
  geom_tile() + 
  scale_fill_viridis_c(option = "B") + 
  scale_fill_gradientn(colours = c("black", "navy", "blue", "lightgreen"))


uren_preclus[clusters_n$order,clusters_act$order] 
