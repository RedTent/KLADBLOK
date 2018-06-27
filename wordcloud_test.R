#install.packages("wordcloud2")
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
library(dplyr)
?wordcloud2()

test <- as_data_frame(sample(month.name, 100, replace = TRUE)) %>% group_by(value) %>%  summarise(freq = n()) %>% rename(word = value)

test <- as.data.frame(cbind(state.name,state.area)) %>% rename(word = state.name, freq = state.area) %>% mutate(word = as.character(word), freq = as.integer(freq))

wordcloud2(test)

wordcloud2(test, shape = "star")

#wordcloud2(demoFreq, shape = "star")

#letterCloud(demoFreq, word = "H", wordSize = 2)

wordcloud2(test, figPath = "logo_website.jpg")

wordcloud2(test, figPath = "sman.jpeg")

letterCloud(test, "H", size = 2, color = "blue")

figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(test, figPath = figPath, size = 1.5,color = "skyblue")

test <- as.data.frame(cbind(state.name,state.area))