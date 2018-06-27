library(readr)
library(stringr)
library(dplyr)
library(wordcloud2)

stopwoorden <- c("de", "in", "het", "en", "van", "een", "te", "op", "voor", "aan", ">", "dat", "dit", "niet", 
                 "als", "die", "naar", "er", "wat", "hun", "of", "zo", "zodat", "tot", "door", "�", "bij" , "is",
                 "met", "om", "ad", "uit", "ook", "deze", "a", "over", "bijlagen")

ruwe_tekst <- read_delim("wbp.txt", delim =",") %>% rename(word = `1`) %>% mutate(word = str_to_lower(word)) %>% 
  mutate(word = str_remove_all(word, "[:digit:]"),
         word = str_remove_all(word, "[:blank:]"),
         word = str_remove_all(word, "[:punct:]")
         ) %>% 
  filter(word != "") %>% 
  filter(!word %in% stopwoorden) %>% 
  print()

ruwe_tekst %>% group_by(word) %>% summarize(freq = n()) %>% arrange(desc(freq)) %>% letterCloud(word = "HHSK")
ruwe_tekst %>% group_by(word) %>% summarize(freq = n()) %>% arrange(desc(freq)) %>% .[30:40,]
                                                                    