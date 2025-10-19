library(tabr)

x <- as_music("r8 c d e f g a b c'1", lyrics = as_lyrics(". Dit zijn de ly- rics A B C"), tempo = NA)
x |> render_music(file = "images/test.png", keep_ly = TRUE)


phrase(x) |> track()

muziek1 <- "e4 f# g a f#2 e2"
regel1 <- as_lyrics("Bron van za- lig- he- den,")

phrase(muziek1) |> 
  track(lyrics = regel1, tab = FALSE) |> 
  score() |> 
  render_score("images/ex02.pdf")

# as_music(muziek1, lyrics = regel1) |> 
#   render_music(file = "images/test2.png")
# 

# notes <- pc("c b, c d e e d c b, c c c", "c' b c' d' e' e' d' c' b c' c' c''")
# info <- pn("8( 8)( 4) 8( 8) 8( 8) 8 8( 8) 4.- 4", 2)
# strings <- "5*12 3*12"
# phrase(notes, info, strings) |>  as_music() |> render_music(file = "images/test3.png")
# 
# phrase("c d e f g a b c' b c'", "16 16 8 8 4 4 8 2 2 1") |> 
#   track() |> score() |> tab("images/ex01.pdf")


# as_music(muziek1, lyrics = regel1) |> 

