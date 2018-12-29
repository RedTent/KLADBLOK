library(stringr)

x <- c("test", "TEST", "stest", "stet", "tets", "tet", "te5t", "test test", "tteesstt", "ttteeesssttt")

# . matches any character

str_view(x, "s.")
str_view_all(x, "s.")

# anchors
# ^ match the start of a string
# $ matches the end of a string

str_view(x, "^s")
str_view(x, "s$")
str_view(x, "^test$")

# \b boundary between words

# \d matches any digit
# \s matches any whitespace
# [abc] matches a, b or c
# [^abc] matches anyting except a, b or c

str_view(x, "[s]")
str_view(x, "[^s]") # note this!
str_view(x, "[ste]") # letter order doesnt matter
str_view(x, "\\d")
str_view(x, "\\s")

# ? 0 or 1 character
# + 1 or more characters
# * 0 or more characters

str_view(x, "q?te")
str_view(x, "q+te")
str_view(x, "q*te")
str_view(x, "ee+")
str_view(x, "ee+e")
str_view(x, "ee*s")

# {n} exactly n times
# {n,} n or more
# {,m} at most m
# {n,m} between n and m

str_view(x, "e{2,4}")
str_view(x, "e{2}")
str_view(x, "e{2}")
str_view(x, "e{2,2}")
str_view(x, "e{,3}")
str_view(x, "e{2,4}?")
