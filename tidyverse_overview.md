#Overzicht van tidyverse functies nav R4DS

`dplyr::count(kolom_a)`

## Parsing 
https://r4ds.had.co.nz/data-import.html

`readr::parse_*`

`challenge2 <- read_csv(file.csv, col_types = cols(.default = col_character()) )`

## Tidying 
https://r4ds.had.co.nz/tidy-data.html#separating-and-uniting

`separate()` and `unite()` om kolommen te splitsen of samen te voegen
`complete()` om missende waarden toe te voegen


## Strings
https://r4ds.had.co.nz/strings.html#combining-strings 

`stringr::str_c("prefix-", c("a", "b", "c"), "-suffix")` 

`str_to_title`

## Factors
https://r4ds.had.co.nz/factors.html#modifying-factor-order

`forcats::fct_reorder()`

You can use `fct_relevel()`. It takes a factor, f, and then any number of levels that you want to move to the front of the line.

https://r4ds.had.co.nz/factors.html#modifying-factor-levels 

`fct_recode()` - om de namen van levels te veranderen
`fct_collapse()` - om de namen van levels te veranderen en groeperen
`fct_lump` - om levels met kleinere counts samen te voegen

## Dates and Date-times 
https://r4ds.had.co.nz/dates-and-times.html

Package lubridate
`today()` and `now()`

`ymd()` and `ymd_hms()` - willekeurige volgorde van onderdelen is mogelijk

`year()` `month()` `yday()` `wday()` `mday()`
`floor_date()` `round_date()`

Durations (seconds) - periods (human times) - time-spans (specific human times)

## Pipes

`%T>%` returns LHS
`%$% ` expose columns (load magrittr for this explicitly)

## Functions
https://r4ds.had.co.nz/functions.html#multiple-conditions 

`switch()` - run code based on input

`invisible()` - to return an object form a function invisible. Useful inside a pipe.

## Iteration

`map()` makes a list.
`map_lgl()` makes a logical vector.
`map_int()` makes an integer vector.
`map_dbl()` makes a double vector.
`map_chr()` makes a character vector.


https://r4ds.had.co.nz/iteration.html#shortcuts
`map(~lm(mpg ~ wt, data = .))` - use ~ for an anonymous function
`map_dbl("r.squared")` - extract values by name
`map_dbl(2)` - extract values by index

`safely()`
`possibly()`
`quietly()` - for safe execution and error capturing

`purrr::transpose()`

https://r4ds.had.co.nz/iteration.html#mapping-over-multiple-arguments 
`map2_*` -- two inputs
`pmap_*` - multiple inputs

