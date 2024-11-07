library(HHSKwkl)
library(tidyverse)
library(glue)
library(arrow)

theme_set(hhskthema())

download_data("fys_chem.rds")
download_data("fys_chem.csv")

fys_chem <- readRDS("data/fys_chem.rds")


fys_chem_csv <- open_delim_dataset("data/fys_chem.csv", delim = ";", convert_options = csv_convert_options(decimal_point = ","))
fys_chem_csv %>% collect()
fys_chem %>% 
  write_dataset("data/fys_chem", format = "parquet")

fys_chem_pq <- open_dataset("data/fys_chem")

microbenchmark::microbenchmark(readRDS("data/fys_chem.rds"), fys_chem_pq %>% collect(), fys_chem_csv %>% collect(), times = 10)
fys_chem <- open_delim_dataset("data/fys_chem.csv", delim = ";", convert_options = csv_convert_options(decimal_point = ",")) %>% collect()


  filter(fys_chem_csv, parnr == 1, mp == "S_0016") %>% 
  collect() %>%
  ggplot(aes(datum ,waarde )) + geom_line()
