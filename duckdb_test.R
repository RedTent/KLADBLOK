library(HHSKwkl)
library(tidyverse)
library(arrow)
library(duckdb)

db <- duckdb(dbdir = "C:/R/KLADBLOK/data/duck_test/duckdb")
# db <- duckdb()

# 
fys_chem <- readRDS("data/fys_chem.rds")
con <- DBI::dbConnect(db)
duckdb::dbWriteTable(con, "fys_chem", fys_chem)

fys_chem_csv <- open_delim_dataset("data/fys_chem.csv", delim = ";", convert_options = csv_convert_options(decimal_point = ","))
#

fys_chem_csv %>% 
  arrow::to_duckdb(con = con, table_name = "fys_chem_csv")


tabel <- function(tabelnaam){
  dplyr::tbl(con, tabelnaam) 
  
}

fc <- tabel("fys_chem")
fc %>% filter(parnr == 1, mp == "S_0067", year(datum) == 2024)


DBI::dbDisconnect(con)  
