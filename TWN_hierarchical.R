library(dplyr)

get_twn <- function(){
require(readxl)
  
url <- "http://sofus.ecosys.nl/Taxus/Downloads/Taxalists/TWNList.XLS"
destfile <- "twn.xls"
curl::curl_download(url, destfile)
twn <- suppressMessages(read_excel(destfile, col_types = c("text", "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "date", "text")))
twn
}

twn <- get_twn()

# twee loops omdat sommige een dubbele doorverwijzing hebben
twn_preferred <- twn %>% mutate(taxonname_preferred = if_else(is.na(refername),taxonname, refername)) %>% 
    rename(taxonname_orig = taxonname) %>% 
    select(taxonname_preferred, taxontype, taxonname_orig) %>%  
    left_join(twn, by = c("taxonname_preferred" = "taxonname", "taxontype" = "taxontype")) %>% 
    mutate(taxonname_preferred = if_else(is.na(refername), taxonname_preferred, refername)) %>% 
    select(taxonname_preferred, taxontype, taxonname_orig) %>%  
    left_join(twn, by = c("taxonname_preferred" = "taxonname", "taxontype" = "taxontype")) %>% 
    select(taxontype, taxonname_orig, taxonname_preferred, localname, taxongroup, taxonlevel, parentname, date, status)
    


twn_levels <- twn_preferred %>% select(taxontype, taxonname_preferred, taxonlevel) %>% distinct()

basic_levels <- c("Species", "Genus", "Familia", "Ordo", "Classis", "Phylum", "Regnum", "Imperium")

twn_parents <- twn_preferred %>% 
  select(taxontype, taxonname_preferred, taxonlevel, parentname, status) %>% 
  left_join(twn_levels, by = c("parentname" = "taxonname_preferred", "taxontype" = "taxontype")) %>% 
  rename(parentlevel = taxonlevel.y, taxonlevel = taxonlevel.x)
  
twn_basic_parents <- twn_preferred %>% 
  select(taxontype, taxonname_preferred, taxonlevel, parentname, status) %>% 
  left_join(twn_levels, by = c("parentname" = "taxonname_preferred", "taxontype" = "taxontype")) %>% 
  rename(parentlevel = taxonlevel.y, taxonlevel = taxonlevel.x)


# TWN onvolkomenheden -----------------------------------------------------

dubbele_doorverwijzing <- twn %>% mutate(taxonname_preferred = if_else(is.na(refername),taxonname, refername)) %>% 
  select(taxonname_preferred, taxontype, taxonname) %>%  
  left_join(twn, by = c("taxonname_preferred" = "taxonname", "taxontype" = "taxontype")) %>% 
  filter(status == "20")  

missende_verwijzing <- twn %>% filter(is.na(refername), status == "20") 

missende_parents <- twn %>%  filter(is.na(parentname)) #%>% group_by(taxonlevel) %>% summarise(n=n()) %>% select(n) %>% sum()

missende_parents_preferred <- twn_preferred %>%  filter(is.na(parentname)) #%>% group_by(status,taxonlevel) %>% summarise(n=n()) %>% arrange(desc(n))%>% as.data.frame() 

onjuiste_parents <- twn_parents %>% filter(parentlevel == "Varietas")
onjuiste_parents2 <- twn_parents %>% filter(parentlevel == taxonlevel)

onjuist_taxonlevel <- twn %>% filter(taxonname == "Staurodesmus phimus") #wordt als phylum aangeduid ipv species
onjuist_taxonlevel <- twn %>% filter(taxonname == "Syringa") #is genus ipv species

fout_in_twn <- twn %>% filter(taxonname == "Eustigmatales incertae sedis") #parent kan niet van het zelfde niveau zijn, Eustigmatales moet voorkeursnaam zijn


wisselende_parent_levels <- twn_parents %>% group_by(taxonlevel,parentlevel, status) %>% summarize(n = n())