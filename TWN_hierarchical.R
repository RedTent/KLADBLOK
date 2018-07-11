library(dplyr)
library(readr)
library(openxlsx)

#https://www.verspreidingsatlas.nl/soortenlijst/vaatplanten

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

volgorde_taxonlevels <- read_delim("volgorde_taxonlevels.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
                        rename(volgorde = Volgorde) %>% 
                        mutate(taxonlevel_factor = factor(taxonlevel, levels = taxonlevel, labels = taxonlevel, ordered = TRUE))

#basic_levels <- c("Species", "Genus", "Familia", "Ordo", "Classis", "Phylum", "Regnum", "Imperium")

# twee loops omdat sommige een dubbele doorverwijzing hebben
twn_preferred <- twn %>% mutate(taxonname_preferred = if_else(is.na(refername),taxonname, refername)) %>% 
                rename(taxonname_orig = taxonname) %>% 
                select(taxonname_preferred, taxontype, taxonname_orig) %>%  
                left_join(twn, by = c("taxonname_preferred" = "taxonname", "taxontype" = "taxontype")) %>% 
                mutate(taxonname_preferred = if_else(is.na(refername), taxonname_preferred, refername)) %>% 
                select(taxonname_preferred, taxontype, taxonname_orig) %>%  
                left_join(twn, by = c("taxonname_preferred" = "taxonname", "taxontype" = "taxontype")) %>% 
                select(taxontype, taxonname_orig, taxonname_preferred, localname, taxongroup, taxonlevel, parentname, date, status, author, literature)

twn_taxon_levels <- twn_preferred %>% select(taxontype, taxonname_preferred, taxonlevel, status) %>% distinct()


twn_parents <-  twn_preferred %>% 
                select(taxontype, taxonname_preferred, taxonlevel, parentname, status) %>% 
                left_join(twn_taxon_levels, by = c("parentname" = "taxonname_preferred", "taxontype" = "taxontype")) %>% 
                rename(parentlevel = taxonlevel.y, taxonlevel = taxonlevel.x, status = status.x, parentstatus = status.y) %>% 
                distinct()


# create complete tree ----------------------------------------------------

  
taxonlevels <- volgorde_taxonlevels[["taxonlevel"]]

all_parents_basis <- twn_preferred %>% 
  select(taxontype, taxonname_orig, taxonname_preferred, taxonlevel) %>% 
  mutate(taxonname_actueel = taxonname_preferred, taxonlevel_actueel = taxonlevel)

for (level in taxonlevels) {
  #print(level)
  add_level <- twn_parents %>% filter(parentlevel == level) 
  join_pairs <- c("taxonlevel_actueel" = "taxonlevel", "taxonname_actueel" = "taxonname_preferred", "taxontype" = "taxontype") 
  all_parents_basis <- all_parents_basis %>% 
    left_join(add_level, by = join_pairs) %>% 
    mutate(!!level := if_else(taxonlevel_actueel == level, taxonname_actueel, parentname)) %>% 
    
    mutate(taxonname_actueel = if_else(is.na(.[[level]]),taxonname_actueel, .[[level]])) %>% 
    select(-parentname, -status, -parentstatus, -parentlevel, -taxonlevel_actueel) %>% 
    left_join(select(twn_taxon_levels,-status), by = c("taxontype" = "taxontype", "taxonname_actueel" = "taxonname_preferred")) %>% 
    rename(taxonlevel = taxonlevel.x, taxonlevel_actueel = taxonlevel.y)
  
  
}

all_parents <- all_parents_basis %>% select(-taxonname_actueel, -taxonlevel_actueel, -taxonlevel)
total <- twn_preferred %>% left_join(all_parents, c("taxontype" = "taxontype", "taxonname_orig" = "taxonname_orig", "taxonname_preferred" = "taxonname_preferred"))

# TWN onvolkomenheden -----------------------------------------------------

dubbele_doorverwijzing <- twn %>% mutate(taxonname_preferred = if_else(is.na(refername),taxonname, refername)) %>% 
  select(taxonname_preferred, taxontype, taxonname) %>%  
  left_join(twn, by = c("taxonname_preferred" = "taxonname", "taxontype" = "taxontype")) %>% 
  filter(status == "20")  

missende_verwijzing <- twn %>% filter(is.na(refername), status == "20") 

missende_parents <- twn %>%  filter(is.na(parentname)) #%>% group_by(taxonlevel) %>% summarise(n=n()) %>% select(n) %>% sum()

missende_parents_preferred <- twn_preferred %>% filter(is.na(parentname)) #%>% group_by(status,taxonlevel) %>% summarise(n=n()) %>% arrange(desc(n))%>% as.data.frame() 

onjuiste_parents <- twn_parents %>% filter(parentlevel == "Varietas")
onjuiste_parents2 <- twn_parents %>% filter(parentlevel == taxonlevel)

parents_met_afwijkende_Status <- twn_parents %>% filter(status == "10", parentstatus != "10")

onjuist_taxonlevel <- twn %>% filter(taxonname == "Staurodesmus phimus") #wordt als phylum aangeduid ipv species
onjuist_taxonlevel <- twn %>% filter(taxonname == "Syringa") #is genus ipv species

fout_in_twn <- twn %>% filter(taxonname == "Eustigmatales incertae sedis") #parent kan niet van het zelfde niveau zijn, Eustigmatales moet voorkeursnaam zijn

wisselende_parent_levels <- twn_parents %>% group_by(taxonlevel,parentlevel, status) %>% summarize(n = n())
