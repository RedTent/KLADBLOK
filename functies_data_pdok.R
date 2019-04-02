

get_gemeenten <- function() {
  url <- httr::parse_url("https://geodata.nationaalgeoregister.nl/bestuurlijkegrenzen/wfs?request=GetCapabilities&service=wfs")
  url$query <- list(service = "WFS",
                    version = "2.0.0",
                    request = "GetFeature",
                    typename = "bestuurlijkegrenzen:gemeenten",
                    outputFormat = "application/json",
                    bbox = "87000,432000,120000,457000")
  
  request <- httr::build_url(url)
  gemeenten <- sf::st_read(request)
}

get_woonplaatsen <- function() {
  url_woonplaats <- httr::parse_url("https://geodata.nationaalgeoregister.nl/bag/wfs?request=GetCapabilities&service=wfs")
  url_woonplaats$query <- list(service = "WFS",
                               version = "2.0.0",
                               request = "GetFeature",
                               typename = "bag:woonplaats",
                               outputFormat = "application/json",
                               bbox = "87000,432000,120000,457000")
  
  request <- httr::build_url(url_woonplaats)
  woonplaatsen_hhsk <- sf::st_read(request)
}

get_bodem <- function() {
  url <- httr::parse_url("https://geodata.nationaalgeoregister.nl/bodemkaart50000/wfs?request=getCapabilities&service=wfs")
  url$query <- list(service = "WFS",
                               version = "2.0.0",
                               request = "GetFeature",
                               typename = "bodemkaart50000:bodemkaart50000",
                               outputFormat = "application/json",
                               bbox = "85000,430000,120000,455000")
  
  request <- httr::build_url(url)
  woonplaatsen_hhsk <- sf::st_read(request)
}
# get_bodem()
# 
# 
# test <- meetpunten %>% st_join(get_bodem()) #%>% select(mp, bodem1)
# test %>% as_data_frame() %>% select(c(1,34:40)) %>% count(bodemcode)

# temp <- filter(meetpunten, mp == "00007")
# temp <- filter(meetpunten, mp == "00016")
# temp <- filter(meetpunten, mp %in% c("KOP 0310", "00007")) %>% select(mp, x, y)

get_adress_old <- function(sf_punt){
  
  my_bbox <- st_transform(sf_punt, 28992) %>% st_bbox() #+ c(-100,-100,100,100)} %>% paste(collapse = ",")
  
  for (afstand in c(20, 50, 100, 150, 200, 300, 400, 500, 600, 700)){
    #print(afstand)
    my_bbox_text <- {my_bbox + c(-afstand, -afstand, afstand, afstand)} %>% paste(collapse = ",")
    
    url <- httr::parse_url("https://geodata.nationaalgeoregister.nl/inspireadressen/wfs?request=GetCapabilities&service=wfs")
    url$query <- list(service = "WFS",
                      version = "2.0.0",
                      request = "GetFeature",
                      typename = "inspireadressen:inspireadressen",
                      outputFormat = "application/json",
                      bbox = my_bbox_text)
    request <- httr::build_url(url)
    adressen <- sf::st_read(request, quiet = TRUE)
    if ( nrow(adressen) > 0) break
    
    
  }
  
  adres_afstanden <- sf_punt %>% sf::st_distance(adressen) 
  adres_afstand <- min(adres_afstanden)
  adres_min <- adressen[which.min(adres_afstanden),] 
  adres_chr <- paste0(adres_min$straatnaam, " ", adres_min$huisnummer, adres_min$huisletter, adres_min$toevoeging, " ", adres_min$woonplaats) %>% 
    str_replace_all("NA", "")
  list(adres = adres_chr, postcode = as.character(adres_min$postcode), adres_afstand = adres_afstand )
}

#get_adress(temp)
# 
# temp %>% mutate(adresinfo = list(get_adress(.)), 
#                  adres = map_chr(adresinfo, "adres"), 
#                  postcode = map_chr(adresinfo, "postcode"),
#                  adres_afstand = map_chr(adresinfo, "adres_afstand"),
#                  adresinfo = NULL) %>% as_tibble()


get_adress2 <- function(x,y){
  #browser()
  centerbox = c(x,y,x,y)
  print(centerbox)
  #my_bbox <- st_transform(sf_punt, 28992) %>% st_bbox() #+ c(-100,-100,100,100)} %>% paste(collapse = ",")
  
  for (afstand in exp(seq(4, 9, by = 0.2))){
    #print(afstand)
    my_bbox_text <- {centerbox + c(-afstand, -afstand, afstand, afstand)} %>% paste(collapse = ",")
    
    url <- httr::parse_url("https://geodata.nationaalgeoregister.nl/inspireadressen/wfs?request=GetCapabilities&service=wfs")
    url$query <- list(service = "WFS",
                      version = "2.0.0",
                      request = "GetFeature",
                      typename = "inspireadressen:inspireadressen",
                      outputFormat = "application/json",
                      bbox = my_bbox_text)
    request <- httr::build_url(url)
    adressen <- sf::st_read(request, quiet = TRUE)
    if ( nrow(adressen) > 0) break
  }
  
  sf_punt <- tibble(x,y) %>% st_as_sf(coords = c("x", "y"), crs = 28992)
  adres_afstanden <- sf_punt %>% sf::st_distance(adressen) 
  adres_afstand <- min(adres_afstanden)
  adres_min <- adressen[which.min(adres_afstanden),] 
  adres_chr <- paste0(adres_min$straatnaam, " ", adres_min$huisnummer, adres_min$huisletter, adres_min$toevoeging, " ", adres_min$woonplaats) %>% 
    str_replace_all("NA", "")
  list(adres = adres_chr, postcode = as.character(adres_min$postcode), adres_afstand = adres_afstand )
}



# temp %>% mutate(adresinfo = map2(x,y, get_adress2), 
#                 adres = map_chr(adresinfo, "adres"), 
#                 postcode = map_chr(adresinfo, "postcode"),
#                 adres_afstand = map_chr(adresinfo, "adres_afstand"),
#                 adresinfo = NULL)


# get_bbg_2015_hoofdgroep <- function() {
#   url <- httr::parse_url("https://geodata.nationaalgeoregister.nl/bestandbodemgebruik2015/wfs?&request=GetCapabilities&service=WFS")
#   url$query <- list(service = "WFS",
#                     version = "2.0.0",
#                     request = "GetFeature",
#                     typename = "bestandbodemgebruik2015:bbg2015_hoofdgroep",
#                     bbox = "87000,432000,120000,457000")
#   
#   request <- httr::build_url(url)
#   gemeenten <- sf::st_read(request)
# }

get_bbg_2015 <- function() {
  url <- httr::parse_url("https://geodata.nationaalgeoregister.nl/bestandbodemgebruik2015/wfs?&request=GetCapabilities&service=WFS")
  url$query <- list(service = "WFS",
                    version = "2.0.0",
                    request = "GetFeature",
                    typename = "bestandbodemgebruik2015:bbg2015",
                    bbox = "87000,432000,120000,457000")
  
  request <- httr::build_url(url)
  gemeenten <- sf::st_read(request)
}

#temp <- get_bbg_2015()

