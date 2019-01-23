library(tidyverse)
library(sf)
library(tmap)

baseurl <- "https://geodata.nationaalgeoregister.nl/nationaleparken/wfs?"
# Verzoek om output in GeoJSON
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=nationaleparken&outputFormat=application%2Fjson"

wfs_request2 <- paste("request=GetFeature",
                      "service=WFS",
                      "version=2.0.0",
                      "typeName=nationaleparken",
                      "outputFormat=application%2Fjson", 
                      sep = "&")

nl_nationale_parken_wfs <- paste0(baseurl,wfs_request)

nl_nationale_parken <- st_read(nl_nationale_parken_wfs)
nl_nationale_parken

tmap_mode("view")
qtm(nl_nationale_parken, borders = "red", fill = "darkgreen", basemaps = c("Esri.WorldTopoMap","Esri.NatGeoWorldMap","CartoDB.Positron", "OpenStreetMap"))
qtm(nl_nationale_parken, borders = "red", fill = "darkgreen", basemaps = c(test1 = "Esri.WorldTopoMap",test2 = "Esri.NatGeoWorldMap",test3 = "CartoDB.Positron", test4 = "OpenStreetMap"))




# Tutorial WFS ------------------------------------------------------------

# https://github.com/TWIAV/Spatial_Analysis_in_R_with_Open_Geodata

# request = GetCapabilities
# request = GetFeature
# request = DescribeFeatureType 

library(httr)
library(sf)
library(tmap)
url <- parse_url("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs")
url$query <- list(service = "WFS",
                  version = "2.0.0",
                  request = "GetFeature",
                  typename = "cbsgebiedsindelingen:cbs_gemeente_2017_gegeneraliseerd",
                  outputFormat = "application/json")
request <- build_url(url)

NL_Municipalities2017 <- st_read(request)
qtm(NL_Municipalities2017)

#eigen probeersel
url <- parse_url("https://geodata.nationaalgeoregister.nl/bestuurlijkegrenzen/wfs?request=GetCapabilities&service=wfs")
url$query <- list(service = "WFS",
                  version = "2.0.0",
                  request = "GetFeature",
                  typename = "bestuurlijkegrenzen:provincies",
                  outputFormat = "application/json")

request <- build_url(url)
prov <- st_read(request)
qtm(prov, alpha = 0.5, fill = "lightblue")
qtm(prov, fill = NULL) # alleen grenzen

url <- parse_url("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?service=WFS&version=2.0.0&request=DescribeFeatureType&typename=cbsgebiedsindelingen:cbs_gemeente_2017_gegeneraliseerd")


# uitpluizen xml GetCapabilities ------------------------------------------

library(httr)
library(xml2)


url <- parse_url("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs")
url <- parse_url("https://geodata.nationaalgeoregister.nl/bestuurlijkegrenzen/wfs")
url$query <- list(service = "WFS",
                  version = "2.0.0",
                  request = "GetCapabilities")
request <- build_url(url)
doc <- GET(request) %>% content(as = "text", encoding = "UTF-8") %>% read_xml()

#output formats
xpath <- paste0("//ows:Operation[@name='GetFeature']",
                "/ows:Parameter[@name='outputFormat']",
                "/ows:AllowedValues/ows:Value")
output_formats <- doc %>% xml_find_all(xpath) %>% xml_text()
output_formats

# featuretypes
xpath <- "//wfs:FeatureType/wfs:Name"
feature_types <- doc %>% xml_find_all(xpath) %>% xml_text()
feature_types

# max recordcount
xpath <- "//ows:Constraint[@name='CountDefault']/ows:DefaultValue"
maxRecordCount <- doc %>% xml_find_first(xpath) %>% xml_integer()
maxRecordCount
