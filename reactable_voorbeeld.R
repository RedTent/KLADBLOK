# https://glin.github.io/reactable/articles/examples.html

library(tidyverse)
library(reactable)


data <- HHSKwkl:::test_fys_chem()

lang_instellingen <- 
  reactableLang(
    sortLabel = "Sorteer {name}",
    filterPlaceholder = "",
    filterLabel = "Filter {name}",
    searchPlaceholder = "Zoek",
    searchLabel = "Zoek",
    noData = "Geen data gevonden",
    pageNext = "Volgende",
    pagePrevious = "Vorige",
    pageNumbers = "{page} van {pages}",
    pageInfo = "{rowStart}\u2013{rowEnd} van {rows} rijen",
    pageSizeOptions = "Toon {rows}",
    pageNextLabel = "volgende pagina",
    pagePreviousLabel = "Vorige pagina",
    pageNumberLabel = "Pagina {page}",
    pageJumpLabel = "Ga naar pagina page",
    pageSizeOptionsLabel = "Rijen per pagina",
    groupExpandLabel = "Toggle group",
    detailsExpandLabel = "Toggle details",
    selectAllRowsLabel = "Select all rows",
    selectAllSubRowsLabel = "Select all rows in group",
    selectRowLabel = "Select row",
    defaultGroupHeader = NULL,
    detailsCollapseLabel = NULL,
    deselectAllRowsLabel = NULL,
    deselectAllSubRowsLabel = NULL,
    deselectRowLabel = NULL
  )

options(reactable.language = lang_instellingen)

data %>% 
  reactable(filterable = TRUE, 
            defaultPageSize = 7,
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(5,10,25,100),
            language = lang_instellingen,
            columns = list(
              waarde = colDef(format = colFormat(locales = "nl-NL")),
              mp = colDef(sticky = "left")
              
            ),
            striped = TRUE, 
            highlight = TRUE,
            # outlined = TRUE,
            columnGroups = list(
              colGroup(name = "Meetwaarde", columns = c("detectiegrens", "waarde")),
              colGroup(name = "Parameterinfo", columns = c("parnr", "par", "eenheid"))),
            resizable = TRUE,
            )
            




