set_dt_options <- function() {
  options(DT.options = list(
    language = list(
      paginate = list(previous = 'Vorige', `next` = 'Volgende'),
      search = "Zoeken",
      decimal = ",",
      info = 'Rij <b>_START_</b> tot <b>_END_</b> - Totaal: <b>_TOTAL_</b> rijen',
      lengthMenu = "Toon _MENU_ rijen"
    ),
    buttons = c('copy', 'excel', 'pdf'),
    dom = "lifrtpB"
  ))  
}
