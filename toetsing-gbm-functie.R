toetsing_gbm <- function(data, normen){
  
toetsing <- data %>% 
            filter(parnr > 999, parnr < 2000) %>% 
            semi_join(normen, by = "parnr") %>%
            mutate(detectiegrens = if_else(is.na(detectiegrens), "", detectiegrens), 
                   waarde_det_half = if_else(detectiegrens == "<", waarde / 2, waarde) ) %>%
            group_by(mp, parnr, par, jaar) %>%
            summarise(aantal = n(), 
                      aantal_det = sum(detectiegrens == "<"), 
                      MAX = max(waarde_det_half, na.rm = TRUE), 
                      JGM = mean(waarde_det_half, na.rm = TRUE), 
                      P90 = quantile(waarde_det_half, probs = 0.9, na.rm = TRUE) ) %>%
            left_join(normen, by = "parnr") %>%
            mutate(aangetroffen = if_else(aantal == aantal_det, 0, 1), 
                   factor_MAX = MAX / norm_MAX, 
                   factor_JGM = JGM / norm_JGM, 
                   factor_P90 = P90 / norm_P90 ) %>%
            rowwise() %>%
            mutate(hoogste_overschrijding = if_else(aangetroffen == 1, max(factor_MAX, factor_JGM, factor_P90, na.rm = TRUE), 0), 
                   normoverschrijding = if_else(hoogste_overschrijding > 1, 1, 0) ) %>% 
            ungroup()
  
toetsing  
  
}