process_prescriptions <- function(data) {
  data <- data %>%
    map_dfr(as.data.frame) %>%
    mutate(across(everything(), function(x) as.character(x))) %>%
    select(PATID, EVTID, ELTID, everything()) %>%
    rename_with(~ str_remove(.x, "^PRES\\."), starts_with("PRES.")) %>%
    mutate(
      adm_start = str_extract(CAREPLAN, "(\\d{4}\\-\\d{2}\\-\\d{2})\\sà", group = 1),
      adm_end = str_extract(CAREPLAN, "\\d{4}\\-\\d{2}\\-\\d{2}\\sà\\s(\\d{4}\\-\\d{2}\\-\\d{2})", group = 1)
    ) %>%
    select(PATID:ELTID, SPELABEL, CAREPLAN, DATENT, DATSORT, DATPRES, adm_start, adm_end, everything()) %>%
    mutate(across(DATENT:adm_end, function(x) as_date(x)))

  return(data)
}
