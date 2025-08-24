# -*- coding: UTF-8 -*-
#' Get the Wowhead icon URL for a given spell ID
#'
#' This function scrapes the spell page on Wowhead and extracts the
#' true in-game icon using the `iconFilename` specified in the
#' `WeakAuraExport.setOptions(...)` JavaScript block.
#'
#' @param spell_id A character or numeric spell ID (e.g., `"47540"` or `47540`)
#'
#' @return A character string containing the full URL to the icon image
#'   (e.g., `"https://wow.zamimg.com/images/wow/icons/large/spell_holy_penance.jpg"`),
#'   or `NA` if the icon could not be found or the request failed.
#'
#' @examples
#' wowhead_get_icon_from_spell_id(47540)
#' #> [1] "https://wow.zamimg.com/images/wow/icons/large/spell_holy_penance.jpg"
#'
#' @export
wowhead_get_icon_from_spell_id <- function(spell_id) {
  url <- paste0("https://www.wowhead.com/spell=", spell_id)

  resp <- tryCatch(
    {
      httr::GET(url, httr::add_headers(`User-Agent` = "Mozilla/5.0"))
    },
    error = function(e) {
      message("Error fetching spell ID ", spell_id, ": ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp) || httr::http_error(resp)) {
    message("Failed to fetch spell ID: ", spell_id)
    return(NA)
  }

  html <- httr::content(resp, "text", encoding = "UTF-8")

  icon_line <- stringr::str_extract(html, '"iconFilename":"[^"]+')

  if (is.na(icon_line)) {
    message("Icon not found for spell ID: ", spell_id)
    return(NA)
  }

  icon_name <- stringr::str_remove(icon_line, '"iconFilename":"')
  icon_url <- paste0("https://wow.zamimg.com/images/wow/icons/large/", icon_name, ".jpg")

  return(icon_url)
}
