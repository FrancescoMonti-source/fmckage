# Load necessary libraries
library(rvest)
library(dplyr)
library(purrr)

#' Retrieve Locations of NPCs from Wowhead
#'
#' This function retrieves the in-game locations of multiple World of Warcraft NPCs (mobs) from Wowhead based on their NPC IDs.
#'
#' @param npc_ids A vector of NPC IDs (numeric or character) to retrieve locations for. Each ID corresponds to an NPC on Wowhead.
#' @return A data frame with two columns:
#'   \describe{
#'     \item{npc_id}{The NPC ID provided as input.}
#'     \item{location}{The location(s) where the NPC can be found. If multiple locations exist, they are concatenated as a single string separated by "; ". If no location is found, this will be `NA`.}
#'   }
#' If an NPC ID does not exist or if Wowhead fails to load, `location` will contain `NA`.
#'
#' @details
#' This function uses web scraping to retrieve location data from Wowhead’s NPC pages. If an NPC ID is invalid or Wowhead does not provide location information, `location` will be set to `NA`. Note that this function may be slow for a large number of NPC IDs, as it sends a separate request for each ID.
#'
#' @examples
#' \dontrun{
#' # Retrieve location for a single NPC
#' wowhead_get_unit_location(214840)
#'
#' # Retrieve locations for multiple NPCs
#' npc_ids <- c(214840, 214841, 214842)  # Replace with actual NPC IDs
#' npc_locations <- wowhead_get_unit_location(npc_ids)
#' print(npc_locations)
#' }
#'
#' @import rvest dplyr purrr
#' @export
wowhead_get_unit_location <- function(npc_ids) {

    # Inner function to retrieve the location for a single NPC ID
    get_single_npc_location <- function(npc_id) {

        # Construct the URL for the NPC on Wowhead
        url <- paste0("https://www.wowhead.com/npc=", npc_id)

        # Try to read the page
        page <- tryCatch(read_html(url), error = function(e) NULL)

        # If the page fails to load, return NA
        if (is.null(page)) {
            return(NA)
        }

        # Extract the location(s) from the span with id="locations"
        locations <- page %>%
            html_node("#locations") %>%
            html_nodes("a") %>%  # Select all <a> tags within the locations span
            html_text(trim = TRUE)  # Get the text of each <a> tag

        # If no locations are found, return NA
        if (length(locations) == 0) {
            return(NA)
        }

        # Return the locations as a single concatenated string
        return(paste(locations, collapse = "; "))
    }

    # Apply the inner function to each NPC ID in the vector and combine results
    locations_data <- map_dfr(npc_ids, function(id) {
        tibble(npc_id = id, location = get_single_npc_location(id))
    })

    return(locations_data)
}
