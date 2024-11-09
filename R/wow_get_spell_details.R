#' Retrieve Details of WoW Spells from Wowhead
#'
#' This function retrieves detailed information for one or multiple World of Warcraft spells from Wowhead, including spell name, duration, cooldown, range, cast time, school, effects, and flags.
#'
#' @param spell_ids A vector of spell IDs (numeric or character) to retrieve details for. Each ID corresponds to a spell on Wowhead.
#' @return A data frame with columns:
#'   \describe{
#'     \item{spell_id}{The spell ID provided as input.}
#'     \item{name}{The name of the spell. If the spell ID does not exist, this will be `NA`.}
#'     \item{duration}{The duration of the spell in seconds, if available.}
#'     \item{cooldown}{The cooldown of the spell in seconds or minutes, if available.}
#'     \item{range}{The range of the spell in yards, if available.}
#'     \item{cast_time}{The cast time of the spell (e.g., "Instant" or in seconds), if available.}
#'     \item{school}{The school of magic associated with the spell (e.g., Arcane, Fire, etc.), if available.}
#'     \item{effects}{A concatenated string listing the effects of the spell, if available.}
#'     \item{flags}{A concatenated string listing the flags associated with the spell, if available.}
#'   }
#' If any information is unavailable, the respective fields will contain `NA`. For non-existent spell IDs, `name` and all other details will be `NA`.
#'
#' @details
#' This function uses web scraping to retrieve data from Wowhead's spell pages. If a spell ID is invalid or does not exist, the function will set `name` and other details to `NA`. Note that the function may be slow if retrieving information for a large number of spell IDs, and it relies on Wowhead's page structure, which may change over time.
#'
#' @examples
#' \dontrun{
#' # Retrieve details for a single spell
#' get_spell_details(438832)
#'
#' # Retrieve details for multiple spells, including a non-existent spell ID
#' spell_ids <- c(438832, 438833, 438834)  # Assume 438834 does not exist
#' spell_info <- get_spell_details(spell_ids)
#' print(spell_info)
#' }
#'
#' @import rvest dplyr stringr purrr
#' @export

get_spell_details <- function(spell_ids) {

    # Inner function to retrieve details for a single spell ID
    get_single_spell_details <- function(spell_id) {
        # Construct the URL for the spell on Wowhead
        url <- paste0("https://www.wowhead.com/spell=", spell_id)

        # Read the webpage content
        page <- tryCatch(read_html(url), error = function(e) NULL)

        # If the page failed to load, return NA values for all fields
        if (is.null(page)) {
            return(data.frame(
                spell_id = spell_id,
                name = NA,
                duration = NA, cooldown = NA, range = NA, cast_time = NA,
                school = NA, effects = NA, flags = NA, stringsAsFactors = FALSE
            ))
        }

        # Extract spell name from the main heading
        name <- page %>% html_node(".heading-size-1") %>% html_text(trim = TRUE)

        # If name is "Spells", it means the spell ID does not exist
        if (is.na(name) || name == "Spells") {
            name <- NA
        }

        # Extracting information from spelldetails
        spelldetails <- page %>% html_node("#spelldetails") %>% html_nodes("tr")

        # Extract specific fields and clean duplicates or irrelevant text
        duration <- spelldetails %>%
            html_text(trim = TRUE) %>%
            .[grep("Duration", .)] %>%
            str_extract("\\d+ seconds") %>%
            unique()

        range <- spelldetails %>%
            html_text(trim = TRUE) %>%
            .[grep("Range", .)] %>%
            str_extract("\\d+ yards") %>%
            unique()

        cast_time <- spelldetails %>%
            html_text(trim = TRUE) %>%
            .[grep("Cast time", .)] %>%
            str_extract("(\\d+ seconds|Instant)") %>%
            unique()

        cooldown <- spelldetails %>%
            html_text(trim = TRUE) %>%
            .[grep("Cooldown", .)] %>%
            str_extract("\\d+ (seconds|minutes)") %>%
            unique()

        school <- spelldetails %>%
            html_text(trim = TRUE) %>%
            .[grep("School", .)] %>%
            str_extract("(Arcane|Fire|Frost|Holy|Nature|Shadow|Physical)") %>%
            unique()

        # Extract Effects (Effect #1, Effect #2, etc.)
        effects <- spelldetails %>%
            html_text(trim = TRUE) %>%
            .[grep("Effect #", .)] %>%
            paste(collapse = "; ")  # Combine all effects into a single string

        # Extract Flags
        flags <- spelldetails %>%
            html_text(trim = TRUE) %>%
            .[grep("Flags", .)] %>%
            str_split("\n") %>%
            unlist() %>%
            str_trim() %>%
            paste(collapse = "; ")

        # Create a data frame for the current spell
        data.frame(
            spell_id = spell_id,
            name = ifelse(length(name) == 0, NA, name),
            duration = ifelse(length(duration) == 0, NA, duration),
            cooldown = ifelse(length(cooldown) == 0, NA, cooldown),
            range = ifelse(length(range) == 0, NA, range),
            cast_time = ifelse(length(cast_time) == 0, NA, cast_time),
            school = ifelse(length(school) == 0, NA, school),
            effects = ifelse(length(effects) == 0, NA, effects),
            flags = ifelse(length(flags) == 0, NA, flags),
            stringsAsFactors = FALSE
        )
    }

    # Apply the inner function to each spell_id in spell_ids and combine results
    map_dfr(spell_ids, get_single_spell_details)
}
