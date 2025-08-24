#' Generate Cast Timeline
#'
#' This function processes a dataframe of spell data and generates a timeline of cast events based on
#' combattimer, duration, and progressive cooldown intervals.
#'
#' @param data A dataframe containing the columns `combattimer`, `duration`, and `progressive`.
#' @param max_time The maximum time (in seconds) to consider for generating the timeline. Default is 300.
#' @return A dataframe with expanded `cast_times` for each spell event.
#' @examples
#' # Example usage:
#' timeline <- generate_cast_timeline(data)
wow_generate_casts_timeline <- function(data, max_time = 240) {
  # Validate input data
  if (!all(c("combattimer", "duration", "progressive") %in% colnames(data))) {
    stop("The dataframe must contain 'combattimer', 'duration', and 'progressive' columns.")
  }

  process_cast_times <- function(combattimer, duration, progressive, max_time) {
    # Return an empty vector if combattimer or duration is NA
    if (is.na(combattimer)) {
      return(numeric(0))
    }

    start_time <- as.numeric(combattimer)
    duration <- as.numeric(duration)
    max_time <- as.numeric(max_time)

    if (progressive == "0" | is.na(progressive)) {
      # Fixed interval casts
      return(seq(start_time, max_time, by = duration))
    } else {
      # Alternating intervals: parse progressive into a vector of cooldowns
      cooldowns <- as.numeric(unlist(strsplit(progressive, " ")))
      if (any(is.na(cooldowns))) {
        return(numeric(0))
      } # Handle malformed progressive column

      current_time <- start_time
      times <- c(current_time)
      cooldown_index <- 1

      # Generate alternating cast times until reaching max_time
      while (!is.na(current_time) && current_time < max_time) {
        current_time <- current_time + cooldowns[cooldown_index]
        if (is.na(current_time) || current_time > max_time) break
        times <- c(times, current_time)

        # Alternate cooldown index
        cooldown_index <- ifelse(cooldown_index == length(cooldowns), 1, cooldown_index + 1)
      }
      return(times)
    }
  }


  data %>%
    filter(!is.na(combattimer) & (!is.na(duration) | !is.na(progressive))) %>% # Remove rows with invalid values
    rowwise() %>%
    mutate(
      cast_times = list(process_cast_times(combattimer, duration, progressive, max_time))
    ) %>%
    unnest_longer(cast_times) %>% # Expand cast times into rows
    ungroup() %>% # Remove rowwise grouping
    mutate(timer = paste("Timer:", cast_times)) %>%
    select(any_of(c("location", "unit", "isboss", "spell", "cast_times", "timer", "tooltip", "duration", "combattimer", "progressive")))
}
