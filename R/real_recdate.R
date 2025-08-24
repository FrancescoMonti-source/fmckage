real_recdate <- function(data, date_var = "RECDATE", text_var = "RECTXT") {
  # Check if 'lubridate' is installed and loaded
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    message("The 'lubridate' package is required for this function.")

    # Prompt user to install if not available
    install <- readline(prompt = "Would you like to install 'lubridate'? (yes/no): ")
    if (tolower(install) == "yes") {
      install.packages("lubridate")
      library(lubridate)
    } else {
      stop("Please install the 'lubridate' package to use this function.")
    }
  } else {
    # Load lubridate if installed but not loaded
    if (!"lubridate" %in% .packages()) library(lubridate)
  }

  # Extract real_date (YYYY-MM-DD format)
  data$real_date <- as_date(
    ifelse(grepl("\\d{4}-\\d{2}-\\d{2}", data[[text_var]]),
      sub(".*?(\\d{4}-\\d{2}-\\d{2}).*", "\\1", data[[text_var]]),
      NA
    )
  )

  # Extract real_date2 (entr[ée]+ pattern, DD/MM/YYYY format)
  data$real_date2 <- as_date(
    ifelse(grepl("entr[ée]+\\s*:\\s*([0-9]{2}\\/[0-9]{2}\\/[0-9]{4})", data[[text_var]]),
      sub(".*entr[ée]+\\s*:\\s*([0-9]{2}\\/[0-9]{2}\\/[0-9]{4}).*", "\\1", data[[text_var]]),
      NA
    ),
    format = "%d/%m/%Y"
  )

  # Convert RECDATE to Date format if it's not already
  data[[date_var]] <- as_date(data[[date_var]])

  # Use coalesce logic with ifelse to set real_date
  data$real_date <- as_date(ifelse(!is.na(data$real_date), data$real_date,
    ifelse(!is.na(data$real_date2), data$real_date2, data[[date_var]])
  ))

  # Calculate the difference in days
  data$diff <- as.numeric(data$real_date - data[[date_var]])

  # Return only the selected columns
  return(data[, c(date_var, "real_date", "ELTID", "diff")])
}
