#' Convert durations between time units with flexible formatting
#'
#' `convert_time()` converts numeric values from one time unit into a breakdown
#' of other time units (e.g., seconds → minutes + seconds), and returns a
#' formatted string. Multiple output styles are supported, including abbreviated
#' (`"3m 17s"`), colon-separated (`"3:17"`), and long form (`"3 minutes 17 seconds"`).
#'
#' @param x Numeric vector of values to convert.
#' @param from Character scalar. Input unit of \code{x}. Must be one of
#'   `"years"`, `"months"`, `"weeks"`, `"days"`, `"hours"`, `"minutes"`, `"seconds"`.
#' @param to Character vector of target units in decreasing order (e.g.,
#'   \code{c("minutes", "seconds")}). Must be a subset of the same set of unit
#'   names as \code{from}.
#' @param warn_residual Logical (default `TRUE`). If `TRUE`, emit a warning when
#'   some residual time cannot be represented with the chosen `to` units.
#' @param style Output formatting style. One of:
#'   \describe{
#'     \item{`"abbr"`}{Abbreviated with first letter of each unit
#'       (default). Example: `"3m 17s"`.}
#'     \item{`"colon"`}{Clock-like colon-separated. Works only when `to` is a
#'       contiguous chain such as `c("minutes","seconds")` or
#'       `c("hours","minutes","seconds")`. Example: `"3:17"`, `"1:02:03"`.}
#'     \item{`"long"`}{Full unit names with pluralization. Example:
#'       `"3 minutes 17 seconds"`.}
#'   }
#' @param zero_pad Logical (default `TRUE`). If `TRUE`, pad smaller units to
#'   two digits when `style = "colon"`. Example: `"1:02:03"` instead of `"1:2:3"`.
#'
#' @return A character vector of formatted time strings of the same length as
#'   \code{x}. Elements are `NA` if input is `NA` or if no units could be
#'   represented.
#'
#' @examples
#' # Seconds to minutes + seconds
#' convert_time(197, from = "seconds", to = c("minutes","seconds"))
#' #> "3m 17s"
#'
#' # Colon style
#' convert_time(197, from = "seconds", to = c("minutes","seconds"), style = "colon")
#' #> "3:17"
#'
#' # Long style
#' convert_time(197, from = "seconds", to = c("minutes","seconds"), style = "long")
#' #> "3 minutes 17 seconds"
#'
#' # Hours:minutes:seconds with zero-padding
#' convert_time(3723, from = "seconds", to = c("hours","minutes","seconds"), style = "colon")
#' #> "1:02:03"
#'
#' # Vectorized input
#' convert_time(c(59, 60, 61, 197), from = "seconds", to = c("minutes","seconds"), style = "colon")
#' #> "0:59" "1:00" "1:01" "3:17"
#'
#' @export



convert_time <- function(
        x,
        from = NULL,
        to = NULL,                 # e.g., c("minutes","seconds")
        warn_residual = TRUE,
        style = c("abbr", "colon", "long"),  # output style
        zero_pad = TRUE                        # for colon style or abbr with small units
) {
    style <- match.arg(style)

    if (is.null(from) || is.null(to)) {
        stop("Please specify both 'from' and 'to' parameters.")
    }

    # seconds per unit (averages for months/years like you had)
    time_units <- c(
        years  = 60 * 60 * 24 * 365.25,
        months = 60 * 60 * 24 * 30.44,
        weeks  = 60 * 60 * 24 * 7,
        days   = 60 * 60 * 24,
        hours  = 60 * 60,
        minutes= 60,
        seconds= 1
    )

    # validate units
    if (!from %in% names(time_units)) stop("Unknown 'from' unit: ", from)
    if (!all(to %in% names(time_units))) {
        bad <- setdiff(to, names(time_units))
        stop("Unknown 'to' units: ", paste(bad, collapse = ", "))
    }

    # normalize input to seconds
    x_seconds <- as.numeric(x) * time_units[from]

    # compute unit breakdown
    breakdown <- lapply(x_seconds, function(xs) {
        if (is.na(xs)) return(rep(NA_real_, length(to)))
        rem <- xs
        out <- numeric(length(to))
        for (i in seq_along(to)) {
            u <- to[i]
            out[i] <- floor(rem / time_units[u])
            rem <- rem %% time_units[u]
        }
        attr(out, "residual_seconds") <- rem
        names(out) <- to
        out
    })

    # residual warning
    if (warn_residual) {
        res <- vapply(breakdown, function(v) attr(v, "residual_seconds"), numeric(1))
        if (any(res > 0)) {
            warning("Residual time remaining after conversion (not representable with chosen 'to' units) for some inputs.")
        }
    }

    # formatters
    fmt_abbr <- function(vec) {
        # drop zeros, keep order
        keep <- vec > 0 | (all(vec == 0) & length(vec) > 0)
        vec <- vec[keep]
        units <- names(vec)
        if (!length(vec)) return(NA_character_)
        paste0(vec, substr(units, 1, 1), collapse = " ")
    }

    pluralize <- function(n, unit) {
        if (n == 1) unit else paste0(unit, "s")
    }

    fmt_long <- function(vec) {
        keep <- vec > 0 | (all(vec == 0) & length(vec) > 0)
        vec <- vec[keep]
        units <- names(vec)
        if (!length(vec)) return(NA_character_)
        parts <- mapply(function(n, u) paste(n, pluralize(n, u)), vec, units, USE.NAMES = FALSE)
        paste(parts, collapse = " ")
    }

    fmt_colon <- function(vec) {
        # Colon style expects contiguous, clock-like units (e.g., hours:minutes:seconds or minutes:seconds)
        # We’ll allow any chain among (hours, minutes, seconds) or (days, hours, minutes, seconds).
        allowed <- list(
            c("minutes", "seconds"),
            c("hours", "minutes"),
            c("hours", "minutes", "seconds"),
            c("days", "hours", "minutes"),
            c("days", "hours", "minutes", "seconds")
        )
        to_now <- names(vec)
        if (!any(vapply(allowed, function(p) identical(p, to_now), logical(1)))) {
            stop("For style='colon', 'to' must be a contiguous clock-like chain, e.g. c('minutes','seconds') or c('hours','minutes','seconds').")
        }

        # First unit unpadded; subsequent units zero-padded to 2 by default
        pad <- function(n, first = FALSE) {
            if (first || !zero_pad) as.character(n) else sprintf("%02d", as.integer(n))
        }

        if (all(is.na(vec))) return(NA_character_)
        if (length(vec) == 0) return(NA_character_)

        out <- character(length(vec))
        for (i in seq_along(vec)) {
            out[i] <- if (i == 1) pad(vec[i], first = TRUE) else pad(vec[i], first = FALSE)
        }
        paste(out, collapse = ":")
    }

    # build formatted output
    out <- vapply(breakdown, function(vec) {
        if (all(is.na(vec))) return(NA_character_)
        switch(
            style,
            abbr  = fmt_abbr(vec),
            long  = fmt_long(vec),
            colon = fmt_colon(vec)
        )
    }, character(1))

    out
}
