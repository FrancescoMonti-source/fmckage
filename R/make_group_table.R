#' Summarise binary variables across groups
#'
#' Create a descriptive table of binary variables stratified by a grouping
#' variable (which can have 2 or more levels). For each variable, the function
#' reports counts, denominators, and percentages in the format
#' \code{"N/total (xx.x%)"}. An overall "Total" column is always added.
#'
#' @param data A data frame with one row per subject.
#' @param group_col Bare column name of the grouping variable.
#'   Can be numeric, character, or factor with any number of levels.
#' @param exclude Optional character vector of column names to drop before
#'   selecting binary variables.
#' @param na_mode One of \code{"exclude"} (default) or \code{"include"}.
#'   If \code{"exclude"}, denominators exclude missing values (typical
#'   epidemiology convention). If \code{"include"}, denominators use all rows
#'   in the group, including missing values.
#' @param include_missing Logical, default \code{TRUE}. If \code{TRUE}, a
#'   separate "— Missing" row is added for each variable, showing the number
#'   and percentage of \code{NA} values in each group and overall.
#' @param digits Integer, number of decimal places for percentages. Default 1.
#' @param group_order Optional vector giving the desired order of groups in
#'   the output (e.g. \code{c("control","treatment")}).
#'   If \code{NULL}, factor levels are respected (if \code{group_col} is a
#'   factor), otherwise natural sort of unique values is used.
#' @param group_labels Optional named character vector mapping raw group
#'   values to pretty column labels. Names should match the raw group values,
#'   values are the desired labels. Unmapped groups keep their raw name.
#'
#' @return A tibble with one row per variable (plus optional "— Missing" rows)
#'   and one column per group, plus a "Total" column. Each cell contains a
#'   string formatted as \code{"N/total (xx.x%)"}.
#'
#' @details
#' Binary variables are detected automatically: a column is considered binary if
#' its non-missing unique values can be coerced to 0/1. Supported storage types
#' include numeric, logical, factor, and character.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   group = factor(sample(c("NS", "SI", "SA"), 100, TRUE)),
#'   diabete = rbinom(100, 1, 0.2),
#'   hta = rbinom(100, 1, 0.3),
#'   cancer = sample(c(0, 1, NA), 100, TRUE, prob = c(0.8, 0.15, 0.05))
#' )
#'
#' # Default: exclude missing from denominator, include "Missing" rows
#' make_group_table(df, group_col = group)
#'
#' # Include NA in denominator, custom order and labels
#' make_group_table(
#'   df,
#'   group_col = group,
#'   na_mode = "include",
#'   group_order = c("NS", "SI", "SA"),
#'   group_labels = c(NS = "non_suicidaires", SI = "suicidaires_I", SA = "suicidaires_A")
#' )
#'
#' @export

make_group_table <- function(data,
                             group_col = group,
                             exclude = NULL,
                             na_mode = c("exclude", "include"),
                             include_missing = TRUE,
                             digits = 1,
                             group_order = NULL,
                             group_labels = NULL) {
  na_mode <- match.arg(na_mode)
  group_sym <- rlang::ensym(group_col)
  group_str <- rlang::as_string(group_sym)

  is_binary <- function(x) {
    suppressWarnings(xx <- as.numeric(as.character(x)))
    ux <- unique(stats::na.omit(xx))
    length(ux) <= 2 && all(ux %in% c(0, 1))
  }

  # select group + binary vars, coerce binaries to numeric 0/1
  df <- data %>%
    {
      if (!is.null(exclude)) dplyr::select(., -tidyselect::all_of(exclude)) else .
    } %>%
    dplyr::select(!!group_sym, dplyr::where(is_binary)) %>%
    dplyr::mutate(dplyr::across(-!!group_sym, ~ suppressWarnings(as.numeric(as.character(.x)))))

  # establish group ordering
  grp_vals <- df[[group_str]]
  if (is.null(group_order)) {
    if (is.factor(grp_vals)) group_order <- levels(grp_vals) else group_order <- sort(unique(grp_vals))
  }
  grp_chr_order <- as.character(group_order)

  long <- tidyr::pivot_longer(df, -!!group_sym, names_to = "variable", values_to = "value")

  # size per group (for na_mode = "include")
  n_by_group <- dplyr::count(dplyr::distinct(long, !!group_sym), !!group_sym, name = "n_group")

  by_group <- long %>%
    dplyr::group_by(variable, !!group_sym) %>%
    dplyr::summarise(
      N = sum(value == 1, na.rm = TRUE),
      nonmiss = sum(!is.na(value)),
      miss = sum(is.na(value)),
      .groups = "drop"
    ) %>%
    dplyr::left_join(n_by_group, by = group_str) %>%
    dplyr::mutate(
      total = if (na_mode == "exclude") nonmiss else n_group,
      pct   = 100 * N / dplyr::if_else(total > 0, total, NA_real_)
    )

  overall <- by_group %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      !!group_str := "everyone",
      N = sum(N), nonmiss = sum(nonmiss), miss = sum(miss),
      total = if (na_mode == "exclude") sum(nonmiss) else sum(n_group),
      .groups = "drop"
    ) %>%
    dplyr::mutate(pct = 100 * N / dplyr::if_else(total > 0, total, NA_real_))

  wide <- dplyr::bind_rows(by_group, overall) %>%
    dplyr::mutate(group_chr = as.character(!!group_sym)) %>%
    dplyr::select(variable, group_chr, N, total, pct) %>%
    tidyr::pivot_wider(
      names_from = group_chr,
      values_from = c(N, total, pct),
      names_glue = "{.value}_{group_chr}"
    )

  fmt <- function(n, t, p) {
    ifelse(is.na(p), sprintf("%d/%d (NA)", n, t),
      sprintf(paste0("%%d/%%d (%%.", digits, "f%%%%)")) %>% sprintf(n, t, p)
    )
  }

  # ---- dynamic build of K group columns ----
  # derive available group suffixes from the N_* columns
  have_groups <- sub("^N_", "", grep("^N_", names(wide), value = TRUE))

  # Order: use group_order first (intersect), keep any extras afterwards
  ordered_groups <- c(
    intersect(grp_chr_order, have_groups),
    setdiff(have_groups, grp_chr_order)
  )

  # pretty labels
  label_of <- function(g) {
    if (!is.null(group_labels) && !is.null(group_labels[[g]])) group_labels[[g]] else g
  }

  # create formatted columns for each group level
  for (g in ordered_groups) {
    Ncol <- paste0("N_", g)
    Tcol <- paste0("total_", g)
    Pcol <- paste0("pct_", g)
    lbl <- label_of(g)
    wide[[lbl]] <- if (all(c(Ncol, Tcol, Pcol) %in% names(wide))) fmt(wide[[Ncol]], wide[[Tcol]], wide[[Pcol]]) else NA_character_
  }

  # overall column last
  wide[["Total"]] <- if (all(c("N_everyone", "total_everyone", "pct_everyone") %in% names(wide))) {
    fmt(wide[["N_everyone"]], wide[["total_everyone"]], wide[["pct_everyone"]])
  } else {
    NA_character_
  }

  out_main <- dplyr::select(wide, variable, dplyr::all_of(c(ordered_groups %>% vapply(label_of, FUN.VALUE = character(1)), "Total")))

  # ---- optional Missing rows (also K-level) ----
  if (isTRUE(include_missing)) {
    miss_by_group <- long %>%
      dplyr::group_by(variable, !!group_sym) %>%
      dplyr::summarise(miss = sum(is.na(value)), .groups = "drop") %>%
      dplyr::mutate(group_chr = as.character(!!group_sym)) %>%
      dplyr::select(variable, group_chr, miss) %>%
      tidyr::pivot_wider(names_from = group_chr, values_from = miss, names_glue = "miss_{group_chr}")

    miss_overall <- long %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(miss_everyone = sum(is.na(value)), .groups = "drop")

    miss_tbl <- dplyr::left_join(miss_by_group, miss_overall, by = "variable")

    totals_for_miss <- dplyr::bind_rows(by_group, overall) %>%
      dplyr::mutate(group_chr = as.character(!!group_sym)) %>%
      dplyr::select(variable, group_chr, total) %>%
      tidyr::pivot_wider(names_from = group_chr, values_from = total, names_glue = "total_{group_chr}")

    miss_tbl <- dplyr::left_join(miss_tbl, totals_for_miss, by = "variable")

    miss_fmt <- function(m, t) {
      ifelse(is.na(m) | is.na(t), NA_character_,
        sprintf(paste0("%%d/%%d (%%.", digits, "f%%%%)"), m, t, 100 * m / dplyr::if_else(t > 0, t, NA_real_))
      )
    }

    miss_out <- tibble::tibble(variable = paste0(miss_tbl$variable, " — Missing"))
    for (g in ordered_groups) {
      mc <- paste0("miss_", g)
      tc <- paste0("total_", g)
      lbl <- label_of(g)
      miss_out[[lbl]] <- if (all(c(mc, tc) %in% names(miss_tbl))) miss_fmt(miss_tbl[[mc]], miss_tbl[[tc]]) else NA_character_
    }
    miss_out[["Total"]] <- if (all(c("miss_everyone", "total_everyone") %in% names(miss_tbl))) {
      miss_fmt(miss_tbl[["miss_everyone"]], miss_tbl[["total_everyone"]])
    } else {
      NA_character_
    }

    out <- dplyr::bind_rows(out_main, miss_out) %>% dplyr::arrange(variable)
  } else {
    out <- out_main
  }

  out
}
