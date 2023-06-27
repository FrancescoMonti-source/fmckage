fuzzy_fill_na <- function(df1, df2, match_cols, match_fun_list, fill_cols, replace_cols = NULL, keep_cols = NULL) {
    # Perform the fuzzy join
    join_result <- df1 %>%
        fuzzyjoin::fuzzy_left_join(df2,
                                   by = match_cols,
                                   match_fun = match_fun_list)

    # Update fill_cols to reflect the column names after the join
    fill_cols <- paste0(fill_cols, ".x")

    # Fill in missing values in df1 with values from df2
    for (col in fill_cols) {
        join_result <- join_result %>%
            dplyr::mutate({{col}} := coalesce(.data[[col]], .data[[gsub(".x",".y", col)]]))
    }

    # Replace specified columns in df1 with their counterparts in df2
    if (!is.null(replace_cols)) {
        replace_cols <- paste0(replace_cols, ".x")
        for (col in replace_cols) {
            join_result <- join_result %>%
                dplyr::mutate({{col}} := .data[[gsub(".x",".y", col)]])
        }
    }

    # Keep specified columns from df2
    if (!is.null(keep_cols)) {
        keep_cols <- paste0(keep_cols, ".y")
    } else {
        keep_cols <- character(0)
    }

    # Keep only the original columns from df1 and specified columns from df2
    df_filled <- join_result %>%
        dplyr::select(c(dplyr::ends_with(".x"), keep_cols)) %>%
        dplyr::rename_with(~ gsub("\\.x$", "", .x), -all_of(keep_cols)) %>%
        dplyr::rename_with(~ paste0(gsub("\\.y$", "", .x), "_df2"), all_of(keep_cols))

    return(df_filled)
}
