make_unique_names <- function(nms, sep = "_") {
    # normalize whitespace
    nms <- trimws(nms)
    nms <- gsub("\\s+", "_", nms)

    # replace blanks/NA with a base name
    nms[nms == "" | is.na(nms)] <- "x"

    # number duplicates within each group
    idx <- ave(seq_along(nms), nms, FUN = seq_along)
    dup <- duplicated(nms) | duplicated(nms, fromLast = TRUE)
    nms[dup] <- paste0(nms[dup], sep, idx[dup])

    nms
}
