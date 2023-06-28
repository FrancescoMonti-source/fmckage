#' Remove Accents from Characters
#'
#' This function removes various types of accents (acute, grave, circumflex, tilde, umlaut, and cedilla)
#' from a character string. The types of accents to be removed can be specified.
#'
#' @param str The character string from which accents are to be removed.
#' @param pattern A character vector specifying the types of accents to be removed. This can include
#' "´" for acute, "`" for grave, "^" for circumflex, "~" for tilde, "¨" for umlaut, "ç" for cedilla,
#' and "all" for all types of accents. If not specified, all types of accents are removed.
#'
#' @return A character string with the specified accents removed.
#'
#' @examples
#' rm_accent("pâté")
#' rm_accent("naïve")
#' rm_accent("papá", pattern = "´")
#'
#' @export
rm_accent <- function(str,pattern="all") {
    if(!is.character(str))
        str <- as.character(str)

    pattern <- unique(pattern)

    if(any(pattern=="Ç"))
        pattern[pattern=="Ç"] <- "ç"

    symbols <- c(
        acute = "áéíóúÁÉÍÓÚýÝ",
        grave = "àèìòùÀÈÌÒÙ",
        circunflex = "âêîôûÂÊÎÔÛ",
        tilde = "ãõÃÕñÑ",
        umlaut = "äëïöüÄËÏÖÜÿ",
        cedil = "çÇ"
    )

    nudeSymbols <- c(
        acute = "aeiouAEIOUyY",
        grave = "aeiouAEIOU",
        circunflex = "aeiouAEIOU",
        tilde = "aoAOnN",
        umlaut = "aeiouAEIOUy",
        cedil = "cC"
    )

    accentTypes <- c("´","`","^","~","¨","ç")

    if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern))
        return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))

    for(i in which(accentTypes%in%pattern))
        str <- chartr(symbols[i],nudeSymbols[i], str)

    return(str)
}
