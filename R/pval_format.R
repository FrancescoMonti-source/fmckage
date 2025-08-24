# Format p value removing precision if not significative or if "more significative"
# than sig.limit argument.

# Example: pval_format(c(0.1, 0.0001, 1e-27))

pval_format <- function(pvals, sig.limit = .001, digits = 3, html = FALSE) {
  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0("%.", digits, "f"), x)
    zzz <- paste0("0.", paste(rep("0", digits), collapse = ""))
    res[res == paste0("-", zzz)] <- zzz
    res
  }

  sapply(pvals, function(x, sig.limit) {
    if (x < sig.limit) {
      if (html) {
        return(sprintf("&lt; %s", format(sig.limit)))
      } else {
        return(sprintf("< %s", format(sig.limit)))
      }
    }
    if (x > .1) {
      return(roundr(x, digits = 2))
    } else {
      return(roundr(x, digits = digits))
    }
  }, sig.limit = sig.limit)
}
