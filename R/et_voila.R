#' Generate a Sample with a Specific Aggregate Statistic
#'
#' This function generates a sample of `n` numbers within a given range `[min_val, max_val]`
#' such that a specified aggregate statistic (e.g., mean, median) is approximately equal to a target value.
#'
#' @param n Integer. The size of the sample to generate.
#' @param min_val Numeric. The minimum value that can appear in the sample.
#' @param max_val Numeric. The maximum value that can appear in the sample.
#' @param target_value Numeric. The target value for the aggregate statistic.
#' @param agg_func Function. The aggregate function to use (e.g., `mean`, `median`). Default is `mean`.
#'
#' @return A numeric vector of length `n` that approximates the target aggregate statistic.
#'
#' @examples
#' \dontrun{
#' # Generate a sample with a target mean of 6
#' sample_data_mean <- et_voila(300, 1, 30, 6, mean)
#'
#' # Generate a sample with a target median of 6
#' sample_data_median <- et_voila(300, 1, 30, 6, median)
#' }
#'
#' @export
#'

et_voila <- function(n, min_val, max_val, target_value, agg_func = mean) {
# Inizializza il vettore dei risultati
results <- sample(min_val:max_val, n, replace = TRUE)

# Calcola la differenza tra il valore target e la statistica aggregata attuale
diff_value = target_value - agg_func(results)

# Logging
print(paste("Differenza iniziale:", diff_value))

# Controllo dei limiti e correzione
while (abs(diff_value) > 1e-6) {
    # Trova un indice casuale nel vettore dove il valore è maggiore di min_val
    indices_over_min <- which(results > min_val)
    random_index <- sample(indices_over_min, 1)

    # Aggiusta questo valore
    results[random_index] <- results[random_index] + sign(diff_value)

    # Ricalcola la differenza
    diff_value = target_value - agg_func(results)
}

# Test statistici
print(paste("Statistica aggregata calcolata:", agg_func(results)))

return(results)
}
