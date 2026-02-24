#' Estimatore "brut -> net final" (Francia) con IR progressivo e quotient familial
#'
#' Calcola una stima del netto *après impôt* a partire da un salario annuo lordo,
#' usando:
#' - una stima delle cotisations salariali (via `cotis_salariales_rate` oppure `net_sur_brut_ratio`)
#' - una base imponibile semplificata (abattement 10% con min/max oppure frais réels)
#' - un calcolo IR progressivo per tranches (barème) con quotient familial (`parts`)
#'
#' **Nota importante:** stima semplificata. Non include décote, réductions/crédits d'impôt,
#' prélèvements specifici, esenzioni, né casistiche particolari.
#'
#' @param brut_annuel Numeric(1). Salario annuo lordo in euro (>= 0).
#'
#' @param cotis_salariales_rate Numeric(1). Tasso stimato delle cotisations salariali
#'   (es. 0.23 => `net_avant_impot = brut * (1 - 0.23)`).
#'   Ignorato se `net_sur_brut_ratio` è fornito.
#'
#' @param net_sur_brut_ratio Numeric(1) oppure NULL. Rapporto netto/lordo
#'   (es. 0.77). Se fornito, sovrascrive `cotis_salariales_rate`.
#'
#' @param abattement10 Logical(1). Se TRUE applica l'abattement 10% (salarié),
#'   salvo `frais_reels` non-NULL.
#' @param abatt_rate Numeric(1). Tasso dell'abattement (tipicamente 0.10).
#' @param abatt_min Numeric(1). Minimo deducibile (in €) per l'abattement.
#' @param abatt_max Numeric(1). Massimo deducibile (in €) per l'abattement.
#'
#' @param frais_reels Numeric(1) oppure NULL. Se non-NULL, sostituisce l'abattement:
#'   deduzione fissa in euro (>=0).
#'
#' @param parts Numeric(1). Numero di parts per il quotient familial (> 0).
#' @param bareme List. Oggetto barème creato con [make_bareme()] (breaks + rates).
#'
#' @param hours_week Numeric(1). Ore di lavoro settimanali per stimare un netto orario.
#' @param weeks_year Numeric(1). Settimane annue per stimare un netto orario.
#'
#' @return `data.frame` con una riga e colonne:
#' \describe{
#'   \item{brut_annuel}{Lordo annuo (input).}
#'   \item{net_avant_impot}{Netto prima dell'IR (stima).}
#'   \item{mode_frais}{Modalità deduzione: "frais_reels", "abattement_10%", "aucun".}
#'   \item{deduction_frais}{Deduzione applicata (in €).}
#'   \item{revenu_imposable}{Base imponibile stimata dopo deduzioni.}
#'   \item{parts}{Parts utilizzate.}
#'   \item{ir_estime}{IR stimato sul foyer (stima semplificata).}
#'   \item{net_apres_impot}{Netto dopo IR.}
#'   \item{net_mensuel_apres_impot}{Netto mensile dopo IR.}
#'   \item{net_horaire_apres_impot}{Netto orario dopo IR.}
#' }
#'
#' @examples
#' # Esempio semplice (valori "passe-partout")
#' brut_to_net_final(
#'   brut_annuel = 80000,
#'   cotis_salariales_rate = 0.23,
#'   parts = 1
#' )
#'
#' # Definire un barème esplicito
#' bareme_2025 <- make_bareme(
#'   breaks = c(0, 11497, 29315, 83823, 180294),
#'   rates  = c(0.00, 0.11, 0.30, 0.41, 0.45)
#' )
#'
#' brut_to_net_final(
#'   brut_annuel = 80000,
#'   cotis_salariales_rate = 0.23,
#'   parts = 1,
#'   bareme = bareme_2025,
#'   hours_week = 35,
#'   abatt_min = 504,
#'   abatt_max = 14426
#' )
#'
#' @export
impots <- function(
        brut_annuel,
        # Cotisations: choisir UNE des deux options ci-dessous
        cotis_salariales_rate = 0.23,
        net_sur_brut_ratio = NULL,

        # Base imposable
        abattement10 = TRUE,
        abatt_rate = 0.10,
        abatt_min  = 504,
        abatt_max  = 14426,
        frais_reels = NULL,

        # IR
        parts = 1,
        bareme = make_bareme(
            breaks = c(0, 11497, 29315, 83823, 180294),
            rates  = c(0.00, 0.11, 0.30, 0.41, 0.45)
        ),

        # Conversions
        hours_week = 35,
        weeks_year = 52
) {
    stopifnot(is.numeric(brut_annuel), brut_annuel >= 0)

    # Net avant impôt (approx)
    if (!is.null(net_sur_brut_ratio)) {
        stopifnot(net_sur_brut_ratio > 0, net_sur_brut_ratio < 1.5)
        net_avant_impot <- brut_annuel * net_sur_brut_ratio
    } else {
        stopifnot(cotis_salariales_rate >= 0, cotis_salariales_rate < 1)
        net_avant_impot <- brut_annuel * (1 - cotis_salariales_rate)
    }

    # Revenu imposable "simplifié"
    if (!is.null(frais_reels)) {
        stopifnot(is.numeric(frais_reels), frais_reels >= 0)
        revenu_imposable <- max(0, net_avant_impot - frais_reels)
        deduction_frais  <- frais_reels
        mode_frais <- "frais_reels"
    } else if (abattement10) {
        revenu_imposable <- apply_abattement10(
            net_avant_impot,
            rate = abatt_rate,
            min_ded = abatt_min,
            max_ded = abatt_max
        )
        deduction_frais  <- net_avant_impot - revenu_imposable
        mode_frais <- "abattement_10%"
    } else {
        revenu_imposable <- net_avant_impot
        deduction_frais  <- 0
        mode_frais <- "aucun"
    }

    # IR (simple, sans décote, sans réductions/crédits)
    ir <- ir_household(revenu_imposable, parts = parts, bareme = bareme)

    net_apres_impot <- net_avant_impot - ir

    hours_year <- hours_week * weeks_year

    data.frame(
        brut_annuel = brut_annuel,
        net_avant_impot = net_avant_impot,
        mode_frais = mode_frais,
        deduction_frais = deduction_frais,
        revenu_imposable = revenu_imposable,
        parts = parts,
        ir_estime = ir,
        net_apres_impot = net_apres_impot,
        net_mensuel_apres_impot = net_apres_impot / 12,
        net_horaire_apres_impot = net_apres_impot / hours_year,
        stringsAsFactors = FALSE
    )
}

#' Creare un oggetto "barème IR" (tranches + taux marginaux)
#'
#' Costruisce un oggetto barème da passare alle funzioni di calcolo IR.
#' `breaks` sono i limiti inferiori delle tranches (devono iniziare da 0),
#' `rates` i tassi marginali associati (stessa lunghezza di `breaks`).
#'
#' @param breaks Numeric. Vettore crescente dei limiti inferiori delle tranches (prima = 0).
#' @param rates Numeric. Vettore dei tassi marginali (0..1), stessa lunghezza di `breaks`.
#'
#' @return Una lista con `breaks` e `rates`.
#'
#' @examples
#' make_bareme(
#'   breaks = c(0, 11497, 29315, 83823, 180294),
#'   rates  = c(0.00, 0.11, 0.30, 0.41, 0.45)
#' )
#'
#' @keywords internal
make_bareme <- function(breaks, rates) {
    stopifnot(is.numeric(breaks), is.numeric(rates))
    stopifnot(length(breaks) == length(rates))
    stopifnot(breaks[1] == 0)
    stopifnot(all(diff(breaks) > 0))
    stopifnot(all(rates >= 0), all(rates <= 1))
    list(breaks = breaks, rates = rates)
}

#' Calcolo IR progressivo per 1 part (taux marginaux)
#'
#' Applica il barème progressivo ad un reddito imponibile per *una* part.
#' Non gestisce decote, réductions/crédits, né altre regole avanzate.
#'
#' @param income Numeric(1). Revenu imposable per 1 part (>=0; valori <0 sono clampati a 0).
#' @param bareme List. Oggetto creato con [make_bareme()].
#'
#' @return Numeric(1). Imposta stimata per 1 part.
#'
#' @keywords internal
ir_one_part <- function(income, bareme) {
    income <- max(0, income)
    b <- bareme$breaks
    r <- bareme$rates
    upp <- c(b[-1], Inf)

    tax <- 0
    for (i in seq_along(b)) {
        low <- b[i]
        high <- upp[i]
        if (income <= low) break
        tranche <- min(income, high) - low
        tax <- tax + tranche * r[i]
    }
    tax
}

#' Calcolo IR del foyer con quotient familial (versione semplificata)
#'
#' Applica il quotient familial: `taxable_income / parts`, calcolo su 1 part,
#' poi moltiplica per `parts`.
#'
#' @param taxable_income Numeric(1). Revenu imposable del foyer (>=0 consigliato).
#' @param parts Numeric(1). Numero di parts (>0).
#' @param bareme List. Oggetto creato con [make_bareme()].
#'
#' @return Numeric(1). IR stimato per il foyer.
#'
#' @keywords internal
ir_household <- function(taxable_income, parts = 1, bareme) {
    stopifnot(parts > 0)
    q <- taxable_income / parts
    ir_one_part(q, bareme) * parts
}

#' Applicare l'abattement 10% (salarié) con minimo e massimo
#'
#' Calcola la deduzione come `net_income * rate`, poi applica `min_ded` e `max_ded`,
#' e restituisce il reddito dopo deduzione.
#'
#' @param net_income Numeric(1). Reddito (tipicamente `net_avant_impot`).
#' @param rate Numeric(1). Tasso dell'abattement (tipicamente 0.10).
#' @param min_ded Numeric(1). Deduzione minima (in €).
#' @param max_ded Numeric(1). Deduzione massima (in €).
#'
#' @return Numeric(1). Reddito dopo deduzione.
#'
#' @keywords internal
apply_abattement10 <- function(net_income, rate = 0.10, min_ded = 504, max_ded = 14426) {
    ded <- net_income * rate
    ded <- max(min_ded, min(max_ded, ded))
    net_income - ded
}
