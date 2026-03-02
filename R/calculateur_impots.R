#' Create a progressive income-tax schedule ("barème")
#'
#' Helper to define a French-style progressive tax schedule as lower bounds
#' (`breaks`) and marginal rates (`rates`).
#'
#' @param breaks Numeric vector of lower bounds for each bracket, **starting at 0**,
#'   strictly increasing.
#' @param rates Numeric vector of marginal rates (same length as `breaks`),
#'   each in \[0, 1\].
#'
#' @return A list with two elements:
#' \describe{
#'   \item{breaks}{Numeric vector of bracket lower bounds.}
#'   \item{rates}{Numeric vector of marginal tax rates.}
#' }
#'
#' @examples
#' b <- make_bareme(
#'   breaks = c(0, 11497, 29315, 83823, 180294),
#'   rates  = c(0.00, 0.11, 0.30, 0.41, 0.45)
#' )
#' str(b)
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

#' Compute progressive income tax for one fiscal "part"
#'
#' Applies the marginal rates of a `bareme` to a taxable income amount for a single
#' household part (before applying the quotient familial).
#'
#' @param income Numeric scalar. Taxable income for **one** part. Values < 0 are
#'   treated as 0.
#' @param bareme List as returned by [make_bareme()], containing `breaks` and `rates`.
#'
#' @return Numeric scalar: computed tax for one part.
#'
#' @examples
#' bareme_2025 <- make_bareme(
#'   breaks = c(0, 11497, 29315, 83823, 180294),
#'   rates  = c(0.00, 0.11, 0.30, 0.41, 0.45)
#' )
#' ir_one_part(30000, bareme_2025)
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

#' Compute household income tax using the quotient familial (simplified)
#'
#' Computes income tax by dividing taxable income by the number of fiscal parts,
#' applying the progressive schedule to one part, then multiplying back by parts.
#'
#' This is a **simplified** computation: it does not include decote, plafonnement
#' du quotient familial, reductions/crédits d'impôt, exceptional contributions, etc.
#'
#' @param taxable_income Numeric scalar. Household taxable income after deductions
#'   (e.g., after 10% allowance or frais réels).
#' @param parts Numeric scalar > 0. Number of fiscal parts.
#' @param bareme List as returned by [make_bareme()].
#'
#' @return Numeric scalar: estimated household income tax.
#'
#' @examples
#' b <- make_bareme(
#'   breaks = c(0, 11497, 29315, 83823, 180294),
#'   rates  = c(0.00, 0.11, 0.30, 0.41, 0.45)
#' )
#' ir_household(taxable_income = 60000, parts = 2, bareme = b)
#'
#' @keywords internal
ir_household <- function(taxable_income, parts = 1, bareme) {
    stopifnot(parts > 0)
    q <- taxable_income / parts
    ir_one_part(q, bareme) * parts
}

#' Apply the 10% employee allowance (abattement 10%) with floor and cap
#'
#' Applies the standard French employee deduction ("frais professionnels") as:
#' `deduction = clamp(rate * income, min_ded, max_ded)`.
#'
#' @param net_income Numeric scalar. Base income to which the deduction is applied
#'   (typically net imposable).
#' @param rate Numeric scalar in \[0, 1\]. Default 0.10.
#' @param min_ded Numeric scalar >= 0. Minimum deduction amount in euros.
#' @param max_ded Numeric scalar >= 0. Maximum deduction amount in euros.
#'
#' @return Numeric scalar: `net_income` after the deduction.
#'
#' @examples
#' apply_abattement10(30000, rate = 0.10, min_ded = 504, max_ded = 14426)
#'
#' @keywords internal
apply_abattement10 <- function(net_income, rate = 0.10, min_ded = 504, max_ded = 14426) {
    ded <- net_income * rate
    ded <- max(min_ded, min(max_ded, ded))
    net_income - ded
}

#' Estimate French annual net after income tax from annual gross salary (simplified)
#'
#' Estimates a "net after income tax" from an annual gross salary using:
#' 1) a social-contributions approximation (`cotis_salariales_rate` or `net_sur_brut_ratio`),
#' 2) a conversion from "net à payer" to "net imposable" (`net_imposable_ratio`),
#' 3) either the 10% allowance (with min/max) or a fixed `frais_reels` deduction,
#' 4) a progressive income tax schedule with quotient familial (`parts` and `bareme`).
#'
#' This function is designed for **rough budgeting** and scenario comparisons, not
#' a full pay-slip / tax simulator. It does **not** model the prélèvement à la source
#' mechanics, decote, credits/reductions, special contributions, caps, or household
#' specificities beyond `parts`.
#'
#' @param brut_annuel Numeric scalar >= 0. Annual gross salary in euros.
#'
#' @param cotis_salariales_rate Numeric scalar in \[0, 1). Employee contributions rate used
#'   to approximate `net_a_payer_avant_pas = brut_annuel * (1 - cotis_salariales_rate)`.
#'   Ignored if `net_sur_brut_ratio` is provided.
#' @param net_sur_brut_ratio Numeric scalar. If provided, overrides `cotis_salariales_rate` and
#'   directly sets `net_a_payer_avant_pas = brut_annuel * net_sur_brut_ratio`.
#'
#' @param net_imposable_ratio Numeric scalar. Conversion factor from "net à payer (avant PAS)"
#'   to "net imposable". Typically calibrated from a payslip (e.g., `net imposable / net à payer`).
#'
#' @param abattement10 Logical. If `TRUE` and `frais_reels` is `NULL`, applies the 10% allowance.
#' @param abatt_rate Numeric scalar in \[0, 1\]. Rate for the 10% allowance (default 0.10).
#' @param abatt_min Numeric scalar >= 0. Minimum allowance in euros.
#' @param abatt_max Numeric scalar >= 0. Maximum allowance in euros.
#' @param frais_reels Numeric scalar >= 0 or `NULL`. If non-`NULL`, uses this fixed euro deduction
#'   instead of the 10% allowance.
#'
#' @param parts Numeric scalar > 0. Number of fiscal parts for quotient familial.
#' @param bareme List as returned by [make_bareme()]. Default is a common recent French schedule.
#'
#' @param hours_week Numeric scalar > 0. Weekly working hours used only for the hourly conversion.
#' @param weeks_year Numeric scalar > 0. Working weeks per year used only for the hourly conversion.
#'
#' @return A `data.frame` with one row and the following columns:
#' \describe{
#'   \item{cotis_rate_effectif}{Effective contribution rate used.}
#'   \item{mode_frais}{Which deduction mode was applied: `"frais_reels"`, `"abattement_10%"`, or `"aucun"`.}
#'   \item{brut_annuel}{Input gross annual salary.}
#'   \item{net_a_payer_avant_pas}{Estimated net pay before withholding (approx).}
#'   \item{net_imposable}{Estimated net taxable income used as base for deductions.}
#'   \item{deduction_frais}{Applied deduction amount (allowance or frais réels).}
#'   \item{revenu_imposable}{Taxable income after deductions.}
#'   \item{parts}{Fiscal parts used.}
#'   \item{ir_estime}{Estimated income tax (simplified).}
#'   \item{net_apres_impot}{Estimated net after income tax.}
#'   \item{net_mensuel_apres_impot}{Monthly equivalent.}
#'   \item{net_horaire_apres_impot}{Hourly equivalent based on `hours_week * weeks_year`.}
#' }
#'
#' @examples
#' # Basic usage with defaults
#' brut_to_net_final(brut_annuel = 80000, parts = 1)
#'
#' # Provide a custom barème explicitly
#' bareme_2025 <- make_bareme(
#'   breaks = c(0, 11497, 29315, 83823, 180294),
#'   rates  = c(0.00, 0.11, 0.30, 0.41, 0.45)
#' )
#' brut_to_net_final(
#'   brut_annuel = 70000,
#'   cotis_salariales_rate = 0.21,
#'   parts = 1,
#'   bareme = bareme_2025,
#'   hours_week = 35,
#'   abatt_min = 504,
#'   abatt_max = 14426
#' )
#'
#' # Use frais réels instead of the 10% allowance
#' brut_to_net_final(
#'   brut_annuel = 65000,
#'   parts = 1,
#'   frais_reels = 2500
#' )
#'
#' @export
brut_to_net_final <- function(
        brut_annuel,
        cotis_salariales_rate = 0.192,
        net_sur_brut_ratio = NULL,
        net_imposable_ratio = 1.03526,
        abattement10 = TRUE,
        abatt_rate = 0.10,
        abatt_min  = 504,
        abatt_max  = 14426,
        frais_reels = NULL,
        parts = 1,
        bareme = make_bareme(
            breaks = c(0, 11497, 29315, 83823, 180294),
            rates  = c(0.00, 0.11, 0.30, 0.41, 0.45)
        ),
        hours_week = 40,
        weeks_year = 52
) {
    stopifnot(is.numeric(brut_annuel), length(brut_annuel) == 1, brut_annuel >= 0)

    if (!is.null(net_sur_brut_ratio)) {
        stopifnot(is.numeric(net_sur_brut_ratio), length(net_sur_brut_ratio) == 1)
        stopifnot(net_sur_brut_ratio > 0, net_sur_brut_ratio < 1.5)
        net_a_payer_avant_pas <- brut_annuel * net_sur_brut_ratio
        cotis_rate_effectif <- 1 - net_sur_brut_ratio
    } else {
        stopifnot(is.numeric(cotis_salariales_rate), length(cotis_salariales_rate) == 1)
        stopifnot(cotis_salariales_rate >= 0, cotis_salariales_rate < 1)
        net_a_payer_avant_pas <- brut_annuel * (1 - cotis_salariales_rate)
        cotis_rate_effectif <- cotis_salariales_rate
    }

    stopifnot(is.numeric(net_imposable_ratio), length(net_imposable_ratio) == 1)
    stopifnot(net_imposable_ratio > 0.9, net_imposable_ratio < 1.3)
    net_imposable <- net_a_payer_avant_pas * net_imposable_ratio

    if (!is.null(frais_reels)) {
        stopifnot(is.numeric(frais_reels), length(frais_reels) == 1, frais_reels >= 0)
        revenu_imposable <- max(0, net_imposable - frais_reels)
        deduction_frais  <- min(net_imposable, frais_reels)
        mode_frais <- "frais_reels"
    } else if (isTRUE(abattement10)) {
        revenu_imposable <- apply_abattement10(
            net_imposable,
            rate = abatt_rate,
            min_ded = abatt_min,
            max_ded = abatt_max
        )
        deduction_frais <- net_imposable - revenu_imposable
        mode_frais <- "abattement_10%"
    } else {
        revenu_imposable <- net_imposable
        deduction_frais  <- 0
        mode_frais <- "aucun"
    }

    ir <- ir_household(revenu_imposable, parts = parts, bareme = bareme)

    net_apres_impot <- net_a_payer_avant_pas - ir
    hours_year <- hours_week * weeks_year

    data.frame(
        cotis_rate_effectif = cotis_rate_effectif,
        mode_frais = mode_frais,
        brut_annuel = brut_annuel,
        net_a_payer_avant_pas = net_a_payer_avant_pas,
        net_imposable = net_imposable,
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
