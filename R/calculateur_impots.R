# ============================================================
# France: estimateur "brut -> net final" (job-agnostique)
# - Entrées minimales: brut_annuel
# - L'utilisateur peut définir:
#   * taux de cotisations salariales (ou ratio net/brut)
#   * règles d'abattement 10% (min/max) ou frais réels
#   * barème IR (tranches + taux)
#   * nombre de parts (quotient familial)
#
# Références (pour des valeurs par défaut actuelles) :
# - Tranches/taux (barème progressif "IR 2025"): impots.gouv.fr + service-public.gouv.fr
#   https://www.impots.gouv.fr/particulier/questions/comment-calculer-mon-taux-dimposition-dapres-le-bareme-progressif-de-limpot
#   https://www.service-public.fr/particuliers/vosdroits/F1419
# - Abattement 10% (revenus 2025: min 504€, max 14 426€): impots.gouv.fr (aide simulateur 2026)
#   https://simulateur-ir-ifi.impots.gouv.fr/calcul_impot/2026/aides/frais.htm
# ============================================================

# ---------- 1) Barème IR : helper ----------
make_bareme <- function(breaks, rates) {
  # breaks: numeric vector of lower bounds per bracket, starting at 0
  # rates : numeric vector of marginal rates per bracket (same length as breaks)
  stopifnot(is.numeric(breaks), is.numeric(rates))
  stopifnot(length(breaks) == length(rates))
  stopifnot(breaks[1] == 0)
  stopifnot(all(diff(breaks) > 0))
  stopifnot(all(rates >= 0), all(rates <= 1))
  list(breaks = breaks, rates = rates)
}

# Progressive tax for ONE part
ir_one_part <- function(income, bareme) {
  income <- max(0, income)
  b <- bareme$breaks
  r <- bareme$rates
  # upper bounds are next break, last is +Inf
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

# Household IR with quotient familial (simple version)
ir_household <- function(taxable_income, parts = 1, bareme) {
  stopifnot(parts > 0)
  q <- taxable_income / parts
  ir_one_part(q, bareme) * parts
}

# ---------- 2) Abattement 10% salarié ----------
apply_abattement10 <- function(net_income, rate = 0.10, min_ded = 504, max_ded = 14426) {
  ded <- net_income * rate
  ded <- max(min_ded, min(max_ded, ded))
  net_income - ded
}

# ---------- 3) Brut -> net final ----------
# ---------- 3) Brut -> net final (PATCHED) ----------
brut_to_net_final <- function(
        brut_annuel,
        # Cotisations: choisir UNE des deux options ci-dessous
        cotis_salariales_rate = 0.192,   # brut -> net_a_payer_avant_pas = brut * (1 - rate)
        net_sur_brut_ratio = NULL,      # ex: 0.808 (si fourni, écrase cotis_salariales_rate)

        # IMPORTANT (nouveau) : passer de "net à payer" -> "net imposable"
        # Calibrage sur ton bulletin: 4048.41 / 3910.50 = 1.03526
        net_imposable_ratio = 1.03526,

        # Base imposable
        abattement10 = TRUE,
        abatt_rate = 0.10,
        abatt_min  = 504,
        abatt_max  = 14426,
        frais_reels = NULL,             # si non-NULL, remplace l'abattement (déduction fixe en €)

        # IR
        parts = 1,
        bareme = make_bareme(
            breaks = c(0, 11497, 29315, 83823, 180294),
            rates  = c(0.00, 0.11, 0.30, 0.41, 0.45)
        ),

        # Conversions
        hours_week = 40,
        weeks_year = 52
) {
    stopifnot(is.numeric(brut_annuel), length(brut_annuel) == 1, brut_annuel >= 0)

    # 1) Net à payer avant PAS (approx)
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

    # 2) Net imposable (base utilisée pour le PAS/IR en paie française)
    stopifnot(is.numeric(net_imposable_ratio), length(net_imposable_ratio) == 1)
    stopifnot(net_imposable_ratio > 0.9, net_imposable_ratio < 1.3)
    net_imposable <- net_a_payer_avant_pas * net_imposable_ratio

    # 3) Revenu imposable après frais (abattement 10% ou frais réels)
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

    # 4) IR (simple, sans décote, sans réductions/crédits)
    ir <- ir_household(revenu_imposable, parts = parts, bareme = bareme)

    # 5) Net final estimé
    # On soustrait l'IR du "net à payer avant PAS"
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

# ---------- 4) Exemple d'usage ----------
# a) Tu connais ton brut annuel et tu mets juste un PASSE-PARTOUT:
res <- brut_to_net_final(
  brut_annuel = 80000,
  #cotis_salariales_rate = 0.23,   # contributions sociales et similaires
  parts = 1
)
print(res)

# b) Tu veux définir TON barème (tranches + taux) explicitement:
# Exemple "barème IR 2025" (revenus 2024) par part:
bareme_2025 <- make_bareme(
  breaks = c(0, 11497, 29315, 83823, 180294),
  rates  = c(0.00, 0.11, 0.30, 0.41, 0.45)
)

res2 <- brut_to_net_final(
  brut_annuel = 80000,
  cotis_salariales_rate = 0.23,
  parts = 1,
  bareme = bareme_2025,
  hours_week = 35,
  abatt_min = 504, abatt_max = 14426  # revenus 2025 (déclaration 2026)
)
print(res2)
