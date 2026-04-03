# =============================================================================
# R/regression.R
# Étapes 5 et 6 : sélection des points pour la régression quantile
# et ajustement du profil Accélération-Vitesse (AS)
# =============================================================================

library(quantreg)

# -----------------------------------------------------------------------------
# ÉTAPE 5 — Sélection des points pour la régression
# - Point de d'accélération maximale → marqué en vert sur le graphique
# - V_min = vitesse à l'accélération max ; V_max = vitesse maximale
# - Binning en intervalles de 0.2 m/s
# - Sélection des 2 points avec la plus haute accélération par bin
# - Ces points sont affichés en rouge sur le graphique
# -----------------------------------------------------------------------------
etape5_selection_points <- function(df) {
  col_vel <- grep("^velocity$|^vitesse$|^speed$", names(df),
                  ignore.case = TRUE, value = TRUE)[1]
  col_acc <- grep("^acceleration$|^accel$", names(df),
                  ignore.case = TRUE, value = TRUE)[1]

  if (is.na(col_vel) || is.na(col_acc)) {
    stop("Colonnes Velocity ou Acceleration introuvables pour l'étape 5.")
  }

  v <- as.numeric(df[[col_vel]])
  a <- as.numeric(df[[col_acc]])

  # Filtrer les paires (v, a) valides
  valides <- !is.na(v) & !is.na(a)
  v <- v[valides]
  a <- a[valides]

  if (length(v) < 10) {
    stop("Pas assez de points valides pour la sélection (étape 5).")
  }

  # Trouver le point d'accélération maximale (affiché en vert)
  idx_max_acc <- which.max(a)
  v_min       <- v[idx_max_acc]   # vitesse au point d'accélération max
  v_max       <- max(v, na.rm = TRUE)

  # Binning en intervalles de 0.2 m/s entre V_min et V_max
  largeur_bin <- 0.2
  bornes      <- seq(v_min, v_max + largeur_bin, by = largeur_bin)
  bins        <- cut(v, breaks = bornes, include.lowest = TRUE, right = FALSE)

  # Sélectionner les 2 points avec la plus haute accélération par bin
  df_bins <- data.frame(v = v, a = a, bin = bins, stringsAsFactors = FALSE)

  selection <- do.call(rbind, lapply(split(df_bins, df_bins$bin), function(sous_df) {
    if (nrow(sous_df) == 0) return(NULL)
    # Trier par accélération décroissante et prendre les 2 premiers
    sous_df[order(-sous_df$a), ][seq_len(min(2, nrow(sous_df))), ]
  }))

  selection <- selection[!is.na(selection$v), ]

  list(
    df_selection     = selection,
    v_min            = v_min,
    v_max            = v_max,
    idx_max_acc      = idx_max_acc,
    v_max_acc        = v[idx_max_acc],
    a_max_acc        = a[idx_max_acc],
    tous_points      = data.frame(v = v, a = a)
  )
}

# -----------------------------------------------------------------------------
# ÉTAPE 6 — Régression quantile sur les points sélectionnés
# - quantreg::rq() pour τ de 0.05 à 0.95 par pas de 0.05 (19 quantiles)
# - Modèle linéaire : a = A0 + slope × v (droite de régression AS)
# - Extraction de A0, slope, S0 (= -A0/slope), ASslope
# - Moyenne et écart-type de A0, S0, ASslope sur les 19 quantiles
# - Calcul du r² pour la droite moyenne
# - Critère de validation : r² > 0.81
# -----------------------------------------------------------------------------
etape6_regression_quantile <- function(res5) {
  df_sel <- res5$df_selection

  if (nrow(df_sel) < 4) {
    stop("Pas assez de points sélectionnés pour la régression quantile (étape 6).")
  }

  v <- df_sel$v
  a <- df_sel$a

  # Grille de quantiles
  taus <- seq(0.05, 0.95, by = 0.05)

  # Ajustement de la régression quantile pour chaque tau
  resultats_tau <- lapply(taus, function(tau) {
    tryCatch({
      modele <- quantreg::rq(a ~ v, tau = tau, data = df_sel)
      coefs  <- coef(modele)
      A0     <- coefs["(Intercept)"]
      slope  <- coefs["v"]
      # S0 = vitesse pour accélération = 0 : A0 + slope * S0 = 0 → S0 = -A0 / slope
      S0     <- if (!is.na(slope) && abs(slope) > 1e-6) -A0 / slope else NA_real_
      # ASslope = -A0 / S0 = slope (pente de la droite)
      list(tau = tau, A0 = A0, slope = slope, S0 = S0)
    }, error = function(e) {
      list(tau = tau, A0 = NA_real_, slope = NA_real_, S0 = NA_real_)
    })
  })

  # Transformer en data.frame
  df_taus <- data.frame(
    tau   = sapply(resultats_tau, `[[`, "tau"),
    A0    = sapply(resultats_tau, `[[`, "A0"),
    slope = sapply(resultats_tau, `[[`, "slope"),
    S0    = sapply(resultats_tau, `[[`, "S0")
  )

  # Moyennes et écarts-types des paramètres
  A0_moyen      <- mean(df_taus$A0, na.rm = TRUE)
  A0_sd         <- sd(df_taus$A0, na.rm = TRUE)
  slope_moyen   <- mean(df_taus$slope, na.rm = TRUE)
  slope_sd      <- sd(df_taus$slope, na.rm = TRUE)
  S0_moyen      <- mean(df_taus$S0, na.rm = TRUE)
  S0_sd         <- sd(df_taus$S0, na.rm = TRUE)
  ASslope_moyen <- slope_moyen   # ASslope = pente moyenne
  ASslope_sd    <- slope_sd

  # Calcul du r² pour la droite moyenne (A0_moyen + slope_moyen × v)
  a_pred <- A0_moyen + slope_moyen * v
  SS_res <- sum((a - a_pred)^2, na.rm = TRUE)
  SS_tot <- sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)
  r2     <- if (SS_tot > 0) 1 - SS_res / SS_tot else NA_real_

  # Critère de validation
  validation <- !is.na(r2) && r2 > 0.81

  list(
    df_taus       = df_taus,
    A0_moyen      = A0_moyen,
    A0_sd         = A0_sd,
    slope_moyen   = slope_moyen,
    slope_sd      = slope_sd,
    S0_moyen      = S0_moyen,
    S0_sd         = S0_sd,
    ASslope_moyen = ASslope_moyen,
    ASslope_sd    = ASslope_sd,
    r2            = r2,
    validation    = validation,
    taus          = taus,
    v_range       = range(v, na.rm = TRUE)
  )
}
