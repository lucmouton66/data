# =============================================================================
# R/statistics.R
# Étapes 7 à 9 : statistiques descriptives, ANOVA/Tukey, fiabilité ICC/SEM
# =============================================================================

library(dplyr)
library(tidyr)

# -----------------------------------------------------------------------------
# Étape 7 — Statistiques descriptives par groupe (position ou type de session)
# Renvoie un tableau résumé : moyenne ± SD des paramètres AS
# -----------------------------------------------------------------------------
etape7_stats_descriptives <- function(df_resultats, grouper_par = "position") {
  if (!grouper_par %in% names(df_resultats)) {
    warning(paste("Colonne de regroupement introuvable :", grouper_par))
    return(NULL)
  }

  # Paramètres AS à résumer
  params <- c("A0", "S0", "ASslope", "r2")
  params <- params[params %in% names(df_resultats)]

  if (length(params) == 0) {
    warning("Aucun paramètre AS trouvé dans df_resultats.")
    return(NULL)
  }

  # Calcul des statistiques groupées
  df_resultats %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouper_par))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(params),
        list(
          moy = ~ mean(.x, na.rm = TRUE),
          sd  = ~ sd(.x, na.rm = TRUE),
          n   = ~ sum(!is.na(.x))
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}

# -----------------------------------------------------------------------------
# Étape 8 — ANOVA à un facteur + test post-hoc de Tukey
# Pour comparer les paramètres AS entre groupes (positions ou types de session)
# -----------------------------------------------------------------------------
etape8_anova_tukey <- function(df_resultats, parametre = "A0",
                               grouper_par = "position") {
  # Vérifications
  if (!parametre %in% names(df_resultats)) {
    stop(paste("Paramètre introuvable :", parametre))
  }
  if (!grouper_par %in% names(df_resultats)) {
    stop(paste("Colonne de regroupement introuvable :", grouper_par))
  }

  # Préparer les données
  df_anova <- df_resultats[, c(grouper_par, parametre)]
  df_anova <- df_anova[!is.na(df_anova[[parametre]]), ]
  df_anova[[grouper_par]] <- as.factor(df_anova[[grouper_par]])

  if (nlevels(df_anova[[grouper_par]]) < 2) {
    return(list(
      anova   = NULL,
      tukey   = NULL,
      message = "Au moins 2 groupes requis pour l'ANOVA."
    ))
  }

  # Formule dynamique
  formule <- as.formula(paste(parametre, "~", grouper_par))

  # ANOVA
  modele_aov <- tryCatch(
    aov(formule, data = df_anova),
    error = function(e) { warning(e$message); NULL }
  )

  if (is.null(modele_aov)) {
    return(list(anova = NULL, tukey = NULL, message = "Échec de l'ANOVA."))
  }

  res_anova <- summary(modele_aov)

  # Test de Tukey post-hoc
  res_tukey <- tryCatch(
    TukeyHSD(modele_aov),
    error = function(e) { warning(e$message); NULL }
  )

  list(
    anova   = res_anova,
    tukey   = res_tukey,
    modele  = modele_aov,
    message = "ANOVA et Tukey calculés avec succès."
  )
}

# -----------------------------------------------------------------------------
# Étape 9 — Fiabilité : ICC (Intraclass Correlation Coefficient) et SEM
# Mesure la fiabilité test-retest des paramètres AS
# Utilise le package irr (ICC à 2 voies, accord absolu)
# -----------------------------------------------------------------------------
etape9_fiabilite <- function(df_resultats, parametre = "A0",
                             col_joueur = "joueur", col_session = "session") {
  # Vérifications
  cols_requises <- c(parametre, col_joueur, col_session)
  manquantes <- cols_requises[!cols_requises %in% names(df_resultats)]
  if (length(manquantes) > 0) {
    stop(paste("Colonnes manquantes pour la fiabilité :", paste(manquantes, collapse = ", ")))
  }

  # Mise en forme large : joueurs en lignes, sessions en colonnes
  df_large <- tryCatch({
    df_resultats[, cols_requises] %>%
      tidyr::pivot_wider(
        names_from  = col_session,
        values_from = parametre,
        id_cols     = col_joueur
      )
  }, error = function(e) {
    stop(paste("Impossible de pivoter les données pour la fiabilité :", e$message))
  })

  # Garder uniquement les colonnes numériques (sessions)
  cols_num <- names(df_large)[sapply(df_large, is.numeric)]
  mat <- df_large[, cols_num, drop = FALSE]

  # Supprimer les joueurs avec des NA (au moins une session manquante)
  mat <- mat[complete.cases(mat), , drop = FALSE]

  if (nrow(mat) < 3 || ncol(mat) < 2) {
    return(list(
      icc     = NA_real_,
      icc_lb  = NA_real_,
      icc_ub  = NA_real_,
      sem     = NA_real_,
      cv      = NA_real_,
      message = "Données insuffisantes pour calculer l'ICC (min 3 sujets, 2 sessions)."
    ))
  }

  # Calcul de l'ICC via irr (ICC2,1 : deux voies, accord absolu, mesure unique)
  icc_res <- tryCatch({
    if (requireNamespace("irr", quietly = TRUE)) {
      irr::icc(mat, model = "twoway", type = "agreement", unit = "single")
    } else {
      # Calcul manuel de l'ICC si irr n'est pas disponible
      calcul_icc_manuel(mat)
    }
  }, error = function(e) {
    warning(paste("Erreur ICC :", e$message))
    NULL
  })

  if (is.null(icc_res)) {
    return(list(icc = NA_real_, icc_lb = NA_real_, icc_ub = NA_real_,
                sem = NA_real_, cv = NA_real_, message = "Erreur lors du calcul ICC."))
  }

  # Extraire les valeurs ICC
  if (inherits(icc_res, "icc")) {
    icc_val <- icc_res$value
    icc_lb  <- icc_res$lbound
    icc_ub  <- icc_res$ubound
  } else {
    icc_val <- icc_res$icc
    icc_lb  <- icc_res$lb
    icc_ub  <- icc_res$ub
  }

  # SEM (Standard Error of Measurement) = SD × sqrt(1 - ICC)
  sd_total <- sd(as.vector(as.matrix(mat)), na.rm = TRUE)
  sem      <- if (!is.na(icc_val)) sd_total * sqrt(1 - icc_val) else NA_real_

  # CV% (Coefficient of Variation) = SEM / moyenne globale × 100
  moy_global <- mean(as.vector(as.matrix(mat)), na.rm = TRUE)
  cv <- if (!is.na(sem) && moy_global != 0) (sem / abs(moy_global)) * 100 else NA_real_

  list(
    icc     = round(icc_val, 3),
    icc_lb  = round(icc_lb, 3),
    icc_ub  = round(icc_ub, 3),
    sem     = round(sem, 3),
    cv      = round(cv, 2),
    n_sujets = nrow(mat),
    n_sessions = ncol(mat),
    message = "Fiabilité calculée avec succès."
  )
}

# -----------------------------------------------------------------------------
# Calcul manuel de l'ICC (deux voies, accord absolu) si irr est absent
# Basé sur la méthode ANOVA à deux facteurs sans interaction
# -----------------------------------------------------------------------------
calcul_icc_manuel <- function(mat) {
  n <- nrow(mat)   # nombre de sujets
  k <- ncol(mat)   # nombre de sessions

  # ANOVA à deux facteurs
  SS_total  <- sum((mat - mean(as.matrix(mat)))^2)
  moy_sujet <- rowMeans(mat)
  moy_sess  <- colMeans(mat)
  moy_total <- mean(as.matrix(mat))

  SS_sujets  <- k * sum((moy_sujet - moy_total)^2)
  SS_sessions <- n * sum((moy_sess - moy_total)^2)
  SS_erreur  <- SS_total - SS_sujets - SS_sessions

  MS_sujets  <- SS_sujets / (n - 1)
  MS_erreur  <- SS_erreur / ((n - 1) * (k - 1))
  MS_sessions <- SS_sessions / (k - 1)

  # ICC2,1 accord absolu
  icc_val <- (MS_sujets - MS_erreur) /
    (MS_sujets + (k - 1) * MS_erreur + k * (MS_sessions - MS_erreur) / n)

  # Intervalles de confiance approximatifs (alpha = 0.05)
  F_val <- MS_sujets / MS_erreur
  df1   <- n - 1
  df2   <- (n - 1) * (k - 1)
  F_lb  <- F_val / qf(0.975, df1, df2)
  F_ub  <- F_val * qf(0.975, df2, df1)

  icc_lb <- (F_lb - 1) / (F_lb + k - 1)
  icc_ub <- (F_ub - 1) / (F_ub + k - 1)

  list(icc = icc_val, lb = max(0, icc_lb), ub = min(1, icc_ub))
}
