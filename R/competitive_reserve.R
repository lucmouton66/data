# =============================================================================
# R/competitive_reserve.R
# Calcul de la réserve compétitive (Competitive Reserve)
# Compare le profil AS individuel au profil de référence de l'élite
# =============================================================================

# -----------------------------------------------------------------------------
# Calcul de la réserve compétitive
# La réserve compétitive représente la marge entre le profil actuel du joueur
# et un profil de référence élite (défini par l'utilisateur ou des normes publiées)
# Elle est exprimée en % de la vitesse maximale théorique (S0)
# et de l'accélération maximale théorique (A0)
# -----------------------------------------------------------------------------
calculer_reserve_competitive <- function(resultats_joueur,
                                         A0_reference,
                                         S0_reference) {
  # Vérifications des entrées
  if (is.null(resultats_joueur) || !is.list(resultats_joueur)) {
    stop("resultats_joueur doit être une liste de résultats AS.")
  }

  A0_joueur <- resultats_joueur$A0_moyen
  S0_joueur <- resultats_joueur$S0_moyen

  if (is.na(A0_joueur) || is.na(S0_joueur)) {
    warning("Paramètres AS du joueur manquants, réserve compétitive non calculable.")
    return(NULL)
  }

  # Réserve compétitive en accélération (différence absolue et relative)
  reserve_A0_abs     <- A0_reference - A0_joueur
  reserve_A0_pct     <- (reserve_A0_abs / A0_reference) * 100

  # Réserve compétitive en vitesse maximale (différence absolue et relative)
  reserve_S0_abs     <- S0_reference - S0_joueur
  reserve_S0_pct     <- (reserve_S0_abs / S0_reference) * 100

  # Indice de déficit global (moyenne des deux déficits relatifs)
  deficit_global     <- (abs(reserve_A0_pct) + abs(reserve_S0_pct)) / 2

  # Interprétation qualitative
  interpretation <- if (deficit_global < 5) {
    "Niveau élite — profil proche de la référence"
  } else if (deficit_global < 15) {
    "Bon niveau — marge de progression modérée"
  } else if (deficit_global < 30) {
    "Niveau intermédiaire — axe de travail identifié"
  } else {
    "Déficit important — programme spécifique recommandé"
  }

  list(
    A0_joueur      = A0_joueur,
    S0_joueur      = S0_joueur,
    A0_reference   = A0_reference,
    S0_reference   = S0_reference,
    reserve_A0_abs = round(reserve_A0_abs, 3),
    reserve_A0_pct = round(reserve_A0_pct, 2),
    reserve_S0_abs = round(reserve_S0_abs, 3),
    reserve_S0_pct = round(reserve_S0_pct, 2),
    deficit_global = round(deficit_global, 2),
    interpretation = interpretation
  )
}

# -----------------------------------------------------------------------------
# Calcul de la réserve compétitive pour un groupe de joueurs
# Retourne un data.frame avec une ligne par joueur
# -----------------------------------------------------------------------------
reserve_competitive_groupe <- function(df_resultats,
                                       A0_reference,
                                       S0_reference,
                                       col_joueur = "joueur") {
  if (!col_joueur %in% names(df_resultats)) {
    stop(paste("Colonne joueur introuvable :", col_joueur))
  }

  joueurs <- unique(df_resultats[[col_joueur]])

  res_liste <- lapply(joueurs, function(j) {
    df_j <- df_resultats[df_resultats[[col_joueur]] == j, ]

    # Utiliser la dernière session disponible pour ce joueur
    res_j <- list(
      A0_moyen = mean(df_j$A0, na.rm = TRUE),
      S0_moyen = mean(df_j$S0, na.rm = TRUE)
    )

    rc <- tryCatch(
      calculer_reserve_competitive(res_j, A0_reference, S0_reference),
      error = function(e) NULL
    )

    if (is.null(rc)) return(NULL)

    data.frame(
      joueur         = j,
      A0             = round(res_j$A0_moyen, 3),
      S0             = round(res_j$S0_moyen, 3),
      reserve_A0_pct = rc$reserve_A0_pct,
      reserve_S0_pct = rc$reserve_S0_pct,
      deficit_global = rc$deficit_global,
      interpretation = rc$interpretation,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, res_liste[!sapply(res_liste, is.null)])
}
