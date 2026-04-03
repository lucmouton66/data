# =============================================================================
# R/preprocessing.R
# Pipeline de prétraitement des données GPS Catapult OpenField
# Étapes 0 à 4 : contrôle qualité, nettoyage, suppression outliers, filtrage
# =============================================================================

library(dbscan)

# -----------------------------------------------------------------------------
# Lecture robuste d'un CSV Catapult OpenField
# Gère les lignes de métadonnées en début de fichier (lignes # ...)
# Détecte automatiquement le délimiteur (;) et le séparateur décimal (,)
# -----------------------------------------------------------------------------
lire_csv_catapult <- function(chemin) {
  # Lire toutes les lignes brutes pour détecter l'en-tête
  lignes <- readLines(chemin, warn = FALSE, encoding = "UTF-8")

  # Trouver la première ligne contenant "Timestamp" ou "Time"
  candidats <- which(grepl("Timestamp|\\bTime\\b", lignes, ignore.case = TRUE))

  if (length(candidats) == 0) {
    # Repli : première ligne non commentée
    ligne_entete <- which(!grepl("^\\s*#", lignes))[1]
    if (is.na(ligne_entete)) {
      stop("Impossible de trouver la ligne d'en-tête (colonne 'Timestamp' ou 'Time' absente).")
    }
  } else {
    ligne_entete <- candidats[1]
  }

  # Détecter le séparateur (souvent ';' dans les exports Catapult)
  texte_entete <- lignes[ligne_entete]
  sep <- if (grepl(";", texte_entete)) ";" else ","

  # Heuristique : décimale ',' si on voit des motifs "chiffre,chiffre"
  lignes_donnees <- lignes[seq.int(ligne_entete + 1,
                                   min(length(lignes), ligne_entete + 50))]
  dec <- if (any(grepl("\\d,\\d", lignes_donnees))) "," else "."

  # Relire le fichier en sautant les lignes de métadonnées
  df <- read.csv(
    file          = chemin,
    skip          = ligne_entete - 1,
    sep           = sep,
    dec           = dec,
    check.names   = FALSE,
    stringsAsFactors = FALSE,
    na.strings    = c("", "NA", "NaN")
  )

  list(
    data         = df,
    ligne_entete = ligne_entete,
    skip         = ligne_entete - 1,
    sep          = sep,
    dec          = dec,
    colonnes     = names(df)
  )
}

# -----------------------------------------------------------------------------
# ÉTAPE 0 — Contrôle qualité GPS
# Calcule HDOP moyen et #Sats moyen, renvoie un indicateur coloré
# Pas de filtrage automatique : uniquement un indicateur visuel
# -----------------------------------------------------------------------------
etape0_qualite_gps <- function(df) {
  # Identifier les colonnes HDOP et #Sats (insensible à la casse)
  col_hdop <- grep("^hdop$", names(df), ignore.case = TRUE, value = TRUE)[1]
  col_sats <- grep("^#sats$|^sats$|^nb.?sats$", names(df),
                   ignore.case = TRUE, value = TRUE)[1]

  hdop_moyen <- NA_real_
  sats_moyen <- NA_real_

  if (!is.na(col_hdop)) {
    hdop_moyen <- mean(as.numeric(df[[col_hdop]]), na.rm = TRUE)
  }
  if (!is.na(col_sats)) {
    sats_moyen <- mean(as.numeric(df[[col_sats]]), na.rm = TRUE)
  }

  # Déterminer la couleur de l'indicateur
  couleur <- if (!is.na(hdop_moyen) && !is.na(sats_moyen)) {
    if (hdop_moyen < 1.0 && sats_moyen > 14) {
      "success"   # vert
    } else if (hdop_moyen <= 2.0 && sats_moyen >= 10) {
      "warning"   # orange
    } else {
      "danger"    # rouge
    }
  } else {
    "secondary"   # gris si données absentes
  }

  texte_qualite <- if (!is.na(hdop_moyen) && !is.na(sats_moyen)) {
    if (couleur == "success") {
      "Bon signal GPS"
    } else if (couleur == "warning") {
      "Signal GPS moyen"
    } else {
      "Mauvais signal GPS"
    }
  } else {
    "Données GPS absentes"
  }

  list(
    hdop_moyen    = hdop_moyen,
    sats_moyen    = sats_moyen,
    couleur       = couleur,
    texte_qualite = texte_qualite
  )
}

# -----------------------------------------------------------------------------
# ÉTAPE 1 — Prétraitement de base
# - Supprime Velocity ≤ 0 ou Acceleration NA
# - Ne conserve que les séquences de ≥ 4 points consécutifs (> 0.4 s à 10 Hz)
# -----------------------------------------------------------------------------
etape1_pretraitement <- function(df) {
  # Identifier les colonnes clés
  col_vel  <- grep("^velocity$|^vitesse$|^speed$", names(df),
                   ignore.case = TRUE, value = TRUE)[1]
  col_acc  <- grep("^acceleration$|^accel$", names(df),
                   ignore.case = TRUE, value = TRUE)[1]

  if (is.na(col_vel)) stop("Colonne Velocity introuvable.")
  if (is.na(col_acc)) stop("Colonne Acceleration introuvable.")

  df[[col_vel]] <- as.numeric(df[[col_vel]])
  df[[col_acc]] <- as.numeric(df[[col_acc]])

  # Supprimer les lignes avec Velocity ≤ 0 ou Acceleration manquante
  masque_valide <- df[[col_vel]] > 0 & !is.na(df[[col_acc]])
  df <- df[masque_valide, ]

  if (nrow(df) == 0) return(df)

  # Identifier les séquences de points consécutifs (à 10 Hz, Δt = 0.1 s)
  # On vérifie la continuité via la colonne Seconds si disponible
  col_sec <- grep("^seconds$|^time$", names(df), ignore.case = TRUE, value = TRUE)[1]

  if (!is.na(col_sec)) {
    df[[col_sec]] <- as.numeric(df[[col_sec]])
    delta_t <- diff(df[[col_sec]])
    # Un saut > 0.15 s (1.5 × Δt) indique une rupture de séquence
    rupture <- c(0, ifelse(abs(delta_t - 0.1) > 0.05, 1, 0))
    df$seq_id <- cumsum(rupture) + 1
  } else {
    # Sans colonne Seconds, on considère toutes les lignes comme une seule séquence
    df$seq_id <- 1
  }

  # Compter la longueur de chaque séquence et ne garder que celles ≥ 4 points
  longueurs <- tapply(seq_len(nrow(df)), df$seq_id, length)
  seq_valides <- as.integer(names(longueurs[longueurs >= 4]))
  df <- df[df$seq_id %in% seq_valides, ]
  df$seq_id <- NULL

  rownames(df) <- NULL
  df
}

# -----------------------------------------------------------------------------
# ÉTAPE 2 — Suppression outliers : règle des 3 sigma + droite triangulaire
# Retourne la liste : données nettoyées + données rejetées + flag session supprimée
# -----------------------------------------------------------------------------
etape2_outliers_sigma <- function(df) {
  col_vel <- grep("^velocity$|^vitesse$|^speed$", names(df),
                  ignore.case = TRUE, value = TRUE)[1]
  col_acc <- grep("^acceleration$|^accel$", names(df),
                  ignore.case = TRUE, value = TRUE)[1]

  v <- df[[col_vel]]
  a <- df[[col_acc]]

  # Calcul des seuils 3-sigma
  mu_v  <- mean(v, na.rm = TRUE);  sigma_v  <- sd(v, na.rm = TRUE)
  mu_a  <- mean(a, na.rm = TRUE);  sigma_a  <- sd(a, na.rm = TRUE)

  seuil_vitesse      <- mu_v + 3 * sigma_v
  seuil_acceleration <- mu_a + 3 * sigma_a

  # Masque de rejet : sigma + droite triangulaire
  rejet_v     <- v > seuil_vitesse
  rejet_a     <- a > seuil_acceleration
  rejet_ligne <- a > seuil_acceleration * (1 - v / seuil_vitesse)

  masque_rejet <- rejet_v | rejet_a | rejet_ligne
  masque_rejet[is.na(masque_rejet)] <- FALSE

  nb_rejetes <- sum(masque_rejet)

  # Marquer les points rejetés pour l'affichage (couleur noire sur le graphique)
  df$statut_sigma <- ifelse(masque_rejet, "rejeté_sigma", "ok")

  df_rejetes  <- df[masque_rejet, ]
  df_propre   <- df[!masque_rejet, ]

  # Seuil de 15 points = 1.5 s de données invalides à 10 Hz (critère qualité session)
  session_supprimee <- nb_rejetes > 15

  list(
    df_propre          = df_propre,
    df_rejetes_sigma   = df_rejetes,
    nb_rejetes         = nb_rejetes,
    session_supprimee  = session_supprimee,
    seuil_vitesse      = seuil_vitesse,
    seuil_acceleration = seuil_acceleration
  )
}

# -----------------------------------------------------------------------------
# ÉTAPE 3 — Suppression outliers : DBSCAN
# eps = 1.13 calculé comme : eps = τ × Smax × Δt = 1.19 × 9.89 × 0.1 ≈ 1.13 m/s
# minPts = 3 (le point lui-même + son précédent + son suivant)
# Points de bruit (cluster = 0) affichés en rouge sur le graphique
# -----------------------------------------------------------------------------
etape3_dbscan <- function(df) {
  col_vel <- grep("^velocity$|^vitesse$|^speed$", names(df),
                  ignore.case = TRUE, value = TRUE)[1]
  col_acc <- grep("^acceleration$|^accel$", names(df),
                  ignore.case = TRUE, value = TRUE)[1]

  # Matrice d'entrée pour DBSCAN : (Velocity, Acceleration)
  mat <- cbind(
    as.numeric(df[[col_vel]]),
    as.numeric(df[[col_acc]])
  )

  # Supprimer les lignes avec NA avant DBSCAN
  lignes_valides <- complete.cases(mat)
  mat_propre <- mat[lignes_valides, , drop = FALSE]

  resultat <- dbscan::dbscan(mat_propre, eps = 1.13, minPts = 3)

  # Reconstruire le vecteur de clusters (taille = nrow(df))
  clusters <- rep(NA_integer_, nrow(df))
  clusters[lignes_valides] <- resultat$cluster

  # Points de bruit : cluster == 0 (ou NA)
  bruit <- !is.na(clusters) & clusters == 0

  df$statut_dbscan <- ifelse(bruit, "bruit_dbscan", "ok")
  df$cluster_dbscan <- clusters

  df_bruit  <- df[bruit, ]
  df_propre <- df[!bruit | is.na(clusters), ]
  # Conserver uniquement les points valides (cluster ≥ 1)
  df_propre <- df[!is.na(clusters) & clusters >= 1, ]

  list(
    df_propre       = df_propre,
    df_bruit_dbscan = df_bruit,
    nb_bruit        = sum(bruit, na.rm = TRUE)
  )
}

# -----------------------------------------------------------------------------
# ÉTAPE 4 — Filtre gaussien sur la Velocity
# Appliqué APRÈS le nettoyage des outliers pour éviter la contamination
# Recalcule l'accélération filtrée comme dérivée de la vitesse filtrée
# Ne remplace PAS la colonne Acceleration originale
# -----------------------------------------------------------------------------
etape4_filtre_gaussien <- function(df, bandwidth = 5) {
  col_vel <- grep("^velocity$|^vitesse$|^speed$", names(df),
                  ignore.case = TRUE, value = TRUE)[1]
  col_sec <- grep("^seconds$|^time$", names(df),
                  ignore.case = TRUE, value = TRUE)[1]

  if (is.na(col_vel)) {
    warning("Colonne Velocity introuvable, filtre gaussien ignoré.")
    return(df)
  }

  v <- as.numeric(df[[col_vel]])
  n <- length(v)

  if (n < 3) return(df)

  # Axe temporel (utiliser Seconds si disponible, sinon indice × 0.1)
  if (!is.na(col_sec)) {
    t <- as.numeric(df[[col_sec]])
  } else {
    t <- seq(0, by = 0.1, length.out = n)
  }

  # Filtre gaussien via ksmooth (noyau gaussien)
  lissage <- tryCatch(
    ksmooth(t, v, kernel = "normal", bandwidth = bandwidth * 0.1),
    error = function(e) NULL
  )

  if (!is.null(lissage) && length(lissage$y) == n) {
    df$velocity_filtre <- lissage$y
  } else {
    # Repli : filtre de convolution gaussien simple
    sigma_pts <- max(1, bandwidth)
    kernel_size <- 2 * sigma_pts + 1
    poids <- dnorm(seq(-sigma_pts, sigma_pts), sd = sigma_pts / 2)
    poids <- poids / sum(poids)
    df$velocity_filtre <- stats::filter(v, poids, sides = 2)
    # Remplir les NA aux bords par la valeur originale
    na_idx <- is.na(df$velocity_filtre)
    df$velocity_filtre[na_idx] <- v[na_idx]
  }

  # Recalcul de l'accélération filtrée comme dérivée (diff / Δt)
  # Stockée dans une colonne séparée, NE remplace PAS Acceleration originale
  dt <- 0.1  # 10 Hz
  acc_filtre <- c(NA_real_, diff(df$velocity_filtre) / dt)
  df$acceleration_filtre <- acc_filtre

  df
}

# -----------------------------------------------------------------------------
# Fonction principale de la pipeline complète (étapes 0 à 4)
# Entrée : data.frame brut issu de lire_csv_catapult()
# Sortie : liste avec données nettoyées + métadonnées de chaque étape
# -----------------------------------------------------------------------------
pipeline_preprocessing <- function(df) {
  # Étape 0 : qualité GPS
  qualite <- etape0_qualite_gps(df)

  # Étape 1 : prétraitement de base
  df1 <- etape1_pretraitement(df)

  if (nrow(df1) == 0) {
    return(list(
      qualite           = qualite,
      df_final          = df1,
      session_supprimee = TRUE,
      message           = "Session vide après l'étape 1 (prétraitement)."
    ))
  }

  # Étape 2 : outliers sigma
  res2 <- etape2_outliers_sigma(df1)

  if (res2$session_supprimee) {
    return(list(
      qualite           = qualite,
      df_final          = data.frame(),
      df_sigma          = res2$df_rejetes_sigma,
      session_supprimee = TRUE,
      nb_rejetes_sigma  = res2$nb_rejetes,
      message           = paste0("Session supprimée : ", res2$nb_rejetes,
                                 " points rejetés (> 15) à l'étape 2.")
    ))
  }

  # Étape 3 : DBSCAN
  res3 <- etape3_dbscan(res2$df_propre)

  # Étape 4 : filtre gaussien sur la vitesse (après nettoyage)
  df_final <- etape4_filtre_gaussien(res3$df_propre)

  list(
    qualite              = qualite,
    df_final             = df_final,
    df_rejetes_sigma     = res2$df_rejetes_sigma,
    df_bruit_dbscan      = res3$df_bruit_dbscan,
    nb_rejetes_sigma     = res2$nb_rejetes,
    nb_bruit_dbscan      = res3$nb_bruit,
    seuil_vitesse        = res2$seuil_vitesse,
    seuil_acceleration   = res2$seuil_acceleration,
    session_supprimee    = FALSE,
    message              = "Prétraitement terminé avec succès."
  )
}
