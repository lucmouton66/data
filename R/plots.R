# =============================================================================
# R/plots.R
# Fonctions de visualisation ggplot2 pour le profil Accélération-Vitesse
# Toutes les fonctions retournent un objet ggplot avec theme_minimal()
# =============================================================================

library(ggplot2)

# -----------------------------------------------------------------------------
# Graphique principal du profil AS individuel
# Affiche :
#   - Points nettoyés (fond gris clair)
#   - Points rejetés sigma en NOIR
#   - Points bruit DBSCAN en ROUGE
#   - Points sélectionnés pour la régression en ROUGE (bordure)
#   - Point d'accélération maximale en VERT
#   - 19 droites quantiles en gris pointillé
#   - Droite moyenne finale en couleur pleine
#   - Annotation : A0, S0, r²
# -----------------------------------------------------------------------------
plot_profil_as <- function(tous_points,
                           df_rejetes_sigma  = NULL,
                           df_bruit_dbscan   = NULL,
                           res5              = NULL,
                           res6              = NULL,
                           titre             = "Profil Accélération-Vitesse",
                           couleur_ligne     = "#1f77b4") {
  col_vel <- grep("^velocity$|^vitesse$|^speed$|^v$", names(tous_points),
                  ignore.case = TRUE, value = TRUE)[1]
  col_acc <- grep("^acceleration$|^accel$|^a$", names(tous_points),
                  ignore.case = TRUE, value = TRUE)[1]

  # Si les colonnes "v" et "a" existent directement (format df_selection)
  if (is.na(col_vel) && "v" %in% names(tous_points)) col_vel <- "v"
  if (is.na(col_acc) && "a" %in% names(tous_points)) col_acc <- "a"

  if (is.na(col_vel) || is.na(col_acc)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                               label = "Données insuffisantes pour le graphique") +
             theme_minimal())
  }

  df_plot <- data.frame(
    v = as.numeric(tous_points[[col_vel]]),
    a = as.numeric(tous_points[[col_acc]])
  )
  df_plot <- df_plot[!is.na(df_plot$v) & !is.na(df_plot$a), ]

  p <- ggplot() +
    # Points de fond (nettoyés)
    geom_point(data = df_plot, aes(x = v, y = a),
               colour = "grey70", alpha = 0.4, size = 0.8) +
    theme_minimal(base_size = 13) +
    labs(
      title = titre,
      x     = "Vitesse (m/s)",
      y     = "Accélération (m/s²)"
    )

  # Points rejetés par la règle 3-sigma → NOIR
  if (!is.null(df_rejetes_sigma) && nrow(df_rejetes_sigma) > 0) {
    col_v_s <- grep("^velocity$|^vitesse$|^speed$", names(df_rejetes_sigma),
                    ignore.case = TRUE, value = TRUE)[1]
    col_a_s <- grep("^acceleration$|^accel$", names(df_rejetes_sigma),
                    ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(col_v_s) && !is.na(col_a_s)) {
      df_sigma <- data.frame(
        v = as.numeric(df_rejetes_sigma[[col_v_s]]),
        a = as.numeric(df_rejetes_sigma[[col_a_s]])
      )
      p <- p + geom_point(data = df_sigma, aes(x = v, y = a),
                          colour = "black", alpha = 0.7, size = 1,
                          shape = 4)
    }
  }

  # Points bruit DBSCAN → ROUGE
  if (!is.null(df_bruit_dbscan) && nrow(df_bruit_dbscan) > 0) {
    col_v_d <- grep("^velocity$|^vitesse$|^speed$", names(df_bruit_dbscan),
                    ignore.case = TRUE, value = TRUE)[1]
    col_a_d <- grep("^acceleration$|^accel$", names(df_bruit_dbscan),
                    ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(col_v_d) && !is.na(col_a_d)) {
      df_dbscan <- data.frame(
        v = as.numeric(df_bruit_dbscan[[col_v_d]]),
        a = as.numeric(df_bruit_dbscan[[col_a_d]])
      )
      p <- p + geom_point(data = df_dbscan, aes(x = v, y = a),
                          colour = "red", alpha = 0.7, size = 1.2,
                          shape = 1)
    }
  }

  # Points sélectionnés pour la régression → ROUGE plein
  if (!is.null(res5)) {
    df_sel <- res5$df_selection
    if (!is.null(df_sel) && nrow(df_sel) > 0) {
      p <- p + geom_point(data = df_sel, aes(x = v, y = a),
                          colour = "red", size = 2, alpha = 0.9)
    }

    # Point d'accélération maximale → VERT
    if (!is.null(res5$v_max_acc) && !is.null(res5$a_max_acc)) {
      df_max <- data.frame(v = res5$v_max_acc, a = res5$a_max_acc)
      p <- p + geom_point(data = df_max, aes(x = v, y = a),
                          colour = "green3", size = 4, shape = 17)
    }
  }

  # 19 droites quantiles en gris pointillé + droite moyenne finale
  if (!is.null(res6)) {
    v_seq <- seq(res6$v_range[1], res6$v_range[2], length.out = 100)

    # 19 droites grises pointillées
    for (i in seq_len(nrow(res6$df_taus))) {
      A0_i    <- res6$df_taus$A0[i]
      slope_i <- res6$df_taus$slope[i]
      if (!is.na(A0_i) && !is.na(slope_i)) {
        df_ligne <- data.frame(v = v_seq, a = A0_i + slope_i * v_seq)
        p <- p + geom_line(data = df_ligne, aes(x = v, y = a),
                           colour = "grey50", linetype = "dashed",
                           alpha = 0.5, linewidth = 0.4)
      }
    }

    # Droite moyenne finale (colorée et pleine)
    df_moy <- data.frame(
      v = v_seq,
      a = res6$A0_moyen + res6$slope_moyen * v_seq
    )
    p <- p + geom_line(data = df_moy, aes(x = v, y = a),
                       colour = couleur_ligne, linewidth = 1.5)

    # Annotation avec les paramètres AS
    label_annot <- sprintf(
      "A0 = %.2f ± %.2f | S0 = %.2f ± %.2f | r² = %.3f",
      res6$A0_moyen, res6$A0_sd,
      res6$S0_moyen, res6$S0_sd,
      res6$r2
    )

    p <- p + annotate(
      "text",
      x     = min(v_seq) + diff(range(v_seq)) * 0.02,
      y     = max(df_plot$a, na.rm = TRUE) * 0.95,
      label = label_annot,
      hjust = 0, vjust = 1,
      size  = 3.5,
      colour = couleur_ligne,
      fontface = "bold"
    )
  }

  p
}

# -----------------------------------------------------------------------------
# Graphique de comparaison des profils AS entre joueurs / groupes
# Superpose les droites moyennes de plusieurs profils
# -----------------------------------------------------------------------------
plot_comparaison_profils <- function(liste_resultats,
                                     noms_groupes = NULL,
                                     titre = "Comparaison des profils AS") {
  if (is.null(noms_groupes)) {
    noms_groupes <- paste("Groupe", seq_along(liste_resultats))
  }

  # Palette de couleurs distinctes
  couleurs <- scales::hue_pal()(length(liste_resultats))

  p <- ggplot() +
    theme_minimal(base_size = 13) +
    labs(title = titre, x = "Vitesse (m/s)", y = "Accélération (m/s²)",
         colour = "Groupe")

  for (i in seq_along(liste_resultats)) {
    res6 <- liste_resultats[[i]]
    if (is.null(res6) || is.null(res6$v_range)) next

    v_seq <- seq(res6$v_range[1], res6$v_range[2], length.out = 100)
    df_ligne <- data.frame(
      v      = v_seq,
      a      = res6$A0_moyen + res6$slope_moyen * v_seq,
      groupe = noms_groupes[i]
    )

    p <- p + geom_line(data = df_ligne, aes(x = v, y = a, colour = groupe),
                       linewidth = 1.5)
  }

  p + scale_colour_manual(values = setNames(couleurs, noms_groupes))
}

# -----------------------------------------------------------------------------
# Graphique en boîte (boxplot) pour comparer les paramètres AS par groupe
# -----------------------------------------------------------------------------
plot_boxplot_parametre <- function(df_resultats,
                                   parametre  = "A0",
                                   grouper_par = "position",
                                   titre       = NULL) {
  if (!parametre %in% names(df_resultats) ||
      !grouper_par %in% names(df_resultats)) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "Données manquantes") +
             theme_minimal())
  }

  if (is.null(titre)) {
    titre <- paste("Distribution de", parametre, "par", grouper_par)
  }

  df_plot <- df_resultats[, c(grouper_par, parametre)]
  names(df_plot) <- c("groupe", "valeur")
  df_plot <- df_plot[!is.na(df_plot$valeur), ]

  ggplot(df_plot, aes(x = groupe, y = valeur, fill = groupe)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
    geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
    theme_minimal(base_size = 13) +
    labs(title = titre, x = grouper_par, y = parametre) +
    theme(legend.position = "none")
}

# -----------------------------------------------------------------------------
# Graphique de fiabilité : graphique de Bland-Altman
# Compare deux mesures répétées d'un même paramètre
# -----------------------------------------------------------------------------
plot_bland_altman <- function(mesure1, mesure2, titre = "Bland-Altman") {
  if (length(mesure1) != length(mesure2)) {
    stop("Les deux vecteurs de mesures doivent avoir la même longueur.")
  }

  moy  <- (mesure1 + mesure2) / 2
  diff <- mesure2 - mesure1

  moy_diff <- mean(diff, na.rm = TRUE)
  sd_diff  <- sd(diff, na.rm = TRUE)
  limite_sup <- moy_diff + 1.96 * sd_diff
  limite_inf <- moy_diff - 1.96 * sd_diff

  df_ba <- data.frame(moy = moy, diff = diff)
  df_ba <- df_ba[!is.na(df_ba$moy) & !is.na(df_ba$diff), ]

  ggplot(df_ba, aes(x = moy, y = diff)) +
    geom_point(colour = "#1f77b4", alpha = 0.7, size = 2) +
    geom_hline(yintercept = moy_diff,   colour = "black",  linewidth = 1) +
    geom_hline(yintercept = limite_sup, colour = "red",    linetype = "dashed") +
    geom_hline(yintercept = limite_inf, colour = "red",    linetype = "dashed") +
    annotate("text", x = max(df_ba$moy, na.rm = TRUE),
             y = limite_sup + sd_diff * 0.1,
             label = sprintf("+1.96 SD = %.3f", limite_sup),
             hjust = 1, colour = "red", size = 3.5) +
    annotate("text", x = max(df_ba$moy, na.rm = TRUE),
             y = limite_inf - sd_diff * 0.1,
             label = sprintf("-1.96 SD = %.3f", limite_inf),
             hjust = 1, colour = "red", size = 3.5) +
    annotate("text", x = max(df_ba$moy, na.rm = TRUE),
             y = moy_diff + sd_diff * 0.1,
             label = sprintf("Biais = %.3f", moy_diff),
             hjust = 1, colour = "black", size = 3.5) +
    theme_minimal(base_size = 13) +
    labs(title = titre,
         x = "Moyenne des deux mesures",
         y = "Différence (mesure 2 - mesure 1)")
}

# -----------------------------------------------------------------------------
# Graphique de la réserve compétitive en radar / barres horizontales
# -----------------------------------------------------------------------------
plot_reserve_competitive <- function(df_reserve,
                                     titre = "Réserve compétitive") {
  if (is.null(df_reserve) || nrow(df_reserve) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "Données insuffisantes") +
             theme_minimal())
  }

  # Préparer les données en format long pour les deux dimensions
  df_long <- data.frame(
    joueur     = rep(df_reserve$joueur, 2),
    dimension  = c(rep("A0 (%)", nrow(df_reserve)),
                   rep("S0 (%)", nrow(df_reserve))),
    reserve    = c(df_reserve$reserve_A0_pct, df_reserve$reserve_S0_pct),
    stringsAsFactors = FALSE
  )

  ggplot(df_long, aes(x = reorder(joueur, reserve), y = reserve,
                      fill = dimension)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
    coord_flip() +
    scale_fill_manual(values = c("A0 (%)" = "#e74c3c", "S0 (%)" = "#3498db")) +
    theme_minimal(base_size = 13) +
    labs(title = titre,
         x = "Joueur",
         y = "Réserve compétitive (%)",
         fill = "Dimension") +
    theme(legend.position = "top")
}
