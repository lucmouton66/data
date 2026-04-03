# =============================================================================
# app.R — Application Shiny : Profil Accélération-Vitesse (AS) Rugby GPS
# Données : exports Catapult OpenField CSV (10 Hz)
# Architecture : fichier principal + modules dans R/
# Commentaires en français
# =============================================================================

# ---- Chargement des bibliothèques ----
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(dplyr)
library(tidyr)

# ---- Chargement des modules applicatifs ----
source("R/preprocessing.R")
source("R/regression.R")
source("R/statistics.R")
source("R/competitive_reserve.R")
source("R/plots.R")

# =============================================================================
# INTERFACE UTILISATEUR
# =============================================================================

ui <- page_navbar(
  title = tags$span(
    tags$i(class = "bi bi-activity", style = "margin-right: 8px;"),
    "Rugby GPS — Profil AS"
  ),
  theme = bs_theme(
    bootswatch = "darkly",
    base_font  = font_google("Inter")
  ),
  fillable = FALSE,

  # Lien Bootstrap Icons
  header = tags$head(
    tags$link(rel = "stylesheet",
              href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css"),
    tags$style(HTML("
      .quality-badge { font-size: 1rem; padding: 6px 14px; border-radius: 20px; font-weight: bold; }
      .sidebar-icon { margin-right: 6px; }
      .param-box { background: #2c2c2c; border-radius: 8px; padding: 12px;
                   margin-bottom: 8px; text-align: center; }
      .param-val { font-size: 1.4rem; font-weight: bold; color: #5bc0de; }
      .param-lbl { font-size: 0.85rem; color: #aaa; }
    "))
  ),

  # ------------------------------------------------------------------
  # Onglet 1 : Import des données
  # ------------------------------------------------------------------
  nav_panel(
    title = tagList(tags$i(class = "bi bi-cloud-upload sidebar-icon"), "Import"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        tags$h5(tags$i(class = "bi bi-file-earmark-spreadsheet"), " Fichier CSV"),
        fileInput(
          inputId  = "csv_file",
          label    = "Dépose ton export Catapult OpenField ici",
          accept   = c(".csv", "text/csv"),
          multiple = TRUE,
          buttonLabel = tags$span(
            tags$i(class = "bi bi-upload"), " Parcourir"
          ),
          placeholder = "Glisser-déposer ou cliquer"
        ),
        hr(),
        tags$h5(tags$i(class = "bi bi-person-badge"), " Métadonnées joueur"),
        textInput("nom_joueur", "Nom du joueur", placeholder = "Ex : Dupont T."),
        selectInput("position", "Poste",
                    choices = c("—", "Pilier", "Talonneur", "Deuxième ligne",
                                "Troisième ligne aile", "Troisième ligne centre",
                                "Demi de mêlée", "Demi d'ouverture",
                                "Centre", "Ailier", "Arrière"),
                    selected = "—"),
        selectInput("type_session", "Type de session",
                    choices = c("—", "Entraînement", "Match", "Pré-saison",
                                "Compétition", "Test de terrain"),
                    selected = "—"),
        hr(),
        actionBttn(
          inputId  = "btn_analyser",
          label    = "Analyser",
          icon     = icon("play-circle"),
          style    = "gradient",
          color    = "primary",
          size     = "md",
          block    = TRUE
        )
      ),
      # Panneau principal de l'onglet Import
      fluidRow(
        column(12,
          h4(tags$i(class = "bi bi-info-circle"), " Résumé du fichier"),
          withSpinner(verbatimTextOutput("resume_fichier"), type = 4)
        )
      ),
      fluidRow(
        column(12,
          br(),
          h4(tags$i(class = "bi bi-table"), " Colonnes détectées"),
          withSpinner(tableOutput("apercu_colonnes"), type = 4)
        )
      ),
      fluidRow(
        column(12,
          br(),
          h4(tags$i(class = "bi bi-eye"), " Aperçu des données (10 premières lignes)"),
          withSpinner(DT::dataTableOutput("apercu_donnees"), type = 4)
        )
      )
    )
  ),

  # ------------------------------------------------------------------
  # Onglet 2 : Profil AS individuel
  # ------------------------------------------------------------------
  nav_panel(
    title = tagList(tags$i(class = "bi bi-graph-up sidebar-icon"), "Profil AS"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        tags$h5(tags$i(class = "bi bi-sliders"), " Qualité GPS"),
        uiOutput("badge_qualite_gps"),
        br(),
        uiOutput("info_preprocessing"),
        hr(),
        downloadButton("dl_plot_as", "Télécharger PNG",
                       icon = icon("image"),
                       class = "btn-sm btn-outline-info w-100")
      ),
      fluidRow(
        column(12,
          withSpinner(plotOutput("plot_as_principal", height = "520px"), type = 4)
        )
      )
    )
  ),

  # ------------------------------------------------------------------
  # Onglet 3 : Résultats individuels
  # ------------------------------------------------------------------
  nav_panel(
    title = tagList(tags$i(class = "bi bi-clipboard-data sidebar-icon"), "Résultats"),
    fluidRow(
      column(3,
        div(class = "param-box",
          div(class = "param-lbl", "A0 (m/s²)"),
          div(class = "param-val", textOutput("val_A0")),
          div(class = "param-lbl", "± SD"),
          textOutput("sd_A0")
        )
      ),
      column(3,
        div(class = "param-box",
          div(class = "param-lbl", "S0 (m/s)"),
          div(class = "param-val", textOutput("val_S0")),
          div(class = "param-lbl", "± SD"),
          textOutput("sd_S0")
        )
      ),
      column(3,
        div(class = "param-box",
          div(class = "param-lbl", "Pente AS"),
          div(class = "param-val", textOutput("val_slope")),
          div(class = "param-lbl", "± SD"),
          textOutput("sd_slope")
        )
      ),
      column(3,
        div(class = "param-box",
          div(class = "param-lbl", "r²"),
          div(class = "param-val", textOutput("val_r2")),
          div(class = "param-lbl",
            uiOutput("badge_validation")
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(12,
        h4(tags$i(class = "bi bi-table"), " Tableau des 19 quantiles"),
        withSpinner(DT::dataTableOutput("table_taus"), type = 4),
        br(),
        downloadButton("dl_resultats_csv", "Exporter CSV",
                       icon = icon("download"),
                       class = "btn-sm btn-outline-success")
      )
    )
  ),

  # ------------------------------------------------------------------
  # Onglet 4 : Comparaison par poste
  # ------------------------------------------------------------------
  nav_panel(
    title = tagList(tags$i(class = "bi bi-people sidebar-icon"), "Par poste"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        tags$h5(tags$i(class = "bi bi-filter"), " Paramètre à comparer"),
        selectInput("param_poste", "Paramètre AS",
                    choices  = c("A0", "S0", "ASslope", "r2"),
                    selected = "A0")
      ),
      fluidRow(
        column(6,
          h4("Boxplot par poste"),
          withSpinner(plotOutput("plot_boxplot_poste", height = "400px"), type = 4)
        ),
        column(6,
          h4("Statistiques descriptives"),
          withSpinner(DT::dataTableOutput("table_stats_poste"), type = 4)
        )
      ),
      fluidRow(
        column(12,
          br(),
          h4("ANOVA — Résumé"),
          verbatimTextOutput("anova_poste"),
          h4("Test de Tukey"),
          verbatimTextOutput("tukey_poste")
        )
      )
    )
  ),

  # ------------------------------------------------------------------
  # Onglet 5 : Comparaison par type de session
  # ------------------------------------------------------------------
  nav_panel(
    title = tagList(tags$i(class = "bi bi-calendar3 sidebar-icon"), "Par session"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        tags$h5(tags$i(class = "bi bi-filter"), " Paramètre à comparer"),
        selectInput("param_session", "Paramètre AS",
                    choices  = c("A0", "S0", "ASslope", "r2"),
                    selected = "S0")
      ),
      fluidRow(
        column(6,
          h4("Boxplot par type de session"),
          withSpinner(plotOutput("plot_boxplot_session", height = "400px"), type = 4)
        ),
        column(6,
          h4("Statistiques descriptives"),
          withSpinner(DT::dataTableOutput("table_stats_session"), type = 4)
        )
      ),
      fluidRow(
        column(12,
          br(),
          h4("ANOVA — Résumé"),
          verbatimTextOutput("anova_session"),
          h4("Test de Tukey"),
          verbatimTextOutput("tukey_session")
        )
      )
    )
  ),

  # ------------------------------------------------------------------
  # Onglet 6 : Réserve compétitive
  # ------------------------------------------------------------------
  nav_panel(
    title = tagList(tags$i(class = "bi bi-trophy sidebar-icon"), "Réserve"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        tags$h5(tags$i(class = "bi bi-sliders2"), " Profil de référence élite"),
        numericInput("ref_A0", "A0 référence (m/s²)", value = 8.5,
                     min = 0, max = 20, step = 0.1),
        numericInput("ref_S0", "S0 référence (m/s)",  value = 9.5,
                     min = 0, max = 15, step = 0.1),
        hr(),
        actionBttn(
          inputId = "btn_reserve",
          label   = "Calculer la réserve",
          icon    = icon("calculator"),
          style   = "gradient",
          color   = "warning",
          size    = "md",
          block   = TRUE
        )
      ),
      fluidRow(
        column(12,
          h4("Réserve compétitive"),
          withSpinner(plotOutput("plot_reserve", height = "400px"), type = 4)
        )
      ),
      fluidRow(
        column(12,
          br(),
          h4("Tableau de la réserve compétitive"),
          withSpinner(DT::dataTableOutput("table_reserve"), type = 4)
        )
      )
    )
  ),

  # ------------------------------------------------------------------
  # Onglet 7 : Fiabilité & Rapport
  # ------------------------------------------------------------------
  nav_panel(
    title = tagList(tags$i(class = "bi bi-file-earmark-text sidebar-icon"), "Rapport"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        tags$h5(tags$i(class = "bi bi-check2-circle"), " Fiabilité"),
        selectInput("param_fiabilite", "Paramètre",
                    choices  = c("A0", "S0", "ASslope"),
                    selected = "A0"),
        hr(),
        tags$h5(tags$i(class = "bi bi-file-pdf"), " Rapport PDF"),
        downloadButton("dl_rapport_pdf", "Générer & Télécharger PDF",
                       icon  = icon("file-pdf"),
                       class = "btn-outline-danger w-100")
      ),
      fluidRow(
        column(6,
          h4("Indicateurs de fiabilité"),
          withSpinner(uiOutput("panel_fiabilite"), type = 4)
        ),
        column(6,
          h4("Graphique Bland-Altman"),
          withSpinner(plotOutput("plot_bland_altman", height = "400px"), type = 4)
        )
      )
    )
  )
)

# =============================================================================
# SERVEUR
# =============================================================================

server <- function(input, output, session) {

  # (1) Augmenter la taille maximale des uploads à 500 MB
  options(shiny.maxRequestSize = 500 * 1024^2)

  # ---- Valeurs réactives globales ----
  rv <- reactiveValues(
    donnees_brutes    = list(),   # liste de data.frames bruts par fichier
    resultats_pipeline = list(),  # résultats du prétraitement par fichier
    resultats_as      = list(),   # résultats AS (régression) par fichier
    df_resultats_tous = NULL,     # data.frame consolidé de tous les résultats
    erreur            = NULL      # message d'erreur éventuel
  )

  # ------------------------------------------------------------------
  # Lecture des fichiers CSV (plusieurs fichiers possibles)
  # ------------------------------------------------------------------
  observeEvent(input$csv_file, {
    req(input$csv_file)
    rv$erreur <- NULL

    tryCatch({
      rv$donnees_brutes <- lapply(seq_len(nrow(input$csv_file)), function(i) {
        chemin <- input$csv_file$datapath[i]
        nom    <- input$csv_file$name[i]

        res <- lire_csv_catapult(chemin)
        res$nom_fichier <- nom
        res
      })
    }, error = function(e) {
      rv$erreur <- paste("Erreur de lecture :", conditionMessage(e))
    })
  })

  # ------------------------------------------------------------------
  # Résumé du fichier chargé
  # ------------------------------------------------------------------
  output$resume_fichier <- renderText({
    if (!is.null(rv$erreur)) return(paste("ERREUR :", rv$erreur))
    req(rv$donnees_brutes, length(rv$donnees_brutes) > 0)

    infos <- sapply(seq_along(rv$donnees_brutes), function(i) {
      res <- rv$donnees_brutes[[i]]
      sprintf(
        "Fichier %d : %s\n  → En-tête ligne %d | Sépar. '%s' | Déc. '%s'\n  → %d lignes × %d colonnes",
        i, res$nom_fichier, res$ligne_entete, res$sep, res$dec,
        nrow(res$data), ncol(res$data)
      )
    })
    paste(infos, collapse = "\n\n")
  })

  # ------------------------------------------------------------------
  # Aperçu des colonnes détectées
  # ------------------------------------------------------------------
  output$apercu_colonnes <- renderTable({
    req(rv$donnees_brutes, length(rv$donnees_brutes) > 0)
    colonnes <- rv$donnees_brutes[[1]]$colonnes
    data.frame(
      N°     = seq_along(colonnes),
      Colonne = colonnes,
      check.names = FALSE
    )
  })

  # ------------------------------------------------------------------
  # Aperçu des 10 premières lignes
  # ------------------------------------------------------------------
  output$apercu_donnees <- DT::renderDataTable({
    req(rv$donnees_brutes, length(rv$donnees_brutes) > 0)
    DT::datatable(
      head(rv$donnees_brutes[[1]]$data, 10),
      options = list(scrollX = TRUE, pageLength = 10, dom = "t"),
      rownames = FALSE
    )
  })

  # ------------------------------------------------------------------
  # Analyse complète : pipeline preprocessing + régression AS
  # Déclenchée par le bouton "Analyser"
  # ------------------------------------------------------------------
  observeEvent(input$btn_analyser, {
    req(rv$donnees_brutes, length(rv$donnees_brutes) > 0)
    rv$erreur <- NULL

    withProgress(message = "Analyse en cours…", value = 0, {

      n_fichiers <- length(rv$donnees_brutes)
      rv$resultats_pipeline <- vector("list", n_fichiers)
      rv$resultats_as       <- vector("list", n_fichiers)
      lignes_resultats      <- vector("list", n_fichiers)

      for (i in seq_len(n_fichiers)) {

        setProgress(value = i / (n_fichiers + 1),
                    detail = paste("Fichier", i, "/", n_fichiers))

        df_brut <- rv$donnees_brutes[[i]]$data
        nom     <- rv$donnees_brutes[[i]]$nom_fichier

        # --- Étapes 0 à 4 : prétraitement ---
        res_prep <- tryCatch(
          pipeline_preprocessing(df_brut),
          error = function(e) {
            list(session_supprimee = TRUE,
                 message = paste("Erreur preprocessing :", conditionMessage(e)))
          }
        )
        rv$resultats_pipeline[[i]] <- res_prep

        if (res_prep$session_supprimee || nrow(res_prep$df_final) == 0) next

        # --- Étape 5 : sélection des points ---
        res5 <- tryCatch(
          etape5_selection_points(res_prep$df_final),
          error = function(e) {
            list(erreur = conditionMessage(e))
          }
        )

        if (!is.null(res5$erreur)) next

        # --- Étape 6 : régression quantile ---
        res6 <- tryCatch(
          etape6_regression_quantile(res5),
          error = function(e) {
            list(erreur = conditionMessage(e))
          }
        )

        if (!is.null(res6$erreur)) next

        rv$resultats_as[[i]] <- list(res5 = res5, res6 = res6)

        # Construire une ligne de résultats pour le tableau global
        lignes_resultats[[i]] <- data.frame(
          fichier    = nom,
          joueur     = if (nchar(input$nom_joueur) > 0) input$nom_joueur else paste("Joueur", i),
          position   = input$position,
          session    = input$type_session,
          A0         = res6$A0_moyen,
          A0_sd      = res6$A0_sd,
          S0         = res6$S0_moyen,
          S0_sd      = res6$S0_sd,
          ASslope    = res6$ASslope_moyen,
          ASslope_sd = res6$ASslope_sd,
          r2         = res6$r2,
          valide     = res6$validation,
          stringsAsFactors = FALSE
        )
      }

      setProgress(value = 1, detail = "Consolidation…")

      # Consolider tous les résultats
      df_tous <- do.call(rbind, lignes_resultats[!sapply(lignes_resultats, is.null)])
      rv$df_resultats_tous <- df_tous
    })
  })

  # ------------------------------------------------------------------
  # Badge qualité GPS
  # ------------------------------------------------------------------
  output$badge_qualite_gps <- renderUI({
    req(rv$resultats_pipeline, length(rv$resultats_pipeline) > 0)
    qgps <- rv$resultats_pipeline[[1]]$qualite
    if (is.null(qgps)) return(NULL)

    tags$div(
      class = paste("quality-badge alert alert-", qgps$couleur, sep = ""),
      tags$i(class = "bi bi-satellite"),
      sprintf(" %s | HDOP: %.2f | Sats: %.0f",
              qgps$texte_qualite, qgps$hdop_moyen, qgps$sats_moyen)
    )
  })

  # ------------------------------------------------------------------
  # Info preprocessing (nb points rejetés, etc.)
  # ------------------------------------------------------------------
  output$info_preprocessing <- renderUI({
    req(rv$resultats_pipeline, length(rv$resultats_pipeline) > 0)
    res <- rv$resultats_pipeline[[1]]

    tags$div(
      tags$small(class = "text-muted",
        tags$p(tags$b("Points rejetés σ : "), res$nb_rejetes_sigma %||% "—"),
        tags$p(tags$b("Points bruit DBSCAN : "), res$nb_bruit_dbscan %||% "—"),
        tags$p(tags$b("Message : "), res$message)
      )
    )
  })

  # ------------------------------------------------------------------
  # Graphique AS principal (onglet Profil AS)
  # ------------------------------------------------------------------
  plot_as_reactive <- reactive({
    req(rv$resultats_pipeline, rv$resultats_as)
    req(length(rv$resultats_pipeline) > 0, length(rv$resultats_as) > 0)

    res_prep <- rv$resultats_pipeline[[1]]
    res_as   <- rv$resultats_as[[1]]

    req(!is.null(res_as), !is.null(res_prep$df_final))

    plot_profil_as(
      tous_points       = res_prep$df_final,
      df_rejetes_sigma  = res_prep$df_rejetes_sigma,
      df_bruit_dbscan   = res_prep$df_bruit_dbscan,
      res5              = res_as$res5,
      res6              = res_as$res6,
      titre             = paste("Profil AS —",
                                if (nchar(input$nom_joueur) > 0)
                                  input$nom_joueur else "Joueur")
    )
  })

  output$plot_as_principal <- renderPlot({
    tryCatch(plot_as_reactive(), error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Graphique non disponible :", e$message)) +
        theme_minimal()
    })
  })

  # Téléchargement PNG du graphique AS
  output$dl_plot_as <- downloadHandler(
    filename = function() {
      paste0("profil_AS_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- plot_as_reactive()
      ggplot2::ggsave(file, plot = p, width = 10, height = 7,
                      dpi = 150, bg = "white")
    }
  )

  # ------------------------------------------------------------------
  # Valeurs AS individuelles (onglet Résultats)
  # ------------------------------------------------------------------
  output$val_A0    <- renderText({ formater_val(rv$resultats_as, "A0_moyen") })
  output$sd_A0     <- renderText({ formater_val(rv$resultats_as, "A0_sd", prefixe = "± ") })
  output$val_S0    <- renderText({ formater_val(rv$resultats_as, "S0_moyen") })
  output$sd_S0     <- renderText({ formater_val(rv$resultats_as, "S0_sd", prefixe = "± ") })
  output$val_slope <- renderText({ formater_val(rv$resultats_as, "ASslope_moyen") })
  output$sd_slope  <- renderText({ formater_val(rv$resultats_as, "ASslope_sd", prefixe = "± ") })
  output$val_r2    <- renderText({ formater_val(rv$resultats_as, "r2") })

  output$badge_validation <- renderUI({
    req(rv$resultats_as, length(rv$resultats_as) > 0)
    res6 <- rv$resultats_as[[1]]$res6
    req(!is.null(res6))
    if (res6$validation) {
      tags$span(class = "badge bg-success", "✓ r² > 0.81")
    } else {
      tags$span(class = "badge bg-danger",  "✗ r² ≤ 0.81")
    }
  })

  # Tableau des 19 quantiles
  output$table_taus <- DT::renderDataTable({
    req(rv$resultats_as, length(rv$resultats_as) > 0)
    res6 <- rv$resultats_as[[1]]$res6
    req(!is.null(res6))

    df_taus <- res6$df_taus
    df_taus <- round(df_taus, 4)
    DT::datatable(df_taus,
                  options = list(pageLength = 20, dom = "tip"),
                  rownames = FALSE,
                  colnames = c("τ", "A0", "Pente", "S0"))
  })

  # Export CSV des résultats
  output$dl_resultats_csv <- downloadHandler(
    filename = function() paste0("resultats_AS_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$df_resultats_tous)
      write.csv(rv$df_resultats_tous, file, row.names = FALSE)
    }
  )

  # ------------------------------------------------------------------
  # Onglet 4 : Comparaison par poste
  # ------------------------------------------------------------------
  output$plot_boxplot_poste <- renderPlot({
    req(rv$df_resultats_tous)
    plot_boxplot_parametre(rv$df_resultats_tous,
                           parametre   = input$param_poste,
                           grouper_par = "position",
                           titre       = paste(input$param_poste, "par poste"))
  })

  output$table_stats_poste <- DT::renderDataTable({
    req(rv$df_resultats_tous)
    stats <- etape7_stats_descriptives(rv$df_resultats_tous, grouper_par = "position")
    if (is.null(stats)) return(DT::datatable(data.frame()))
    DT::datatable(round(stats, 3), rownames = FALSE,
                  options = list(scrollX = TRUE, dom = "tip"))
  })

  output$anova_poste <- renderPrint({
    req(rv$df_resultats_tous)
    res <- tryCatch(
      etape8_anova_tukey(rv$df_resultats_tous,
                         parametre   = input$param_poste,
                         grouper_par = "position"),
      error = function(e) list(message = e$message)
    )
    if (!is.null(res$anova)) print(res$anova) else cat(res$message)
  })

  output$tukey_poste <- renderPrint({
    req(rv$df_resultats_tous)
    res <- tryCatch(
      etape8_anova_tukey(rv$df_resultats_tous,
                         parametre   = input$param_poste,
                         grouper_par = "position"),
      error = function(e) list(message = e$message)
    )
    if (!is.null(res$tukey)) print(res$tukey) else cat(res$message)
  })

  # ------------------------------------------------------------------
  # Onglet 5 : Comparaison par type de session
  # ------------------------------------------------------------------
  output$plot_boxplot_session <- renderPlot({
    req(rv$df_resultats_tous)
    plot_boxplot_parametre(rv$df_resultats_tous,
                           parametre   = input$param_session,
                           grouper_par = "session",
                           titre       = paste(input$param_session, "par type de session"))
  })

  output$table_stats_session <- DT::renderDataTable({
    req(rv$df_resultats_tous)
    stats <- etape7_stats_descriptives(rv$df_resultats_tous, grouper_par = "session")
    if (is.null(stats)) return(DT::datatable(data.frame()))
    DT::datatable(round(stats, 3), rownames = FALSE,
                  options = list(scrollX = TRUE, dom = "tip"))
  })

  output$anova_session <- renderPrint({
    req(rv$df_resultats_tous)
    res <- tryCatch(
      etape8_anova_tukey(rv$df_resultats_tous,
                         parametre   = input$param_session,
                         grouper_par = "session"),
      error = function(e) list(message = e$message)
    )
    if (!is.null(res$anova)) print(res$anova) else cat(res$message)
  })

  output$tukey_session <- renderPrint({
    req(rv$df_resultats_tous)
    res <- tryCatch(
      etape8_anova_tukey(rv$df_resultats_tous,
                         parametre   = input$param_session,
                         grouper_par = "session"),
      error = function(e) list(message = e$message)
    )
    if (!is.null(res$tukey)) print(res$tukey) else cat(res$message)
  })

  # ------------------------------------------------------------------
  # Onglet 6 : Réserve compétitive
  # ------------------------------------------------------------------
  reserve_calculee <- eventReactive(input$btn_reserve, {
    req(rv$df_resultats_tous)
    tryCatch(
      reserve_competitive_groupe(rv$df_resultats_tous,
                                 A0_reference = input$ref_A0,
                                 S0_reference = input$ref_S0,
                                 col_joueur   = "joueur"),
      error = function(e) {
        showNotification(paste("Erreur réserve :", e$message), type = "error")
        NULL
      }
    )
  })

  output$plot_reserve <- renderPlot({
    req(reserve_calculee())
    plot_reserve_competitive(reserve_calculee())
  })

  output$table_reserve <- DT::renderDataTable({
    req(reserve_calculee())
    DT::datatable(reserve_calculee(), rownames = FALSE,
                  options = list(scrollX = TRUE, dom = "tip"))
  })

  # ------------------------------------------------------------------
  # Onglet 7 : Fiabilité
  # ------------------------------------------------------------------
  output$panel_fiabilite <- renderUI({
    req(rv$df_resultats_tous)

    fb <- tryCatch(
      etape9_fiabilite(rv$df_resultats_tous,
                       parametre   = input$param_fiabilite,
                       col_joueur  = "joueur",
                       col_session = "session"),
      error = function(e) list(message = e$message)
    )

    if (is.null(fb) || is.na(fb$icc)) {
      return(tags$p(class = "text-muted", fb$message))
    }

    tagList(
      div(class = "param-box",
        div(class = "param-lbl", "ICC (IC 95%)"),
        div(class = "param-val",
            sprintf("%.3f [%.3f – %.3f]", fb$icc, fb$icc_lb, fb$icc_ub))
      ),
      div(class = "param-box",
        div(class = "param-lbl", "SEM"),
        div(class = "param-val", sprintf("%.3f", fb$sem))
      ),
      div(class = "param-box",
        div(class = "param-lbl", "CV%"),
        div(class = "param-val", sprintf("%.2f%%", fb$cv))
      )
    )
  })

  output$plot_bland_altman <- renderPlot({
    req(rv$df_resultats_tous, input$param_fiabilite)

    df <- rv$df_resultats_tous
    param <- input$param_fiabilite

    # Nécessite au moins 2 sessions par joueur pour Bland-Altman
    if (!all(c("joueur", "session", param) %in% names(df))) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "Données insuffisantes pour Bland-Altman") +
               theme_minimal())
    }

    # Pivoter pour avoir 2 colonnes de sessions
    df_large <- tryCatch({
      df[, c("joueur", "session", param)] %>%
        tidyr::pivot_wider(names_from = "session", values_from = param)
    }, error = function(e) NULL)

    if (is.null(df_large) || ncol(df_large) < 3) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "Deux sessions minimum requises") +
               theme_minimal())
    }

    cols_sess <- names(df_large)[sapply(df_large, is.numeric)]
    if (length(cols_sess) < 2) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "Deux sessions numériques minimum requises") +
               theme_minimal())
    }

    # Utiliser les deux premières sessions numériques
    m1 <- df_large[[cols_sess[1]]]
    m2 <- df_large[[cols_sess[2]]]
    ok <- !is.na(m1) & !is.na(m2)

    if (sum(ok) < 3) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "Données insuffisantes pour Bland-Altman") +
               theme_minimal())
    }

    plot_bland_altman(m1[ok], m2[ok],
                      titre = paste("Bland-Altman —", param))
  })

  # ------------------------------------------------------------------
  # Génération du rapport PDF
  # ------------------------------------------------------------------
  output$dl_rapport_pdf <- downloadHandler(
    filename = function() paste0("rapport_AS_", Sys.Date(), ".pdf"),
    content  = function(file) {
      withProgress(message = "Génération du rapport PDF…", {

        # Rassembler les objets pour le rapport
        res6 <- if (length(rv$resultats_as) > 0 && !is.null(rv$resultats_as[[1]]))
          rv$resultats_as[[1]]$res6 else NULL

        params_rapport <- list(
          resultats    = res6,
          plots        = list(
            as      = tryCatch(plot_as_reactive(), error = function(e) NULL),
            reserve = tryCatch(
              if (!is.null(reserve_calculee()))
                plot_reserve_competitive(reserve_calculee()) else NULL,
              error = function(e) NULL
            )
          ),
          qualite_gps  = if (length(rv$resultats_pipeline) > 0)
            rv$resultats_pipeline[[1]]$qualite else NULL,
          stats_groupe = if (!is.null(rv$df_resultats_tous))
            etape7_stats_descriptives(rv$df_resultats_tous) else NULL,
          reserve      = tryCatch(reserve_calculee(), error = function(e) NULL),
          fiabilite    = if (!is.null(rv$df_resultats_tous))
            tryCatch(
              etape9_fiabilite(rv$df_resultats_tous,
                               parametre   = input$param_fiabilite,
                               col_joueur  = "joueur",
                               col_session = "session"),
              error = function(e) NULL
            ) else NULL,
          nom_session  = input$type_session,
          nom_joueur   = if (nchar(input$nom_joueur) > 0) input$nom_joueur else "Joueur"
        )

        # Rendre le rapport depuis un répertoire temporaire
        tmp_rmd <- file.path(tempdir(), "report.Rmd")
        file.copy("R/report.Rmd", tmp_rmd, overwrite = TRUE)

        tryCatch({
          rmarkdown::render(
            input       = tmp_rmd,
            output_file = file,
            params      = params_rapport,
            envir       = new.env(parent = globalenv()),
            quiet       = TRUE
          )
        }, error = function(e) {
          showNotification(
            paste("Erreur génération PDF :", conditionMessage(e)),
            type     = "error",
            duration = 10
          )
          stop(e)
        })
      })
    }
  )
}

# =============================================================================
# Fonctions utilitaires locales
# =============================================================================

# Formater une valeur numérique issue des résultats AS
formater_val <- function(res_as, champ, prefixe = "", digits = 3) {
  if (is.null(res_as) || length(res_as) == 0 || is.null(res_as[[1]])) return("—")
  res6 <- res_as[[1]]$res6
  if (is.null(res6)) return("—")
  val <- res6[[champ]]
  if (is.null(val) || is.na(val)) return("—")
  paste0(prefixe, round(val, digits))
}

# Opérateur "null-coalesce" (retourne b si a est NULL)
`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# Lancement de l'application
# =============================================================================
shinyApp(ui = ui, server = server)
