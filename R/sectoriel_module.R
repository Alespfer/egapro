# ==============================================================================
# Module: Analyse Sectorielle (Version finale avec infobulles)
# ==============================================================================

sectoriel_ui <- function(id, df) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 3,
      bslib::card(
        bslib::card_header("Filtres de l'analyse"),
        bslib::card_body(
          padding = "10px", # On garde le padding pour l'harmonie
          # --- CORRECTION FINALE ---: On utilise l'accordéon mais on le force à rester ouvert
          bslib::accordion(
            open = TRUE, # L'ordre crucial : les panneaux ne peuvent pas être fermés
            bslib::accordion_panel(
              title = "Options de Filtrage",
              icon = shiny::icon("filter"),
              
              shinyWidgets::sliderTextInput(
                inputId = ns("filtre_annee_secteur"),
                label = "Année :",
                choices = sort(unique(df$annee)),
                selected = max(df$annee),
                grid = TRUE,
                width = "100%"
              ),
              shiny::div(class = "d-flex align-items-center",
                         shiny::selectInput(ns("filtre_taille_secteur"), "Taille d'entreprise :", choices = c("Toutes les tailles", unique(df$tranche_effectifs)), width = "90%"),
                         bslib::tooltip(
                           shiny::icon("info-circle"),
                           "L'Index est obligatoire pour les entreprises de 50 salariés et plus. Le calcul de certains indicateurs varie selon la tranche d'effectifs.",
                           placement = "right"
                         )
              ),
              shiny::selectizeInput(ns("filtre_secteurs"), "Secteur(s) d'activité :", choices = sort(unique(na.omit(df$secteur_activite))), multiple = TRUE, options  = list(placeholder = "Top/Bottom 5", maxItems = 15)),
              shiny::tags$small(
                class = "text-muted",
                shiny::icon("info-circle"), 
                "L'affichage par défaut montre les 5 meilleurs et 5 moins bons secteurs ayant au moins 10 entreprises déclarantes."
              ),
              shiny::checkboxInput(ns("afficher_tous_secteurs"), "Afficher tous les secteurs", value = FALSE)
            )
          )
        )
      )
    ),
    
    shiny::column(
      width = 9,
      shiny::uiOutput(ns("kpi_sectoriel_ui")),
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     "Performance par Secteur d'Activité",
                     color_switch_ui(ns("color_switch_secteur"))
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("plot_sector_ui"))
        )
      ),
      bslib::card(
        bslib::card_header("Tableau de Synthèse par Secteur"),
        bslib::card_body(
          DT::dataTableOutput(ns("table_secteur"))
        )
      )
    )
  )
}

sectoriel_server <- function(id, master_df_historique, palette_accessible, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    data_secteur_filtree <- shiny::reactive({
      df <- master_df_historique %>%
        dplyr::filter(annee == input$filtre_annee_secteur, !is.na(secteur_activite))
      if (input$filtre_taille_secteur != "Toutes les tailles") {
        df <- df %>% dplyr::filter(tranche_effectifs == input$filtre_taille_secteur)
      }
      if (isTRUE(input$afficher_tous_secteurs)) {
        return(df)
      }
      if (!is.null(input$filtre_secteurs) && length(input$filtre_secteurs) > 0) {
        return(df %>% dplyr::filter(secteur_activite %in% input$filtre_secteurs))
      }
      sector_summary <- df %>%
        dplyr::group_by(secteur_activite) %>%
        dplyr::summarise(median_score = stats::median(index, na.rm = TRUE), n = dplyr::n(), .groups = "drop") %>%
        dplyr::filter(n >= 10) %>%
        dplyr::arrange(dplyr::desc(median_score))
      df %>%
        dplyr::filter(secteur_activite %in% c(utils::head(sector_summary$secteur_activite, 5), utils::tail(sector_summary$secteur_activite, 5)))
    })
    
    output$kpi_sectoriel_ui <- shiny::renderUI({
      df_filtre <- data_secteur_filtree()
      shiny::validate(shiny::need(nrow(df_filtre) > 0, "Aucune donnée à afficher pour les filtres sélectionnés."))
      mediane_globale <- median(df_filtre$index, na.rm = TRUE)
      summary_secteur <- df_filtre %>%
        dplyr::group_by(secteur_activite) %>%
        dplyr::summarise(mediane_score = median(index, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(!is.na(mediane_score)) %>%
        dplyr::arrange(dplyr::desc(mediane_score))
      secteur_top <- if(nrow(summary_secteur) > 0) summary_secteur$secteur_activite[1] else "N/A"
      score_top <- if(nrow(summary_secteur) > 0) summary_secteur$mediane_score[1] else NA
      shiny::fluidRow(
        shiny::column(width = 6,
                      bslib::value_box(
                        title = "Secteur le plus performant",
                        value = secteur_top,
                        showcase = shiny::icon("trophy"),
                        theme = "success",
                        p(paste("Médiane de", round(score_top, 1)))
                      )
        ),
        shiny::column(width = 6,
                      bslib::value_box(
                        title = "Médiane globale (tous secteurs affichés)",
                        value = round(mediane_globale, 1),
                        showcase = shiny::icon("chart-pie"),
                        theme = "primary"
                      )
        )
      )
    })
    
    plot_height_react <- shiny::reactiveVal(450)
    
    output$plot_sector_ui <- shiny::renderUI({
      plotly::plotlyOutput(session$ns("plot_secteur"), height = paste0(plot_height_react(), "px"))
    })
    
    output$plot_secteur <- plotly::renderPlotly({
      df_plot <- data_secteur_filtree()
      shiny::req(nrow(df_plot) > 0, cancelOutput = TRUE)
      sector_summary <- df_plot %>%
        dplyr::group_by(secteur_activite) %>%
        dplyr::summarise(score_median = stats::median(index, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(score_median)
      default_palette <- c("#7B61FF", "#495057", "#20C997", "#FD7E14", "#FFC107")
      palette_a_utiliser <- if (isTRUE(input$color_switch_secteur)) palette_accessible else default_palette
      n_cols <- dplyr::n_distinct(sector_summary$secteur_activite)
      palette_cols <- rep(palette_a_utiliser, length.out = n_cols)
      sector_summary$col <- palette_cols
      plot_height <- max(400, 35 * nrow(sector_summary))
      left_margin <- max(150, 7 * max(nchar(sector_summary$secteur_activite)))
      plot_height_react(plot_height)
      
      g <- ggplot2::ggplot(sector_summary,
                           ggplot2::aes(x = score_median,
                                        y = reorder(stringr::str_wrap(secteur_activite, 40), score_median),
                                        key = secteur_activite,
                                        text = sprintf("<b>Secteur :</b> %s<br><b>Médiane :</b> %.1f",
                                                       secteur_activite, score_median))) +
        ggplot2::geom_segment(ggplot2::aes(x = 0, xend = score_median, yend = reorder(stringr::str_wrap(secteur_activite, 40), score_median)), linewidth = 1.5, colour = "#CED4DA") +
        ggplot2::geom_point(size = 7, aes(fill = col), shape = 21, colour = "#495057", stroke = 1) +
        ggplot2::geom_vline(xintercept = 85, linetype = "dashed", linewidth = 1, colour = "#DC3545") +
        ggplot2::scale_fill_identity() +
        ggplot2::guides(fill = "none") +
        ggplot2::labs(x = "Score Egapro (médiane)", y = NULL) +
        ggplot2::theme_minimal(base_family = "Inter") +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(face = "bold")
        )
      
      p <- g %>%
        plotly::ggplotly(tooltip = "text", source = "plot_secteur_source", height = plot_height) %>%
        plotly::layout(margin = list(l = left_margin, t = 30, r = 20, b = 40)) %>%
        plotly::config(displayModeBar = FALSE)
      
      p <- p %>% plotly::event_register("plotly_click")
      p
    })
    
    shiny::observeEvent(plotly::event_data("plotly_click", source = "plot_secteur_source"), {
      clicked_sector <- plotly::event_data("plotly_click", source = "plot_secteur_source")$key
      shiny::req(clicked_sector)
      shared_state$selected_sector <- clicked_sector
      shiny::showNotification(
        paste("Filtre appliqué sur le secteur :", clicked_sector, ". Affichage de la carte..."),
        type = "message",
        duration = 4
      )
      shiny::updateNavbarPage(session, "main_navbar", selected = "Carte & Territoires")
    })
    
    output$table_secteur <- DT::renderDataTable({
      summary_df <- data_secteur_filtree() %>%
        dplyr::group_by(secteur_activite) %>%
        dplyr::summarise(`Nb entreprises` = dplyr::n(),
                         `Score Moyen`    = mean(index,  na.rm = TRUE),
                         `Score Médian`   = stats::median(index, na.rm = TRUE),
                         `% < 85`         = mean(index < 85, na.rm = TRUE),
                         .groups          = "drop") %>%
        dplyr::arrange(dplyr::desc(`Score Moyen`))
      
      summary_df %>%
        DT::datatable(rownames = FALSE, extensions = "Buttons",
                      options  = list(pageLength = 10, dom = "Bfrtip",
                                      buttons   = list("copy", "csv", "excel"),
                                      language  = list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"))) %>%
        DT::formatRound(columns = 2,   digits = 0) %>%
        DT::formatRound(columns = 3:4, digits = 1) %>%
        DT::formatPercentage(columns = 5, digits = 1)
    })
  })
}