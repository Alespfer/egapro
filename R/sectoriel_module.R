sectoriel_ui <- function(id, df) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
                        shiny::h4("Filtres de l'analyse"),
                        shiny::sliderInput(ns("filtre_annee_secteur"), "Année :", min = min(df$annee), max = max(df$annee), value = max(df$annee), step = 1, sep = ""),
                        shiny::selectInput(ns("filtre_taille_secteur"), "Taille d'entreprise :", choices = c("Toutes les tailles", unique(df$tranche_effectifs))),
                        shiny::selectizeInput(ns("filtre_secteurs"), "Secteur(s) d'activité :", choices = sort(unique(na.omit(df$secteur_activite))), multiple = TRUE, options  = list(placeholder = "Top/Bottom 5", maxItems = 15)),
                        shiny::tags$small(
                          class = "text-muted",
                          shiny::icon("info-circle"), 
                          "L'affichage par défaut montre les 5 meilleurs et 5 moins bons secteurs",
                          "ayant au moins 10 entreprises déclarantes pour la période sélectionnée."
                        ),
                        shiny::checkboxInput(ns("afficher_tous_secteurs"), "Afficher tous les secteurs", value = FALSE)
    ),
    shiny::mainPanel(width = 9, shiny::h3("Performance par Secteur d'Activité"), shiny::uiOutput(ns("plot_sector_ui")), color_switch_ui(ns("color_switch_secteur")), shiny::hr(), shiny::h4("Tableau de synthèse"), DT::dataTableOutput(ns("table_secteur")))
  )
}

sectoriel_server <- function(id, master_df_historique, palette_accessible) {
  shiny::moduleServer(id, function(input, output, session) {
    data_secteur_filtree <- shiny::reactive({
      df <- master_df_historique |>
        dplyr::filter(annee == input$filtre_annee_secteur, !is.na(secteur_activite))
      if (input$filtre_taille_secteur != "Toutes les tailles") {
        df <- df |> dplyr::filter(tranche_effectifs == input$filtre_taille_secteur)
      }
      if (isTRUE(input$afficher_tous_secteurs)) {
        return(df)
      }
      if (!is.null(input$filtre_secteurs) && length(input$filtre_secteurs) > 0) {
        return(df |> dplyr::filter(secteur_activite %in% input$filtre_secteurs))
      }
      sector_summary <- df |>
        dplyr::group_by(secteur_activite) |>
        dplyr::summarise(median_score = stats::median(index, na.rm = TRUE), n = dplyr::n(), .groups = "drop") |>
        dplyr::filter(n >= 10) |>
        dplyr::arrange(dplyr::desc(median_score))
      df |>
        dplyr::filter(secteur_activite %in% c(utils::head(sector_summary$secteur_activite, 5), utils::tail(sector_summary$secteur_activite, 5)))
    })
    
    plot_height_react <- shiny::reactiveVal(450)
    
    output$plot_sector_ui <- shiny::renderUI({
      plotly::plotlyOutput(session$ns("plot_secteur"), height = paste0(plot_height_react(), "px"))
    })
    
    output$plot_secteur <- plotly::renderPlotly({
      df_plot <- data_secteur_filtree()
      shiny::req(nrow(df_plot) > 0, cancelOutput = TRUE)
      
      sector_summary <- df_plot |>
        dplyr::group_by(secteur_activite) |>
        dplyr::summarise(score_median = stats::median(index, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(score_median)
      
      n_cols <- dplyr::n_distinct(sector_summary$secteur_activite)
      palette_cols <- if (isTRUE(input$color_switch_secteur)) {
        rep(palette_accessible, length.out = n_cols)
      } else {
        rep(RColorBrewer::brewer.pal(9, "Set1"), length.out = n_cols)
      }
      sector_summary$col <- palette_cols
      plot_height <- max(400, 40 * nrow(sector_summary))
      left_margin <- max(150, 6 * max(nchar(sector_summary$secteur_activite)))
      plot_height_react(plot_height)
      
      g <- ggplot2::ggplot(sector_summary,
                           ggplot2::aes(x = score_median,
                                        y = reorder(stringr::str_wrap(secteur_activite, 40), score_median),
                                        text = sprintf("<b>Secteur :</b> %s<br><b>Médiane :</b> %.1f",
                                                       secteur_activite, score_median))) +
        ggplot2::geom_segment(ggplot2::aes(x = 0, xend = score_median,
                                           yend = reorder(stringr::str_wrap(secteur_activite, 40),
                                                          score_median)),
                              linewidth = 1.2, colour = "#BDBDBD") +
        ggplot2::geom_point(ggplot2::aes(fill = col),
                            colour = "black", shape = 21, size = 7, stroke = .6) +
        ggplot2::geom_vline(xintercept = 85, linetype = "dashed",
                            linewidth = 1.2, colour = "red") +
        ggplot2::scale_fill_identity() +
        ggplot2::guides(colour = "none", fill = "none") +
        ggplot2::labs(x = "Score Egapro (médiane)", y = NULL) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      
      g |>
        plotly::ggplotly(tooltip = "text") |>
        plotly::layout(margin = list(l = left_margin, t = 10, r = 20, b = 10),
                       height = plot_height) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    output$table_secteur <- DT::renderDataTable({
      summary_df <- data_secteur_filtree() |>
        dplyr::group_by(secteur_activite) |>
        dplyr::summarise(`Nb entreprises` = dplyr::n(),
                         `Score Moyen`    = mean(index,  na.rm = TRUE),
                         `Score Médian`   = stats::median(index, na.rm = TRUE),
                         `% < 85`         = mean(index < 85, na.rm = TRUE),
                         .groups          = "drop") |>
        dplyr::arrange(dplyr::desc(`Score Moyen`))
      
      summary_df |>
        DT::datatable(rownames = FALSE, extensions = "Buttons",
                      options  = list(pageLength = 10, dom = "Bfrtip",
                                      buttons   = list("copy", "csv", "excel"),
                                      language  = list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"))) |>
        DT::formatRound(columns = 2,   digits = 0) |>
        DT::formatRound(columns = 3:4, digits = 1) |>
        DT::formatPercentage(columns = 5, digits = 1)
    })
  })
}