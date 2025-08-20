historique_ui <- function(id, df) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
                        shiny::h4("Filtres de l'analyse"),
                        shiny::selectInput(ns("filtre_taille_historique"), "Taille d'entreprise :", choices = c("Toutes les tailles", unique(df$tranche_effectifs))),
                        shiny::selectizeInput(ns("filtre_ept_historique"), "Choisir un ou plusieurs territoire(s) :", choices = sort(unique(df$ept_name)), selected = c("Ville de Paris", "Grand Paris Seine Ouest"), multiple = TRUE)
    ),
    shiny::mainPanel(width = 9, shiny::uiOutput(ns("kpi_historique_ui")), shiny::hr(), plotly::plotlyOutput(ns("plot_historique_interactif"), height = "450px"), color_switch_ui(ns("color_switch_historique")), shiny::hr(), DT::dataTableOutput(ns("table_historique")))
  )
}

historique_server <- function(id, master_df_historique, palette_accessible) {
  shiny::moduleServer(id, function(input, output, session) {
    data_historique_agg <- shiny::reactive({
      shiny::req(input$filtre_ept_historique)
      df_base <- master_df_historique
      if (input$filtre_taille_historique != "Toutes les tailles") {
        df_base <- df_base |> dplyr::filter(tranche_effectifs == input$filtre_taille_historique)
      }
      df_base |> 
        dplyr::filter(ept_name %in% input$filtre_ept_historique) |>
        dplyr::group_by(annee, ept_name) |>
        dplyr::summarise(score_moyen = weighted.mean(index, poids, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(ept_name, annee)
    })
    output$kpi_historique_ui <- shiny::renderUI({
      df <- data_historique_agg(); shiny::req(nrow(df) > 1)
      annee_debut <- min(df$annee); annee_fin <- max(df$annee)
      progression_df <- df |> dplyr::group_by(ept_name) |> dplyr::summarise(score_start = score_moyen[annee == annee_debut], score_end = score_moyen[annee == annee_fin], .groups = "drop") |> dplyr::filter(!is.na(score_start) & !is.na(score_end))
      if (nrow(progression_df) == 0) return(shiny::tags$div(class="alert alert-warning", "Données insuffisantes pour calculer la progression."))
      mean_progression <- mean(progression_df$score_end - progression_df$score_start, na.rm = TRUE)
      progression_texte <- sprintf("%+.1f", mean_progression)
      kpi_style <- "background-color: #2C3E50; padding: 25px; border-radius: 12px; text-align: center; margin-top: 0px; box-shadow: 0 4px 12px rgba(0,0,0,0.15);"
      shiny::tags$div(style = kpi_style,
                      shiny::tags$h2(style="margin: 0; font-size: 3em; font-weight: bold; color: #ECF0F1;", progression_texte),
                      shiny::tags$p(style="margin: 0; font-size: 1.2em; color: #ECF0F1;", paste("Progression moyenne entre", annee_debut, "et", annee_fin))
      )
    })
    output$plot_historique_interactif <- plotly::renderPlotly({
      df_plot <- data_historique_agg()
      shiny::req(nrow(df_plot) > 0, cancelOutput = TRUE)
      if (nrow(df_plot) == 0) {
        shiny::showNotification("Veuillez sélectionner au moins un territoire.", type = "message", duration = 5)
        return(NULL)
      }
      df_plot_with_tooltip <- df_plot |>
        dplyr::mutate(tooltip_text = paste0("<b>Territoire:</b> ", as.character(ept_name), "<br><b>Année:</b> ", annee, "<br><b>Score:</b> ", round(score_moyen, 2)))
      palette_a_utiliser <- if (isTRUE(input$color_switch_historique)) {
        palette_accessible[seq_len(dplyr::n_distinct(df_plot_with_tooltip$ept_name))]
      } else {
        suppressWarnings(
          RColorBrewer::brewer.pal(max(3, dplyr::n_distinct(df_plot_with_tooltip$ept_name)), "Set1")
        )[seq_len(dplyr::n_distinct(df_plot_with_tooltip$ept_name))]
      }
      plotly::plot_ly(data = df_plot_with_tooltip, x = ~annee, y = ~score_moyen, color = ~ept_name, colors = palette_a_utiliser, type = 'scatter', mode = 'lines+markers', text = ~tooltip_text, hoverinfo = 'text') |>
        plotly::layout(title = list(text = "Évolution Comparée des Scores Egapro", x = 0.5),
                       xaxis = list(title = "Année"), yaxis = list(title = "Score Egapro moyen pondéré"),
                       legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)) |>
        plotly::config(displayModeBar = FALSE)
    })
    output$table_historique <- DT::renderDataTable({
      df <- data_historique_agg(); if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "Aucune donnée disponible"), options = list(dom = 't')))
      df_table <- df |> dplyr::select(Territoire = ept_name, Année = annee, `Score Moyen` = score_moyen) |> tidyr::pivot_wider(names_from = Année, values_from = `Score Moyen`)
      df_table |>
        DT::datatable(rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip', ordering = FALSE, buttons = list('copy', 'csv', 'excel'), language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json')),
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color: #333; font-size: 1.2em; font-weight: bold;', 'Données détaillées')) |>
        DT::formatRound(columns = which(sapply(df_table, is.numeric)), digits = 2)
    })
  })
}