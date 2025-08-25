# ==============================================================================
# Module: Historique & Évolutions (Version avec UI affinée)
# ==============================================================================

historique_ui <- function(id, df) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 3,
      bslib::card(
        bslib::card_header("Filtres de l'analyse"),
        bslib::card_body(
          padding = "10px",
          # --- AJOUT ---: Implémentation de l'accordéon statique
          bslib::accordion(
            open = TRUE,
            bslib::accordion_panel(
              title = "Options de Filtrage",
              icon = shiny::icon("filter"),
              
              shiny::selectInput(ns("filtre_taille_historique"), "Taille d'entreprise :", 
                                 choices = c("Toutes les tailles", unique(df$tranche_effectifs))),
              shiny::selectizeInput(ns("filtre_ept_historique"), "Choisir un ou plusieurs territoire(s) :", 
                                    choices = sort(unique(df$ept_name)), 
                                    selected = c("Ville de Paris", "Grand Paris Seine Ouest"), 
                                    multiple = TRUE)
            )
          )
        )
      )
    ),
    
    shiny::column(
      width = 9,
      shiny::uiOutput(ns("kpi_historique_ui")),
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     "Évolution Comparée des Scores Egapro",
                     color_switch_ui(ns("color_switch_historique"))
          )
        ),
        bslib::card_body(
          plotly::plotlyOutput(ns("plot_historique_interactif"), height = "450px")
        )
      ),
      bslib::card(
        bslib::card_header("Données Détaillées"),
        bslib::card_body(
          DT::dataTableOutput(ns("table_historique"))
        )
      )
    )
  )
}
# Le serveur est celui que tu as fourni, il est déjà parfait.
historique_server <- function(id, master_df_historique, palette_accessible) {
  shiny::moduleServer(id, function(input, output, session) {
    data_historique_agg <- shiny::reactive({
      shiny::req(input$filtre_ept_historique)
      df_base <- master_df_historique
      if (input$filtre_taille_historique != "Toutes les tailles") {
        df_base <- df_base %>% dplyr::filter(tranche_effectifs == input$filtre_taille_historique)
      }
      df_base %>% 
        dplyr::filter(ept_name %in% input$filtre_ept_historique) %>%
        dplyr::group_by(annee, ept_name) %>% 
        dplyr::summarise(score_moyen = weighted.mean(index, poids, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(ept_name, annee)
    })
    
    output$kpi_historique_ui <- shiny::renderUI({
      df <- data_historique_agg(); shiny::req(nrow(df) > 1)
      annee_debut <- min(df$annee); annee_fin <- max(df$annee)
      progression_df <- df %>% 
        dplyr::group_by(ept_name) %>% 
        dplyr::summarise(
          score_start = score_moyen[annee == annee_debut], 
          score_end = score_moyen[annee == annee_fin], 
          .groups = "drop"
        ) %>% 
        dplyr::filter(!is.na(score_start) & !is.na(score_end))
      
      if (nrow(progression_df) == 0) {
        return(bslib::value_box(title = "Progression Moyenne", value = "Données insuffisantes", theme = "secondary"))
      }
      
      mean_progression <- mean(progression_df$score_end - progression_df$score_start, na.rm = TRUE)
      
      kpi_theme <- if (mean_progression > 0) "success" else if (mean_progression < 0) "danger" else "secondary"
      kpi_icon <- if (mean_progression > 0) shiny::icon("arrow-trend-up") else if (mean_progression < 0) shiny::icon("arrow-trend-down") else shiny::icon("minus")
      
      bslib::value_box(
        title = paste("Progression Moyenne (", annee_debut, "-", annee_fin, ")"),
        value = sprintf("%+.1f points", mean_progression),
        showcase = kpi_icon,
        theme = kpi_theme
      )
    })
    
    output$plot_historique_interactif <- plotly::renderPlotly({
      df_plot <- data_historique_agg()
      shiny::req(nrow(df_plot) > 0, cancelOutput = TRUE)
      
      df_plot_with_tooltip <- df_plot %>%
        dplyr::mutate(tooltip_text = paste0("<b>Territoire:</b> ", as.character(ept_name), "<br><b>Année:</b> ", annee, "<br><b>Score:</b> ", round(score_moyen, 2)))
      
      palette_a_utiliser <- if (isTRUE(input$color_switch_historique)) {
        palette_accessible
      } else {
        rep(c("#7B61FF", "#495057", "#20C997", "#FD7E14", "#FFC107"), length.out = dplyr::n_distinct(df_plot_with_tooltip$ept_name))
      }
      
      plotly::plot_ly(data = df_plot_with_tooltip, x = ~annee, y = ~score_moyen, color = ~ept_name, colors = palette_a_utiliser, type = 'scatter', mode = 'lines+markers', text = ~tooltip_text, hoverinfo = 'text') %>%
        plotly::layout(title = list(text = NULL),
                       xaxis = list(title = "Année"), 
                       yaxis = list(title = "Score Egapro moyen pondéré"),
                       legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$table_historique <- DT::renderDataTable({
      df <- data_historique_agg()
      if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "Aucune donnée disponible"), options = list(dom = 't')))
      
      df_table <- df %>% 
        dplyr::select(Territoire = ept_name, Année = annee, `Score Moyen` = score_moyen) %>% 
        tidyr::pivot_wider(names_from = Année, values_from = `Score Moyen`)
      
      df_table %>%
        DT::datatable(rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip', ordering = FALSE, buttons = list('copy', 'csv', 'excel'), language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json')),
                      caption = NULL) %>%
        DT::formatRound(columns = which(sapply(df_table, is.numeric)), digits = 2)
    })
  })
}