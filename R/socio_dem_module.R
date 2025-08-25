# ==============================================================================
# Module: Socio-démographique (Version FINALE - OPÉRATION "PONDÉRATION")
# ==============================================================================

socio_dem_ui <- function(id, df) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 3,
      bslib::card(
        bslib::card_header("Filtres de l'analyse"),
        bslib::card_body(
          padding = "10px",
          bslib::accordion(
            open = TRUE,
            bslib::accordion_panel(
              title = "Options de Filtrage",
              icon = shiny::icon("filter"),
              shinyWidgets::sliderTextInput(inputId = ns("filtre_annee_socio"), label = "Année :", choices = sort(unique(df$annee)), selected = max(df$annee), grid = TRUE, width = "100%"),
              shiny::selectInput(ns("socio_variable"), "Indicateur Socio-économique :", choices = list("Structure emploi féminin" = c(`Part des femmes cadres` = "part_femmes_cadres", `Part des femmes prof. inter.` = "part_femmes_prof_inter"), "Mixité & activité" = c(`Taux de féminisation des cadres` = "taux_femmes_parmi_cadres", `Taux d'activité des femmes 15-64`= "taux_activite_femmes")), selected = "part_femmes_cadres"),
              shiny::selectizeInput(ns("filtre_ze_sd"), "Filtrer par Zone d'Emploi :", choices  = sort(unique(na.omit(df$ze_name))), multiple = TRUE, options = list(placeholder = "Toutes les zones d'emploi"))
            )
          )
        )
      )
    ),
    shiny::column(
      width = 9,
      bslib::card(bslib::card_header(shiny::uiOutput(ns("titre_socio_ui"))), bslib::card_body(plotly::plotlyOutput(ns("plot_socio_interactif"), height = "500px"))),
      shiny::uiOutput(ns("kpi_socio_dem_ui")),
      bslib::card(bslib::card_header("Données Moyennes des Zones d'Emploi"), bslib::card_body(DT::DTOutput(ns("table_sd"))))
    )
  )
}

socio_dem_server <- function(id, master_df_historique, socio_variable_labels) {
  shiny::moduleServer(id, function(input, output, session) {
    data_sd <- shiny::reactive({
      master_df_historique %>%
        dplyr::filter(
          annee == input$filtre_annee_socio,
          if (length(input$filtre_ze_sd) > 0) ze_name %in% input$filtre_ze_sd else TRUE,
          !is.na(ze_name)
        ) %>%
        dplyr::group_by(ze_name) %>%
        dplyr::summarise(
          score_moyen = weighted.mean(index, poids, na.rm = TRUE),
          socio_val   = mean(.data[[input$socio_variable]], na.rm = TRUE),
          n           = dplyr::n(),
          .groups     = "drop"
        ) %>%
        dplyr::filter(is.finite(score_moyen) & is.finite(socio_val))
    })
    
    output$titre_socio_ui <- shiny::renderUI({ shiny::div(shiny::h4("Corrélation : Score Egapro & Indicateur Social", style = "margin-bottom: 0.2rem;"), shiny::p(class = "text-muted", style = "margin-top: 0; font-size: 0.9rem;", paste(socio_variable_labels[input$socio_variable], "(données agrégées par Zone d'Emploi)"))) })
    
    # --- MOTEUR DE CALCUL KPI AMÉLIORÉ ---
    output$kpi_socio_dem_ui <- shiny::renderUI({
      df <- data_sd()
      if (nrow(df) < 3) { return(shiny::fluidRow(shiny::column(width = 6, bslib::value_box("Coefficient (r)", "Données", showcase = icon("times"), theme = "secondary")), shiny::column(width = 6, bslib::value_box("Significativité (p)", "insuffisantes", showcase = icon("times"), theme = "secondary")))) }
      
      # UTILISATION DE LA CORRÉLATION PONDÉRÉE
      ct <- weights::wtd.cor(df$socio_val, df$score_moyen, weight = df$n)
      r  <- ct[1, "correlation"]
      p  <- ct[1, "p.value"]
      
      tendance <- dplyr::case_when(r > 0.4 ~ "Positive marquée", r > 0.15 ~ "Positive faible", r < -0.4 ~ "Négative marquée", r < -0.15 ~ "Négative faible", TRUE ~ "Quasi nulle")
      icon_name_p <- dplyr::case_when(p < 0.05 ~ "check", p < 0.10 ~ "exclamation-triangle", TRUE ~ "question")
      kpi_theme_p <- dplyr::case_when(p < 0.05 ~ "success", p < 0.10 ~ "warning", TRUE ~ "secondary")
      signification_texte <- dplyr::case_when(p < 0.05 ~ "Statistiquement significatif", p < 0.10 ~ "Marginalement significatif", TRUE ~ "Non significatif")
      shiny::fluidRow(shiny::column(width = 6, bslib::value_box(title = "Coefficient de Corrélation (r)", value = format(round(r, 2), nsmall = 2), showcase = icon("right-left"), theme = "primary", p(tendance))), shiny::column(width = 6, bslib::value_box(title = "Significativité (p-value)", value = format.pval(p, digits = 2, eps = 0.001), showcase = shiny::icon(icon_name_p), theme = kpi_theme_p, p(signification_texte))))
    })
    
    # --- GRAPHIQUE AMÉLIORÉ ---
    output$plot_socio_interactif <- plotly::renderPlotly({
      df <- data_sd()
      shiny::validate(shiny::need(nrow(df) > 1, "Données insuffisantes pour tracer une relation."))
      g <- ggplot2::ggplot(df, ggplot2::aes(x = socio_val, y = score_moyen, size = n, label = ze_name, text = paste0("<b>Zone d'Emploi :</b> ", ze_name, "<br><b>Score Egapro moyen :</b> ", round(score_moyen, 1), "<br><b>", stringr::str_trunc(socio_variable_labels[input$socio_variable], 30), " :</b> ", round(socio_val, 1), "%", "<br><b>Nb d'entreprises :</b> ", n))) +
        # LIGNE DE TENDANCE MAINTENANT PONDÉRÉE, COMME LE KPI
        ggplot2::geom_smooth(method = "lm", se = FALSE, ggplot2::aes(weight = n, label = NULL, text = NULL, size = NULL), colour = "#DC3545", linetype = "dashed", linewidth = 0.8) +
        ggplot2::geom_point(colour = "#7B61FF", alpha = 0.7) + ggrepel::geom_text_repel(force = 2, max.overlaps = Inf, min.segment.length = 0) +
        ggplot2::labs(x = socio_variable_labels[input$socio_variable], y = "Score Egapro moyen pondéré", size = "Nb d'entreprises") +
        ggplot2::theme_minimal(base_family = "Inter") + ggplot2::coord_cartesian(ylim = c(80, 100))
      plotly::ggplotly(g, tooltip = "text") %>% plotly::layout(legend = list(orientation = 'h', y = -0.2, x = 0.5, xanchor = 'center')) %>% plotly::config(displayModeBar = FALSE)
    })
    
    output$table_sd <- DT::renderDT({
      master_df_historique %>%
        dplyr::filter(annee == input$filtre_annee_socio, if (length(input$filtre_ze_sd) > 0) ze_name %in% input$filtre_ze_sd else TRUE, !is.na(ze_name)) %>%
        dplyr::distinct(ze_name, .keep_all = TRUE) %>%
        dplyr::select(`Zone d'Emploi` = ze_name, `Taux activité F` = taux_activite_femmes, `Femmes cadres` = part_femmes_cadres, `Femmes prof. inter.` = part_femmes_prof_inter, `Taux féminisation cadres` = taux_femmes_parmi_cadres) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ sprintf("%.1f %%", .x))) %>% DT::datatable(options = list(dom = "t"), rownames = FALSE)
    })
  })
}