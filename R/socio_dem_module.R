# ==============================================================================
# Module: Socio-démographique (Version Finale Corrigée)
# ==============================================================================

socio_dem_ui <- function(id, df) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 3,
      bslib::card(
        bslib::card_header("Filtres de l'analyse"),
        bslib::card_body(
          shinyWidgets::sliderTextInput(
            inputId = ns("filtre_annee_socio"),
            label = "Année :",
            choices = sort(unique(df$annee)),
            selected = max(df$annee),
            grid = TRUE,
            width = "100%"
          ),
          shiny::selectInput(ns("socio_variable"), "Indicateur :",
                             choices = list("Structure emploi féminin" = c(`Part des femmes cadres` = "part_femmes_cadres", `Part des femmes prof. inter.` = "part_femmes_prof_inter"),
                                            "Mixité & activité" = c(`Taux de féminisation des cadres` = "taux_femmes_parmi_cadres", `Taux d'activité des femmes 15-64`= "taux_activite_femmes"))
                             , selected = "part_femmes_cadres"),
          shiny::selectizeInput(ns("filtre_ept_sd"), "Territoires (EPT) :", choices  = sort(unique(df$ept_name)), multiple = TRUE, options = list(placeholder = "Tous les territoires")),
          shiny::uiOutput(ns("alert_paris_sd"))      
        )
      )
    ),
    shiny::column(
      width = 9,
      bslib::card(
        bslib::card_header(shiny::uiOutput(ns("titre_socio_ui"))),
        bslib::card_body(
          plotly::plotlyOutput(ns("plot_socio_interactif"), height = "550px"),
          shiny::uiOutput(ns("corr_banner"))
        )
      ),
      bslib::card(
        bslib::card_header("Données Moyennes des Territoires"),
        bslib::card_body(
          DT::DTOutput(ns("table_sd"))
        )
      )
    )
  )
}

socio_dem_server <- function(id, master_df_historique, socio_variable_labels) {
  shiny::moduleServer(id, function(input, output, session) {
    data_sd <- shiny::reactive({
      master_df_historique %>%
        dplyr::filter(
          annee == input$filtre_annee_socio,
          if (length(input$filtre_ept_sd) > 0)
            ept_name %in% input$filtre_ept_sd else TRUE
        ) %>%
        dplyr::group_by(ept_name) %>%
        dplyr::summarise(
          score_moyen = weighted.mean(index, poids, na.rm = TRUE),
          socio_val   = mean(.data[[input$socio_variable]], na.rm = TRUE),
          n           = dplyr::n(),
          .groups     = "drop"
        ) %>%
        dplyr::filter(is.finite(score_moyen) & is.finite(socio_val))
    })
    
    output$titre_socio_ui <- shiny::renderUI({ 
      shiny::HTML(paste("Score Egapro moyen vs.", socio_variable_labels[input$socio_variable])) 
    })
    
    output$plot_socio_interactif <- plotly::renderPlotly({
      df <- data_sd()
      shiny::validate(shiny::need(nrow(df) > 1, "Données insuffisantes pour tracer une relation."))
      
      # On garde le ggplot de base pour ggrepel
      g <- ggplot2::ggplot(df, ggplot2::aes(x = socio_val, y = score_moyen, 
                                            size = n, 
                                            label = ept_name,
                                            text = paste0(
                                              "<b>Territoire :</b> ", ept_name,
                                              "<br><b>Score Egapro moyen :</b> ", round(score_moyen, 1),
                                              "<br><b>", stringr::str_trunc(socio_variable_labels[input$socio_variable], 30), " :</b> ", round(socio_val, 1), "%",
                                              "<br><b>Nb d'entreprises :</b> ", n
                                            ))) +
        # LA CORRECTION EST ICI : On retire `inherit.aes` et on met l'aes directement
        ggplot2::geom_smooth(method = "lm", se = FALSE, 
                             ggplot2::aes(weight = n, label = NULL, text = NULL), # On annule les aes non désirées
                             colour = "#DC3545", linetype = "dashed", linewidth = 0.8) +
        
        ggplot2::geom_point(colour = "#7B61FF", alpha = 0.7) +
        ggrepel::geom_text_repel(force = 2, max.overlaps = Inf, min.segment.length = 0) +
        ggplot2::labs(
          x = socio_variable_labels[input$socio_variable],
          y = "Score Egapro moyen pondéré",
          size = "Nb d'entreprises"
        ) +
        ggplot2::theme_minimal(base_family = "Inter") +
        ggplot2::coord_cartesian(ylim = c(80, 100))
      
      # Conversion en plotly
      plotly::ggplotly(g, tooltip = "text") %>%
        plotly::layout(
          legend = list(orientation = 'h', y = -0.2, x = 0.5, xanchor = 'center')
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # --- BANNIÈRE DE CORRÉLATION AVEC NOUVELLES COULEURS SÉMANTIQUES ---
    output$corr_banner <- shiny::renderUI({
      df <- data_sd()
      if (nrow(df) < 3) return(NULL)
      
      # Protection contre les erreurs si les données sont insuffisantes
      if(any(is.na(df$socio_val)) || any(is.na(df$score_moyen))) return(NULL)
      ct <- stats::cor.test(df$socio_val, df$score_moyen)
      
      r  <- round(ct$estimate, 2)
      p  <- ct$p.value
      
      # --- NOUVELLE LOGIQUE DE COULEUR BASÉE SUR LES SEUILS STATISTIQUES ---
      couleur_bg <- dplyr::case_when(
        p < 0.05   ~ "#d1f3e0",  # Vert pastel pour "significatif"
        p < 0.10   ~ "#fff3cd",  # Jaune pastel pour "marginalement significatif"
        TRUE       ~ "#e9ecef"   # Gris neutre pour "non significatif"
      )
      couleur_text <- dplyr::case_when(
        p < 0.05   ~ "#0a3622",  # Texte foncé pour le vert
        p < 0.10   ~ "#664d03",  # Texte foncé pour le jaune
        TRUE       ~ "#495057"   # Texte standard pour le gris
      )
      
      tendance <- dplyr::case_when(
        r >  0.4 ~ "positive marquée",
        r >  0.15~ "positive faible",
        r < -0.4 ~ "négative marquée",
        r < -0.15~ "négative faible",
        TRUE     ~ "quasi nulle"
      )
      
      signification_texte <- dplyr::case_when(
        p < 0.05   ~ "Tendance statistiquement significative.",
        p < 0.10   ~ "Tendance marginalement significative.",
        TRUE       ~ "Tendance non significative."
      )
      
      shiny::div(
        style = paste0(
          "background-color:", couleur_bg, 
          "; color:", couleur_text,
          "; padding:12px; font-weight:600; border-radius:8px; margin-top:15px; text-align:center;"
        ),
        paste0(
          "Corrélation de Pearson : ", r, 
          " (p-value = ", formatC(p, digits = 3, format = "f"), "). ",
          "Tendance ", tendance, ". ",
          signification_texte
        )
      )
    })
    
    output$table_sd <- DT::renderDT({
      master_df_historique %>%
        dplyr::filter(annee == input$filtre_annee_socio, if (length(input$filtre_ept_sd) > 0) ept_name %in% input$filtre_ept_sd else TRUE) %>%
        dplyr::summarise(
          `Taux activité F` = mean(taux_activite_femmes, na.rm = TRUE),
          `Femmes cadres` = mean(part_femmes_cadres, na.rm = TRUE),
          `Femmes prof. inter.` = mean(part_femmes_prof_inter, na.rm = TRUE),
          `Taux féminisation cadres` = mean(taux_femmes_parmi_cadres,  na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_longer(everything(), names_to  = "Indicateur", values_to = "Valeur") %>%
        dplyr::mutate(Valeur = sprintf("%.1f %%", Valeur)) %>%
        DT::datatable(options = list(dom = "t"), rownames = FALSE)
    })
    
    output$alert_paris_sd <- shiny::renderUI({
      if ("Ville de Paris" %in% input$filtre_ept_sd) {
        shiny::tags$div(style = "margin-top:8px; font-size: 0.85em; color:#8a6d3b; background:#fcf8e3; border:1px solid #faebcc; border-radius:4px; padding:6px;",
                        shiny::icon("info-circle"), 
                        shiny::HTML("La <strong>Ville de Paris</strong> ne dispose pas<br> de données socio-démographiques<br> (sauf taux de féminisation des cadres).<br> Son inclusion peut biaiser les distributions<br> et les corrélations."))
      } else { NULL }
    })
  })
}