socio_dem_ui <- function(id, df) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::h4("Filtres"),
      shiny::sliderInput(ns("filtre_annee_socio"), "Année :", min = min(df$annee), max = max(df$annee), value = max(df$annee), step  = 1, sep = ""),
      shiny::selectInput(ns("socio_variable"), "Indicateur :",
                         choices = list("Structure emploi féminin" = c(`Part des femmes cadres` = "part_femmes_cadres", `Part des femmes prof. inter.` = "part_femmes_prof_inter"),
                                        "Mixité & activité" = c(`Taux de féminisation des cadres` = "taux_femmes_parmi_cadres", `Taux d'activité des femmes 15-64`= "taux_activite_femmes"))
                         , selected = "part_femmes_cadres"),
      shiny::selectizeInput(ns("filtre_ept_sd"), "Territoires (EPT) :", choices  = sort(unique(df$ept_name)), multiple = TRUE, options = list(placeholder = "Tous")),
      shiny::uiOutput(ns("alert_paris_sd"))      
    ),
    shiny::mainPanel(width = 9, shiny::uiOutput(ns("titre_socio_ui")), shiny::plotOutput(ns("plot_socio"), height = "550px"), shiny::uiOutput(ns("corr_banner")), DT::DTOutput(ns("table_sd")))
  )
}

socio_dem_server <- function(id, master_df_historique, socio_variable_labels) {
  shiny::moduleServer(id, function(input, output, session) {
    data_sd <- shiny::reactive({
      master_df_historique |>
        dplyr::filter(
          annee == input$filtre_annee_socio,
          if (length(input$filtre_ept_sd) > 0)
            ept_name %in% input$filtre_ept_sd else TRUE
        ) |>
        dplyr::group_by(ept_name) |>
        dplyr::summarise(
          score_moyen = weighted.mean(index, poids, na.rm = TRUE),
          socio_val   = mean(.data[[input$socio_variable]], na.rm = TRUE),
          n           = dplyr::n(),
          .groups     = "drop"
        )
    })
    output$titre_socio_ui <- shiny::renderUI({ shiny::h3(paste("Score Egapro moyen vs.", socio_variable_labels[input$socio_variable])) })
    output$plot_socio <- shiny::renderPlot({
      df <- data_sd()
      shiny::validate(shiny::need(nrow(df) > 0, "Aucune donnée disponible."))
      ggplot2::ggplot(df, ggplot2::aes(x = socio_val, y = score_moyen)) +
        ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "#E74C3C", linetype = "dashed") +
        ggplot2::geom_point(ggplot2::aes(size = n), colour = "#2980B9", alpha = .8) +
        ggrepel::geom_text_repel(ggplot2::aes(label = ept_name)) +
        ggplot2::labs(
          x    = socio_variable_labels[input$socio_variable],
          y    = "Score Egapro moyen pondéré",
          size = "Nb d'entreprises",
          caption = "Ligne rouge : régression linéaire."
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::coord_cartesian(ylim = c(80, 100))
    })
    output$corr_banner <- shiny::renderUI({
      df <- data_sd()
      if (nrow(df) < 3) return(NULL)
      # cor.test peut produire des NaN si les données sont insuffisantes, on le protège
      if(any(is.na(df$socio_val)) || any(is.na(df$score_moyen))) return(NULL)
      ct <- stats::cor.test(df$socio_val, df$score_moyen)
      r  <- round(ct$estimate, 2)
      p  <- ct$p.value
      couleur <- dplyr::case_when(p < .001 ~ "#1a9850", p < .05  ~ "#66bd63", p < .1   ~ "#fdae61", TRUE ~ "#bdbdbd")
      tendance <- dplyr::case_when(r >  .4 ~ "positive marquée", r >  .15~ "positive faible", r < -.4 ~ "négative marquée", r < -.15~ "négative faible", TRUE ~ "quasi nulle")
      shiny::div(style = paste0("background:", couleur, ";color:white;padding:8px;font-weight:600;", "border-radius:6px;margin-bottom:10px;"),
                 paste0("Corrélation de Pearson : ", r, " (p-value = ", formatC(p, digits = 3, format = "f"), "). Tendance ", tendance, "."))
    })
    output$table_sd <- DT::renderDT({
      master_df_historique |>
        dplyr::filter(annee == input$filtre_annee_socio, if (length(input$filtre_ept_sd) > 0) ept_name %in% input$filtre_ept_sd else TRUE) |>
        dplyr::summarise(
          `Taux activité F` = mean(taux_activite_femmes, na.rm = TRUE),
          `Femmes cadres` = mean(part_femmes_cadres, na.rm = TRUE),
          `Femmes prof. inter.` = mean(part_femmes_prof_inter, na.rm = TRUE),
          `Taux féminisation cadres` = mean(taux_femmes_parmi_cadres,  na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_longer(everything(), names_to  = "Indicateur", values_to = "Valeur") |>
        dplyr::mutate(Valeur = sprintf("%.1f %%", Valeur)) |>
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