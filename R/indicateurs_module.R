# ==============================================================================
# Module: Analyse des Indicateurs (Version finale CORRIGÉE et ROBUSTE)
# ==============================================================================

indicateurs_ui <- function(id, df) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 3,
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     "Filtres de l'analyse",
                     bslib::tooltip(
                       shiny::actionButton(ns("show_download_modal"), 
                                           label = NULL, 
                                           icon = shiny::icon("download"), 
                                           class = "btn-sm"),
                       "Exporter les données agrégées"
                     )
          )
        ),
        bslib::card_body(
          padding = "10px",
          # --- AJOUT ---: Implémentation de l'accordéon statique
          bslib::accordion(
            open = TRUE,
            bslib::accordion_panel(
              title = "Options de Filtrage",
              icon = shiny::icon("filter"),
          shinyWidgets::sliderTextInput(
            inputId = ns("filtre_annee_indicateur"),
            label = "Année :",
            choices = sort(unique(df$annee)),
            selected = max(df$annee),
            grid = TRUE,
            width = "100%"
          ),
          shiny::selectInput(
            inputId = ns("select_indicateur"),
            label = "Choisir un indicateur à analyser :",
            choices = indicateur_labels,
            selected = "note_remuneration"
          ),
          shiny::uiOutput(ns("indicateur_description_ui")),
          shiny::selectInput(
            inputId = ns("niveau_geo_indicateur"),
            label = "Niveau d'analyse territorial :",
            choices = c("Départements", "Territoires (EPT)", "Zones d'emploi"),
            selected = "Territoires (EPT)"
          )
        )
      )
        )
      )
    ),
    
    shiny::column(
      width = 9,
      bslib::card(
        bslib::card_header("Performance Territoriale sur l'Indicateur Sélectionné"),
        bslib::card_body(
          padding = 0,
          leaflet::leafletOutput(ns("map_indicateur"), height = "450px")
        )
      ),
      bslib::card(
        bslib::card_header("Performance Sectorielle sur l'Indicateur Sélectionné"),
        bslib::card_body(
          plotly::plotlyOutput(ns("plot_sectoriel_indicateur"), height = "450px")
        )
      )
    )
  )
}

indicateurs_server <- function(id, master_df_historique, map_ept, map_dep, map_ze) {
  shiny::moduleServer(id, function(input, output, session) {
    
    data_filtree <- shiny::reactive({
      shiny::req(input$select_indicateur)
      master_df_historique %>%
        dplyr::filter(annee == input$filtre_annee_indicateur, !is.na(.data[[input$select_indicateur]]))
    })
    
    output$indicateur_description_ui <- shiny::renderUI({
      shiny::req(input$select_indicateur)
      description <- indicateur_descriptions[[input$select_indicateur]]
      shiny::div(
        style = "font-size: 0.85em; background-color: #f8f9fa; border-radius: 5px; padding: 10px; margin-top: -10px; margin-bottom: 15px;",
        shiny::HTML(description)
      )
    })
    
    data_agg_territoriale <- shiny::reactive({
      group_var <- if (input$niveau_geo_indicateur == "Départements") "dep_name" else if (input$niveau_geo_indicateur == "Territoires (EPT)") "ept_name" else "ze_name"
      data_filtree() %>%
        dplyr::filter(!is.na(.data[[group_var]])) %>%
        dplyr::group_by(Territoire = .data[[group_var]]) %>%
        dplyr::summarise(
          `Note Moyenne` = mean(.data[[input$select_indicateur]], na.rm = TRUE),
          `Nombre d'entreprises` = dplyr::n(),
          .groups = "drop"
        )
    })
    
    data_agg_sectorielle <- shiny::reactive({
      data_filtree() %>%
        dplyr::group_by(Secteur = secteur_activite) %>%
        dplyr::summarise(
          `Note Moyenne` = mean(.data[[input$select_indicateur]], na.rm = TRUE),
          `Nombre d'entreprises` = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::filter(`Nombre d'entreprises` >= 10)
    })
    
    output$map_indicateur <- leaflet::renderLeaflet({
      agg_data <- data_agg_territoriale()
      
      # --- CORRECTION FINALE ---: Stratégie de variable unique et claire
      if (input$niveau_geo_indicateur == "Départements") {
        map_shape <- map_dep
        group_var <- "dep_name"
      } else if (input$niveau_geo_indicateur == "Territoires (EPT)") {
        map_shape <- map_ept
        group_var <- "ept_name"
      } else {
        map_shape <- map_ze
        group_var <- "ze_name"
      }
      
      map_final <- map_shape %>%
        dplyr::left_join(agg_data, by = setNames("Territoire", group_var)) %>%
        dplyr::filter(!is.na(`Note Moyenne`))
      
      shiny::validate(shiny::need(nrow(map_final) > 0, "Aucune donnée territoriale à afficher pour cette sélection."))
      
      indicateur_nom <- names(indicateur_labels)[indicateur_labels == input$select_indicateur]
      max_points <- as.numeric(stringr::str_extract(indicateur_nom, "\\d+"))
      pal <- leaflet::colorNumeric("viridis", domain = c(0, max_points), na.color = "#E0E0E0")
      popup_name_col <- map_final[[group_var]]
      popup_polygones <- paste0("<strong>", popup_name_col, "</strong><br>Note moyenne : ", round(map_final$`Note Moyenne`, 1), "/", max_points)
      
      leaflet::leaflet(data = map_final) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::fitBounds(lng1 = 2.0, lat1 = 48.6, lng2 = 2.7, lat2 = 49.1) %>%
        leaflet::addPolygons(fillColor = ~pal(`Note Moyenne`), fillOpacity = 0.8, weight = 1.5, color = "white", popup = ~lapply(popup_polygones, shiny::HTML)) %>%
        leaflet::addLegend(pal = pal, values = ~`Note Moyenne`, title = "Note Moyenne", position = "bottomright")
    })
    
    output$plot_sectoriel_indicateur <- plotly::renderPlotly({
      summary_secteur <- data_agg_sectorielle() %>% dplyr::arrange(`Note Moyenne`)
      shiny::validate(shiny::need(nrow(summary_secteur) > 0, "Pas assez de données sectorielles à afficher (seuil de 10 entreprises min. par secteur)."))
      indicateur_nom <- names(indicateur_labels)[indicateur_labels == input$select_indicateur]
      max_points <- as.numeric(stringr::str_extract(indicateur_nom, "\\d+"))
      
      g <- ggplot2::ggplot(summary_secteur,
                           ggplot2::aes(x = `Note Moyenne`,
                                        y = reorder(stringr::str_wrap(Secteur, 40), `Note Moyenne`),
                                        text = sprintf("<b>Secteur :</b> %s<br><b>Note moyenne :</b> %.1f / %d",
                                                       Secteur, `Note Moyenne`, max_points))) +
        ggplot2::geom_col(fill = "#7B61FF", width = 0.7) + 
        ggplot2::labs(x = paste("Note moyenne sur", max_points), y = NULL) +
        ggplot2::coord_cartesian(xlim = c(0, max_points)) + 
        ggplot2::theme_minimal(base_family = "Inter") +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_line(linetype = "dashed", color = "gray80"),
          axis.text.y = ggplot2::element_text(face = "bold")
        )
      
      plotly::ggplotly(g, tooltip = "text") %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    shiny::observeEvent(input$show_download_modal, {
      shiny::showModal(shiny::modalDialog(
        title = "Exporter les Données Agrégées",
        shiny::p("Choisissez les données agrégées que vous souhaitez télécharger, en fonction des filtres actuels."),
        shiny::h4("Données Territoriales (Carte)"),
        shiny::div(
          shiny::downloadButton(session$ns("download_terr_csv"), "Télécharger (CSV)", class = "btn-primary btn-sm"),
          shiny::downloadButton(session$ns("download_terr_excel"), "Télécharger (Excel)", class = "btn-success btn-sm")
        ),
        shiny::hr(),
        shiny::h4("Données Sectorielles (Graphique)"),
        shiny::div(
          shiny::downloadButton(session$ns("download_sec_csv"), "Télécharger (CSV)", class = "btn-primary btn-sm"),
          shiny::downloadButton(session$ns("download_sec_excel"), "Télécharger (Excel)", class = "btn-success btn-sm")
        ),
        footer = shiny::modalButton("Annuler"),
        easyClose = TRUE
      ))
    })
    
    output$download_terr_csv <- shiny::downloadHandler(
      filename = function() { paste0("agregation_territoriale_", input$select_indicateur, "_", Sys.Date(), ".csv") },
      content = function(file) { utils::write.csv(data_agg_territoriale(), file, row.names = FALSE, fileEncoding = "UTF-8") }
    )
    output$download_terr_excel <- shiny::downloadHandler(
      filename = function() { paste0("agregation_territoriale_", input$select_indicateur, "_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(data_agg_territoriale(), file) }
    )
    
    output$download_sec_csv <- shiny::downloadHandler(
      filename = function() { paste0("agregation_sectorielle_", input$select_indicateur, "_", Sys.Date(), ".csv") },
      content = function(file) { utils::write.csv(data_agg_sectorielle(), file, row.names = FALSE, fileEncoding = "UTF-8") }
    )
    output$download_sec_excel <- shiny::downloadHandler(
      filename = function() { paste0("agregation_sectorielle_", input$select_indicateur, "_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(data_agg_sectorielle(), file) }
    )
    
  })
}