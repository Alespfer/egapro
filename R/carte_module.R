# ==============================================================================
# Module: Carte & Territoires (Version finale CORRIGÉE et ROBUSTE avec Zones d'Emploi)
# ==============================================================================

carte_ui <- function(id, df) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 3,
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     "Filtres de la carte",
                     bslib::tooltip(
                       shiny::actionButton(ns("show_download_modal"), label = NULL, icon = shiny::icon("download"), class = "btn-sm"),
                       "Exporter les données filtrées"
                     )
          )
        ),
        bslib::card_body(
          padding = "10px",
          bslib::accordion(
            open = c("Territoires", "Entreprises"),
            
            bslib::accordion_panel(
              title = "Filtres Territoriaux",
              icon = shiny::icon("location-dot"),
              value = "Territoires",
              shinyWidgets::sliderTextInput(
                inputId = ns("filtre_annee_carte"),
                label = "Année de l'Index :",
                choices = sort(unique(df$annee)),
                selected = max(df$annee),
                grid = TRUE,
                width = "100%"
              ),
              shiny::selectInput(ns("niveau_geo_carte"), "Niveau d'analyse :", choices = c("Départements", "Territoires (EPT)", "Zones d'emploi")),
              shiny::selectizeInput(ns("selection_territoires"), "Sélectionner des territoires :", choices = NULL, multiple = TRUE, options = list(placeholder = 'Tous les territoires par défaut'))
            ),
            
            bslib::accordion_panel(
              title = "Filtres sur les Entreprises",
              icon = shiny::icon("building"),
              value = "Entreprises",
              shiny::selectizeInput(ns("filtre_secteur_carte"), "Secteur d'activité :", choices = c("Tous les secteurs", sort(unique(na.omit(df$secteur_activite)))), multiple = TRUE, selected = "Tous les secteurs"),
              shiny::div(class = "d-flex align-items-center",
                         shiny::selectInput(ns("filtre_taille_carte"), "Taille d'entreprise :", choices = c("Toutes les tailles", unique(df$tranche_effectifs)), width = "90%"),
                         bslib::tooltip(
                           shiny::icon("info-circle"),
                           "L'Index est obligatoire pour les entreprises de 50 salariés et plus. Le calcul de certains indicateurs varie selon la tranche d'effectifs.",
                           placement = "right"
                         )
              ),
              shiny::sliderInput(ns("filtre_score_carte"), "Filtrer par Score Egapro :", min = 0, max = 100, value = c(0, 100))
            ),
            
            bslib::accordion_panel(
              title = "Options d'Affichage",
              icon = shiny::icon("eye"),
              value = "Affichage",
              shiny::radioButtons(ns("map_basemap"), "Choisir le fond de carte :", 
                                  choices = list("Clair (Recommandé)" = "CartoDB.Positron", 
                                                 "Détaillé (Satellite)" = "Esri.WorldImagery", 
                                                 "Plan de rues" = "OpenStreetMap.Mapnik"), 
                                  selected = "CartoDB.Positron")
            )
          )
        )
      )
    ),
    
    shiny::column(
      width = 9,
      shiny::uiOutput(ns("kpi_carte_ui")),
      bslib::card(
        bslib::card_header("Carte des Territoires et Entreprises"),
        bslib::card_body(
          padding = 0,
          leaflet::leafletOutput(ns("map"), height = "calc(100vh - 250px)")
        )
      )
    )
  )
}

carte_server <- function(id, master_df_historique, map_ept, map_dep, map_ze, palette_accessible, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(shared_state$selected_sector, {
      selected_sector <- shared_state$selected_sector
      shiny::req(selected_sector)
      shiny::updateSelectizeInput(session, "filtre_secteur_carte", selected = selected_sector)
      shared_state$selected_sector <- NULL
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    shiny::observeEvent(input$niveau_geo_carte, {
      col_name <- if (input$niveau_geo_carte == "Départements") {
        "dep_name"
      } else if (input$niveau_geo_carte == "Territoires (EPT)") {
        "ept_name"
      } else {
        "ze_name"
      }
      
      choix <- master_df_historique[[col_name]] %>% na.omit() %>% unique() %>% sort()
      shiny::updateSelectizeInput(session, "selection_territoires", choices = choix, selected = NULL)
    }, ignoreNULL = FALSE)
    
    data_annee_carte <- shiny::reactive({ 
      master_df_historique %>% dplyr::filter(annee == input$filtre_annee_carte) 
    })
    
    map_data_filtrée <- shiny::reactive({
      df_annee <- data_annee_carte()
      
      # --- CORRECTION FINALE ---: Stratégie de variable unique et claire
      if (input$niveau_geo_carte == "Départements") {
        group_var <- "dep_name"
        map_shape <- map_dep
      } else if (input$niveau_geo_carte == "Territoires (EPT)") {
        group_var <- "ept_name"
        map_shape <- map_ept
      } else { # Zones d'emploi
        group_var <- "ze_name"
        map_shape <- map_ze
      }
      
      agg_data <- df_annee %>%
        dplyr::filter(!is.na(.data[[group_var]])) %>%
        dplyr::group_by(name_col = .data[[group_var]]) %>%
        dplyr::summarise(score_moyen = weighted.mean(index, poids, na.rm = TRUE), .groups = "drop")
      
      df_map <- map_shape %>% 
        dplyr::left_join(agg_data, by = setNames("name_col", group_var))
      
      if (!is.null(input$selection_territoires) && length(input$selection_territoires) > 0) {
        df_map <- df_map %>% dplyr::filter(.data[[group_var]] %in% input$selection_territoires)
      }
      
      df_map %>% dplyr::filter(!is.na(score_moyen))
    })
    
    points_filtres_carte <- shiny::reactive({
      df <- data_annee_carte()
      if (!is.null(input$selection_territoires) && length(input$selection_territoires) > 0) {
        col_geo <- if (input$niveau_geo_carte == "Départements") "dep_name" else if (input$niveau_geo_carte == "Territoires (EPT)") "ept_name" else "ze_name"
        df <- df %>% dplyr::filter(.data[[col_geo]] %in% input$selection_territoires)
      }
      if (input$filtre_taille_carte != "Toutes les tailles") df <- df %>% dplyr::filter(tranche_effectifs == input$filtre_taille_carte)
      if (!is.null(input$filtre_secteur_carte) && !("Tous les secteurs" %in% input$filtre_secteur_carte)) df <- df %>% dplyr::filter(secteur_activite %in% input$filtre_secteur_carte)
      df %>% dplyr::filter(index >= input$filtre_score_carte[1] & index <= input$filtre_score_carte[2])
    })
    
    output$kpi_carte_ui <- shiny::renderUI({
      df_filtre <- points_filtres_carte()
      shiny::req(df_filtre)
      n_entreprises <- nrow(df_filtre)
      score_moyen_pondere <- if (n_entreprises > 0) {
        weighted.mean(df_filtre$index, df_filtre$poids, na.rm = TRUE)
      } else {
        NA
      }
      shiny::fluidRow(
        shiny::column(width = 6, bslib::value_box(title = "Entreprises dans la sélection", value = scales::comma(n_entreprises), showcase = shiny::icon("building"), theme = "secondary")),
        shiny::column(width = 6, bslib::value_box(title = "Score moyen pondéré", value = if (is.na(score_moyen_pondere)) "N/A" else round(score_moyen_pondere, 1), showcase = shiny::icon("balance-scale"), theme = "primary"))
      )
    })
    
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>% 
        leaflet::setView(lng = 2.55, lat = 48.80, zoom = 9) %>%
        leaflet::addProviderTiles("CartoDB.Positron", group = "Clair") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "Plan de rues")
    })
    
    shiny::observe({
      proxy <- leaflet::leafletProxy("map") %>%
        leaflet::hideGroup(c("Clair", "Satellite", "Plan de rues")) %>%
        leaflet::showGroup(switch(input$map_basemap, "CartoDB.Positron" = "Clair", "Esri.WorldImagery" = "Satellite", "OpenStreetMap.Mapnik" = "Plan de rues"))
      df_polygones <- map_data_filtrée()
      df_points <- points_filtres_carte()
      shiny::req(nrow(df_polygones) > 0)
      
      popup_name_col <- df_polygones[[names(df_polygones)[grepl("_name", names(df_polygones))] [1]]]
      
      pal <- leaflet::colorNumeric("YlGnBu", domain = c(75, 100), na.color = "#E0E0E0")
      popup_polygones <- paste0("<strong>", popup_name_col, "</strong><br>Score moyen : ", round(df_polygones$score_moyen, 1))
      popup_points <- if (nrow(df_points) > 0) { apply(df_points, 1, generate_company_popup) } else { NULL }
      proxy <- leaflet::leafletProxy("map", data = df_polygones) %>%
        leaflet::clearGroup(c("Vue Territoriale", "Vue Entreprises")) %>%
        leaflet::clearControls() %>%
        leaflet::fitBounds(lng1 = min(sf::st_bbox(df_polygones)[1]), lat1 = min(sf::st_bbox(df_polygones)[2]), lng2 = max(sf::st_bbox(df_polygones)[3]), lat2 = max(sf::st_bbox(df_polygones)[4])) %>%
        leaflet::addPolygons(fillColor = ~pal(score_moyen), fillOpacity = 0.7, weight = 1.5, color = "white", group = "Vue Territoriale", popup = ~lapply(popup_polygones, shiny::HTML)) %>%
        leaflet::addLegend(pal = pal, values = ~score_moyen, title = "Score Moyen", position = "bottomright")
      if (!is.null(popup_points)) {
        cluster_opts <- leaflet::markerClusterOptions()
        proxy <- proxy %>% leaflet::addCircleMarkers(data = df_points, lng = ~longitude, lat = ~latitude, popup = popup_points, radius = 7, color = "#e41a1c", weight = 1.5, fillColor = "#fb9a99", fillOpacity = 0.85, clusterOptions = cluster_opts, group = "Vue Entreprises")
      }
      proxy %>% leaflet::addLayersControl(overlayGroups = c("Vue Territoriale", "Vue Entreprises"), options = leaflet::layersControlOptions(collapsed = FALSE, position = "topright"))
    })
    
    shiny::observeEvent(input$show_download_modal, {
      shiny::showModal(shiny::modalDialog(
        title = "Exporter les Données Filtrées",
        shiny::p("Choisissez le format pour télécharger la liste des entreprises actuellement visibles sur la carte."),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::downloadButton(session$ns("download_csv"), "Télécharger (CSV)", class = "btn-primary"),
          shiny::downloadButton(session$ns("download_excel"), "Télécharger (Excel)", class = "btn-success")
        )
      ))
    })
    
    output$download_csv <- shiny::downloadHandler(
      filename = function() { paste0("donnees_filtrees_carte_", Sys.Date(), ".csv") },
      content = function(file) { utils::write.csv(points_filtres_carte(), file, row.names = FALSE, fileEncoding = "UTF-8") }
    )
    
    output$download_excel <- shiny::downloadHandler(
      filename = function() { paste0("donnees_filtrees_carte_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(points_filtres_carte(), file) }
    )
    
  })
}