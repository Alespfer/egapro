carte_ui <- function(id, df) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
                        shiny::h4("Filtres de la carte"),
                        shiny::sliderInput(ns("filtre_annee_carte"), "Année de l'Index :", min = min(df$annee), max = max(df$annee), value = max(df$annee), step = 1, sep = ""),
                        shiny::selectInput(ns("niveau_geo_carte"), "Niveau d'analyse :", choices = c("Départements", "Territoires (EPT)")),
                        shiny::selectizeInput(ns("selection_territoires"), "Sélectionner des territoires :", choices = NULL, multiple = TRUE, options = list(placeholder = 'Tous les territoires par défaut')),
                        shiny::hr(),
                        shiny::h4("Filtres sur les entreprises"),
                        shiny::selectizeInput(ns("filtre_secteur_carte"), "Secteur d'activité :", choices = c("Tous les secteurs", sort(unique(na.omit(df$secteur_activite)))), multiple = TRUE, selected = "Tous les secteurs"),
                        shiny::selectInput(ns("filtre_taille_carte"), "Taille d'entreprise :", choices = c("Toutes les tailles", unique(df$tranche_effectifs))),
                        shiny::sliderInput(ns("filtre_score_carte"), "Filtrer par Score Egapro :", min = 0, max = 100, value = c(0, 100)),
                        shiny::hr(),
                        shiny::h4("Options d'affichage"),
                        shiny::radioButtons(ns("map_basemap"), "Choisir le fond de carte :", choices = list("Clair (Recommandé)" = "CartoDB.Positron", "Détaillé (Satellite)" = "Esri.WorldImagery", "Plan de rues" = "OpenStreetMap.Mapnik"), selected = "CartoDB.Positron"),
                        shiny::hr(),
                        shiny::uiOutput(ns("compteur_entreprises_ui")),
                        shiny::downloadButton(ns("download_filtered_map"), "Exporter les données filtrées (CSV)", class = "btn-success")
    ),
    shiny::mainPanel(width = 9, leaflet::leafletOutput(ns("map"), height = "80vh"), color_switch_ui(ns("color_switch_carte")))
  )
}

carte_server <- function(id, master_df_historique, map_ept, map_dep, palette_accessible) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$niveau_geo_carte, {
      choix <- if (input$niveau_geo_carte == "Départements") sort(unique(master_df_historique$dep_name)) else sort(unique(master_df_historique$ept_name))
      shiny::updateSelectizeInput(session, "selection_territoires", choices = choix)
    }, ignoreNULL = FALSE)
    data_annee_carte <- shiny::reactive({ master_df_historique |> dplyr::filter(annee == input$filtre_annee_carte) })
    map_data_filtrée <- shiny::reactive({
      df_annee <- data_annee_carte()
      if (input$niveau_geo_carte == "Départements") {
        agg_data <- df_annee |> dplyr::group_by(code = dep_code, name = dep_name) |> dplyr::summarise(score_moyen = weighted.mean(index, poids, na.rm = TRUE), .groups = "drop")
        df_map <- map_dep |> dplyr::left_join(agg_data, by = c("dep_code" = "code"))
        if (!is.null(input$selection_territoires) && length(input$selection_territoires) > 0) df_map <- df_map |> dplyr::filter(dep_name %in% input$selection_territoires)
      } else {
        agg_data <- df_annee |> dplyr::group_by(code = ept_code, name = ept_name) |> dplyr::summarise(score_moyen = weighted.mean(index, poids, na.rm = TRUE), .groups = "drop")
        df_map <- map_ept |> dplyr::left_join(agg_data, by = c("ept_code" = "code"))
        if (!is.null(input$selection_territoires) && length(input$selection_territoires) > 0) df_map <- df_map |> dplyr::filter(ept_name %in% input$selection_territoires)
      }
      df_map |> dplyr::filter(!is.na(score_moyen))
    })
    points_filtres_carte <- shiny::reactive({
      df <- data_annee_carte()
      if (!is.null(input$selection_territoires) && length(input$selection_territoires) > 0) {
        col_geo <- if(input$niveau_geo_carte == "Départements") "dep_name" else "ept_name"
        df <- df |> dplyr::filter(.data[[col_geo]] %in% input$selection_territoires)
      }
      if (input$filtre_taille_carte != "Toutes les tailles") df <- df |> dplyr::filter(tranche_effectifs == input$filtre_taille_carte)
      if (!is.null(input$filtre_secteur_carte) && !("Tous les secteurs" %in% input$filtre_secteur_carte)) df <- df |> dplyr::filter(secteur_activite %in% input$filtre_secteur_carte)
      df |> dplyr::filter(index >= input$filtre_score_carte[1] & index <= input$filtre_score_carte[2])
    })
    output$compteur_entreprises_ui <- shiny::renderUI({
      n_entreprises <- nrow(points_filtres_carte())
      shiny::tags$p(style="text-align: center; font-size: 1.1em; color: #333;", shiny::HTML(paste0("<strong>", format(n_entreprises, big.mark=" "), "</strong> entreprises affichées")))
    })
    output$download_filtered_map <- shiny::downloadHandler(
      filename = function() { paste0("donnees_filtrees_carte_", Sys.Date(), ".csv") },
      content = function(file) { utils::write.csv(points_filtres_carte(), file, row.names = FALSE, fileEncoding = "UTF-8") }
    )
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |> 
        leaflet::setView(lng = 2.35, lat = 48.85, zoom = 10) |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Clair") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "Plan de rues")
    })
    shiny::observe({
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::hideGroup(c("Clair", "Satellite", "Plan de rues")) |>
        leaflet::showGroup(switch(input$map_basemap, "CartoDB.Positron" = "Clair", "Esri.WorldImagery" = "Satellite", "OpenStreetMap.Mapnik" = "Plan de rues"))
      df_polygones <- map_data_filtrée()
      df_points <- points_filtres_carte()
      shiny::req(nrow(df_polygones) > 0)
      pal <- leaflet::colorNumeric("YlGnBu", domain = c(75, 100), na.color = "#E0E0E0")
      popup_polygones <- paste0("<strong>", df_polygones$name, "</strong><br>Score moyen : ", round(df_polygones$score_moyen, 1))
      popup_points <- if (nrow(df_points) > 0) { apply(df_points, 1, generate_company_popup) } else { NULL }
      proxy <- leaflet::leafletProxy("map", data = df_polygones) |>
        leaflet::clearGroup(c("Vue Territoriale", "Vue Entreprises")) |>
        leaflet::clearControls() |>
        leaflet::fitBounds(lng1 = min(sf::st_bbox(df_polygones)[1]), lat1 = min(sf::st_bbox(df_polygones)[2]), lng2 = max(sf::st_bbox(df_polygones)[3]), lat2 = max(sf::st_bbox(df_polygones)[4])) |>
        leaflet::addPolygons(fillColor = ~pal(score_moyen), fillOpacity = 0.7, weight = 1.5, color = "white", group = "Vue Territoriale", popup = ~lapply(popup_polygones, shiny::HTML)) |>
        leaflet::addLegend(pal = pal, values = ~score_moyen, title = "Score Moyen", position = "bottomright")
      if (!is.null(popup_points)) {
        point_fill <- if (isTRUE(input$color_switch_carte)) "black" else "#fb9a99"
        point_border <- if (isTRUE(input$color_switch_carte)) "black" else "#e41a1c"
        cluster_opts <- if (isTRUE(input$color_switch_carte)) {
          palette_js <- jsonlite::toJSON(palette_accessible)
          htmlwidgets::JS(sprintf(
            "function(cluster) {
        var count  = cluster.getChildCount();
        var index  = count %% %d;
        var colors = %s;
        return new L.DivIcon({
          html: '<div style=\"background-color:' + colors[index] +
                '; color:#222; font-weight:600; text-shadow:0 0 2px rgba(255,255,255,.4); ' +
                'border-radius:20px; padding:4px 10px; font-size:13px;\">' +
                count + '</div>',
          className: 'marker-cluster',
          iconSize: new L.Point(40, 40)
        }); 
     }",
            length(palette_accessible), palette_js
          ))
        } else { NULL }
        proxy <- proxy |> leaflet::addCircleMarkers(data = df_points, lng = ~longitude, lat = ~latitude, popup = popup_points, radius = 7, color = point_border, weight = 1.5, fillColor = point_fill, fillOpacity = 0.85, clusterOptions = leaflet::markerClusterOptions(iconCreateFunction = cluster_opts), group = "Vue Entreprises")
      }
      proxy |> leaflet::addLayersControl(overlayGroups = c("Vue Territoriale", "Vue Entreprises"), options = leaflet::layersControlOptions(collapsed = FALSE, position = "topright"))
    })
  })
}