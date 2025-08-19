# ==============================================================================
# Application Shiny : Baromètre de la Parité en Entreprise – Grand Paris
# Auteur : Alberto Esperon
# Master 2 PISE, Promotion 2024-2025
# Version Finale
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Chargement des librairies et des données
# ------------------------------------------------------------------------------

# -- Librairies --
library(shiny)
library(shinythemes)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(DT)
library(htmltools)
library(scales)
library(stringr)
library(ggrepel)
library(tidyr)
library(RColorBrewer)
library(plotly)
library(shinyWidgets)
library(jsonlite)
library(viridis)
library(shinydashboard)

# -- Fonctions et alias --
validate <- shiny::validate
need     <- shiny::need
source("utils.R", local = TRUE)

# -- Données --
master_df_historique <- readRDS("data_shiny/master_df_historique.RDS")
map_ept <- readRDS("data_shiny/map_ept.RDS")
map_dep <- readRDS("data_shiny/map_dep.RDS")

# ------------------------------------------------------------------------------
# 2. Pré-calculs et objets globaux
# ------------------------------------------------------------------------------

# Transformation des fonds de carte en WGS84
map_ept <- st_transform(map_ept, crs = 4326)
map_dep <- st_transform(map_dep, crs = 4326)

# Vecteur pour la pondération par effectifs
eff_key <- c("50 à 250" = 150, "251 à 999" = 625, "1000 et plus" = 1500)
master_df_historique <- master_df_historique %>%
  mutate(poids = eff_key[tranche_effectifs] %>% unname())

# Palette de couleurs accessible
palette_accessible <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Labels pour les variables socio-démographiques
socio_variable_labels <- c(
  "part_femmes_cadres"        = "Part de femmes cadres parmi les femmes actives (%)",
  "part_femmes_prof_inter"    = "Part de femmes en prof. inter. parmi les femmes actives (%)",
  "taux_femmes_parmi_cadres"  = "Taux de féminisation des postes de cadres (%)",
  "taux_activite_femmes"      = "Taux d'activité des femmes 15-64 ans (%)"
)


# ------------------------------------------------------------------------------
# 3. Contenu de la documentation (statique)
# ------------------------------------------------------------------------------

documentation_content <- fluidPage(
  style = "padding: 20px; max-width: 900px; margin: auto;",
  
  h3(strong("Mission du Baromètre")),
  p("Le « Baromètre de la Parité en Entreprise – Territoires du Grand Paris » est un outil interactif d'aide à la décision conçu pour éclairer les politiques publiques en matière d'égalité professionnelle. Il permet de visualiser, comparer et suivre les performances des entreprises du Grand Paris via l'Index Egapro, en les mettant en perspective avec les dynamiques socio-économiques locales."),
  p("Il s'adresse principalement aux :"),
  tags$ul(
    tags$li("Décideurs politiques (élus régionaux, départementaux et municipaux)"),
    tags$li("Équipes de développement économique territorial (EPT, intercommunalités)"),
    tags$li("Chargés de mission Égalité, Diversité et RSE au sein des collectivités.")
  ),
  br(),
  
  h3(strong("Principes Clés & Méthodologie")),
  
  h4("1. L'Indicateur Central : L'Index Egapro"),
  p("Le cœur de l'outil repose sur les données de l'", tags$a(href = "https://www.data.gouv.fr/fr/datasets/index-egalite-professionnelle-f-h-des-entreprises-de-50-salaries-ou-plus/", "Index de l'Égalité Professionnelle Femmes-Hommes", target = "_blank"), ", publiées par le Ministère du Travail. Cet index (sur 100 points) est obligatoire pour les entreprises de plus de 50 salariés et agrège cinq indicateurs :"),
  tags$ul(
    tags$li("Écarts de rémunération,"),
    tags$li("Écarts dans les taux d'augmentations individuelles,"),
    tags$li("Écarts dans les taux de promotions,"),
    tags$li("Pourcentage de salariées augmentées au retour de leur congé maternité,"),
    tags$li("Nombre de femmes parmi les 10 plus hautes rémunérations.")
  ),
  
  h4("2. Enrichissement Socio-démographique (INSEE)"),
  p("Afin de mettre en perspective la performance des entreprises avec le contexte social de leur territoire, les données sont croisées avec des indicateurs issus du ", tags$a(href = "https://www.insee.fr/fr/statistiques/8268843#:~:text=Activit%C3%A9%20des%20r%C3%A9sidents%20en%202021%20Recensement,population%20%2D%20Base%20infracommunale%20(IRIS)&text=La%20base%20infracommunale%20%C2%AB%20Activit%C3%A9%20des,salari%C3%A9s%20(sexe%20et%20%C3%A2ge).", "Recensement de la Population (base « Activité des résidents »)", target = "_blank"), ". Ces données permettent d'analyser les liens potentiels entre le marché du travail local et les pratiques des entreprises."),
  
  h4("3. Périmètre & Localisation des Entreprises (SIRENE)"),
  p("Pour garantir la pertinence de l'analyse, le baromètre se concentre sur les sièges sociaux des entreprises de 50 salariés et plus, localisés en Île-de-France. La géolocalisation provient de la base", tags$a(href = "https://www.data.gouv.fr/fr/datasets/base-sirene-des-entreprises-et-de-leurs-etablissements-siren-siret/", "SIRENE", target = "_blank"), " de l'INSEE."),
  
  h4("4. Pondération des Scores et choix méthodologique pour la tranche '1000 et plus'"),
  p("Pour que les moyennes calculées reflètent fidèlement la réalité économique, le score de chaque entreprise est pondéré par une estimation de sa taille. Pour les tranches '50-250' et '251-999', nous utilisons leur point-milieu (150 et 625)."),
  p("Pour la tranche ouverte '1000 et plus', un poids forfaitaire de 1500 est utilisé comme estimation pragmatique de l'effectif. Ce choix méthodologique est jugé robuste car il s'ancre dans les données sources, 1500 étant le point-milieu de la première sous-catégorie disponible ('1000 à 1999 salariés'), où se concentrent la majorité de ces entreprises. C'est également un choix conservateur qui, tout en restant significativement supérieur au poids de la tranche précédente, évite de donner une influence disproportionnée aux quelques très grands groupes, ce qui préserve l'équilibre des moyennes territoriales."),
  br(),
  
  h3(strong("Guide d'Utilisation des Onglets")),
  p(strong("Carte & Territoires :"), " Visualisez la géographie de la parité. Comparez les performances moyennes des départements ou des EPT, ainsi que celles des établissements ayant fourni des données Egapro."),
  p(strong("Analyse Sectorielle :"), " Identifiez les secteurs d'activité les plus et les moins performants en termes de parité. Comparez la médiane des scores pour comprendre les dynamiques propres à chaque branche."),
  p(strong("Socio-démographique :"), " Explorez les liens potentiels entre le score Egapro moyen d'un territoire et son profil socio-économique."),
  p(strong("Historique & Évolutions :"), " Suivez les tendances pluriannuelles. Observez si les scores s'améliorent au fil du temps dans les territoires sélectionnés."),
  br(),
  
  h3(strong("Points de Vigilance")),
  tags$ul(
    tags$li(strong("Maille territoriale :"), " L'analyse privilégie les Établissements Publics Territoriaux (EPT) à la commune. Ce choix garantit un nombre suffisant d'entreprises par territoire pour calculer des moyennes robustes et significatives."),
    tags$li(strong("Données socio-démographiques de Paris :"), " La Ville de Paris, en tant que département-commune, ne dispose pas des mêmes données socio-démographiques que les autres EPT. Son inclusion dans l'onglet 'Socio-démographique' peut donc biaiser les analyses de corrélation."),
    tags$li(strong("Corrélation n'est pas causalité :"), " L'outil peut révéler des liens statistiques entre des variables, mais ne permet pas d'établir des relations de cause à effet.")
  ),
  br(),
  hr(),
  
  h3(strong("Crédits")),
  p("Conception et développement : Alberto Esperon.", br(),
    "Projet réalisé dans le cadre du Master 2 PISE (Promotion 2024-2025).")
)


# ==============================================================================
# INTERFACE UTILISATEUR (UI)
# ==============================================================================
ui <- fluidPage(
  theme = "bootstrap",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Baromètre de la Parité en Entreprise – Territoires du Grand Paris"),
  
  navbarPage("Navigation",
             
             # ────────── 1. CARTE & TERRITOIRES ────────────────────────────────────────────
             tabPanel("Carte & Territoires", icon = icon("map-marked-alt"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h4("Filtres de la carte"),
                                     sliderInput("filtre_annee_carte", "Année de l'Index :", min = min(master_df_historique$annee), max = max(master_df_historique$annee), value = max(master_df_historique$annee), step = 1, sep = ""),
                                     selectInput("niveau_geo_carte", "Niveau d'analyse :", choices = c("Départements", "Territoires (EPT)")),
                                     selectizeInput("selection_territoires", "Sélectionner des territoires :", choices = NULL, multiple = TRUE, options = list(placeholder = 'Tous les territoires par défaut')),
                                     hr(),
                                     h4("Filtres sur les entreprises"),
                                     selectizeInput("filtre_secteur_carte", "Secteur d'activité :", choices = c("Tous les secteurs", sort(unique(na.omit(master_df_historique$secteur_activite)))), multiple = TRUE, selected = "Tous les secteurs"),
                                     selectInput("filtre_taille_carte", "Taille d'entreprise :", choices = c("Toutes les tailles", unique(master_df_historique$tranche_effectifs))),
                                     sliderInput("filtre_score_carte", "Filtrer par Score Egapro :", min = 0, max = 100, value = c(0, 100)),
                                     hr(),
                                     h4("Options d'affichage"),
                                     radioButtons("map_basemap", "Choisir le fond de carte :",
                                                  choices = list("Clair (Recommandé)" = "CartoDB.Positron", "Détaillé (Satellite)" = "Esri.WorldImagery", "Plan de rues" = "OpenStreetMap.Mapnik"),
                                                  selected = "CartoDB.Positron"),
                                     hr(),
                                     uiOutput("compteur_entreprises_ui"),
                                     downloadButton("download_filtered_map", "Exporter les données filtrées (CSV)", class = "btn-success")
                        ),
                        mainPanel(width = 9, leafletOutput("map", height = "80vh"), color_switch_ui("color_switch_carte"))
                      )
             ),
             
             # ────────── 2. ANALYSE SECTORIELLE ────────────────────────────────────────────
             tabPanel("Analyse Sectorielle", icon = icon("industry"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h4("Filtres de l'analyse"),
                                     sliderInput("filtre_annee_secteur", "Année :", min = min(master_df_historique$annee), max = max(master_df_historique$annee), value = max(master_df_historique$annee), step = 1, sep = ""),
                                     selectInput("filtre_taille_secteur", "Taille d'entreprise :", choices = c("Toutes les tailles", unique(master_df_historique$tranche_effectifs))),
                                     selectizeInput("filtre_secteurs", "Secteur(s) d'activité :", choices = sort(unique(na.omit(master_df_historique$secteur_activite))), multiple = TRUE, options  = list(placeholder = "Top/Bottom 5", maxItems = 15)),
                                     tags$small(
                                       class = "text-muted",
                                       icon("info-circle"), 
                                       "L'affichage par défaut montre les 5 meilleurs et 5 moins bons secteurs",
                                       "ayant au moins 10 entreprises déclarantes pour la période sélectionnée."
                                     ),
                                     checkboxInput("afficher_tous_secteurs", "Afficher tous les secteurs", value = FALSE)
                        ),
                        mainPanel(width = 9, h3("Performance par Secteur d'Activité"), uiOutput("plot_sector_ui"), color_switch_ui("color_switch_secteur"), hr(), h4("Tableau de synthèse"), DT::dataTableOutput("table_secteur"))
                      )
             ),
                          # ───── 3. ANALYSE SOCIO-DÉMOGRAPHIQUE ────────────────────────────────────
             tabPanel("Socio-démographique", icon = icon("users"),
                      sidebarLayout(
                        ## ----------- PANNEAU LATÉRAL --------------------------------------------
                        sidebarPanel(
                          width = 3,
                          h4("Filtres"),
                          
                          sliderInput("filtre_annee_socio", "Année :",
                                      min   = min(master_df_historique$annee),
                                      max   = max(master_df_historique$annee),
                                      value = max(master_df_historique$annee),
                                      step  = 1, sep = ""),
                          
                          selectInput("socio_variable", "Indicateur :",
                                      choices = list(
                                        "Structure emploi féminin" = c(
                                          `Part des femmes cadres`       = "part_femmes_cadres",
                                          `Part des femmes prof. inter.` = "part_femmes_prof_inter"
                                        ),
                                        "Mixité & activité" = c(
                                          `Taux de féminisation des cadres` = "taux_femmes_parmi_cadres",
                                          `Taux d'activité des femmes 15-64`= "taux_activite_femmes"
                                        )
                                      ),
                                      selected = "part_femmes_cadres"),
                          
                          selectizeInput("filtre_ept_sd", "Territoires (EPT) :",
                                         choices  = sort(unique(master_df_historique$ept_name)),
                                         multiple = TRUE,
                                         options  = list(placeholder = "Tous")),
                          
                          uiOutput("alert_paris_sd")      
                        ),
                        
                        mainPanel(
                          width = 9,
                          uiOutput("titre_socio_ui"),
                          plotOutput("plot_socio", height = "550px"),
                          uiOutput("corr_banner"),
                          DTOutput("table_sd")
                        )
                      )
             ),
             
             # ────────── 4. HISTORIQUE & ÉVOLUTIONS ───────────────────────────────────────
             tabPanel("Historique & Évolutions", icon = icon("chart-line"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h4("Filtres de l'analyse"),
                                     selectInput("filtre_taille_historique", "Taille d'entreprise :", choices = c("Toutes les tailles", unique(master_df_historique$tranche_effectifs))),
                                     selectizeInput("filtre_ept_historique", "Choisir un ou plusieurs territoire(s) :", choices = sort(unique(master_df_historique$ept_name)), selected = c("Ville de Paris", "Grand Paris Seine Ouest"), multiple = TRUE)
                        ),
                        mainPanel(width = 9, uiOutput("kpi_historique_ui"), hr(), plotlyOutput("plot_historique_interactif", height = "450px"), color_switch_ui("color_switch_historique"), hr(), DT::dataTableOutput("table_historique"))
                      )
             ),
             
             # ────────── 5. DOCUMENTATION ────────────────────────────────────────────────
             tabPanel("Documentation", icon = icon("book"), documentation_content)
  ))

# ==============================================================================
# SERVEUR (LOGIQUE DE L'APPLICATION)
# ==============================================================================
server <- function(input, output, session) {
  
  # --- LOGIQUE ONGLET CARTE ---
  observeEvent(input$niveau_geo_carte, {
    choix <- if (input$niveau_geo_carte == "Départements") sort(unique(master_df_historique$dep_name)) else sort(unique(master_df_historique$ept_name))
    updateSelectizeInput(session, "selection_territoires", choices = choix)
  }, ignoreNULL = FALSE)
  data_annee_carte <- reactive({ master_df_historique %>% filter(annee == input$filtre_annee_carte) })
  map_data_filtrée <- reactive({
    df_annee <- data_annee_carte()
    if (input$niveau_geo_carte == "Départements") {
      agg_data <- df_annee %>% group_by(code = dep_code, name = dep_name) %>% summarise(score_moyen = weighted.mean(index, poids, na.rm = TRUE), .groups = "drop")
      df_map <- map_dep %>% left_join(agg_data, by = c("dep_code" = "code"))
      if (!is.null(input$selection_territoires) && length(input$selection_territoires) > 0) df_map <- df_map %>% filter(dep_name %in% input$selection_territoires)
    } else {
      agg_data <- df_annee %>% group_by(code = ept_code, name = ept_name) %>% summarise(score_moyen = weighted.mean(index, poids, na.rm = TRUE), .groups = "drop")
      df_map <- map_ept %>% left_join(agg_data, by = c("ept_code" = "code"))
      if (!is.null(input$selection_territoires) && length(input$selection_territoires) > 0) df_map <- df_map %>% filter(ept_name %in% input$selection_territoires)
    }
    df_map %>% filter(!is.na(score_moyen))
  })
  points_filtres_carte <- reactive({
    df <- data_annee_carte()
    if (!is.null(input$selection_territoires) && length(input$selection_territoires) > 0) {
      col_geo <- if(input$niveau_geo_carte == "Départements") "dep_name" else "ept_name"
      df <- df %>% filter(.data[[col_geo]] %in% input$selection_territoires)
    }
    if (input$filtre_taille_carte != "Toutes les tailles") df <- df %>% filter(tranche_effectifs == input$filtre_taille_carte)
    if (!is.null(input$filtre_secteur_carte) && !("Tous les secteurs" %in% input$filtre_secteur_carte)) df <- df %>% filter(secteur_activite %in% input$filtre_secteur_carte)
    df %>% filter(index >= input$filtre_score_carte[1] & index <= input$filtre_score_carte[2])
  })
  output$compteur_entreprises_ui <- renderUI({
    n_entreprises <- nrow(points_filtres_carte())
    tags$p(style="text-align: center; font-size: 1.1em; color: #333;", HTML(paste0("<strong>", format(n_entreprises, big.mark=" "), "</strong> entreprises affichées")))
  })
  output$download_filtered_map <- downloadHandler(
    filename = function() { paste0("donnees_filtrees_carte_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(points_filtres_carte(), file, row.names = FALSE, fileEncoding = "UTF-8") }
  )
  output$map <- renderLeaflet({
    leaflet() %>% setView(lng = 2.35, lat = 48.85, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron", group = "Clair") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Plan de rues")
  })
  observe({
    leafletProxy("map") %>%
      hideGroup(c("Clair", "Satellite", "Plan de rues")) %>%
      showGroup(switch(input$map_basemap, "CartoDB.Positron" = "Clair", "Esri.WorldImagery" = "Satellite", "OpenStreetMap.Mapnik" = "Plan de rues"))
    df_polygones <- map_data_filtrée()
    df_points <- points_filtres_carte()
    req(nrow(df_polygones) > 0)
    pal <- colorNumeric("YlGnBu", domain = c(75, 100), na.color = "#E0E0E0")
    popup_polygones <- paste0("<strong>", df_polygones$name, "</strong><br>Score moyen : ", round(df_polygones$score_moyen, 1))
    popup_points <- if (nrow(df_points) > 0) { apply(df_points, 1, generate_company_popup) } else { NULL }
    proxy <- leafletProxy("map", data = df_polygones) %>%
      clearGroup(c("Vue Territoriale", "Vue Entreprises")) %>%
      clearControls() %>%
      fitBounds(lng1 = min(st_bbox(df_polygones)[1]), lat1 = min(st_bbox(df_polygones)[2]), lng2 = max(st_bbox(df_polygones)[3]), lat2 = max(st_bbox(df_polygones)[4])) %>%
      addPolygons(fillColor = ~pal(score_moyen), fillOpacity = 0.7, weight = 1.5, color = "white", group = "Vue Territoriale", popup = ~lapply(popup_polygones, HTML)) %>%
      addLegend(pal = pal, values = ~score_moyen, title = "Score Moyen", position = "bottomright")
    if (!is.null(popup_points)) {
      point_fill <- if (isTRUE(input$color_switch_carte)) "black" else "#fb9a99"
      point_border <- if (isTRUE(input$color_switch_carte)) "black" else "#e41a1c"
      cluster_opts <- if (isTRUE(input$color_switch_carte)) {
        palette_js <- jsonlite::toJSON(palette_accessible)
        JS(sprintf(
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
      proxy <- proxy %>% addCircleMarkers(data = df_points, lng = ~longitude, lat = ~latitude, popup = popup_points, radius = 7, color = point_border, weight = 1.5, fillColor = point_fill, fillOpacity = 0.85, clusterOptions = markerClusterOptions(iconCreateFunction = cluster_opts), group = "Vue Entreprises")
    }
    proxy %>% addLayersControl(overlayGroups = c("Vue Territoriale", "Vue Entreprises"), options = layersControlOptions(collapsed = FALSE, position = "topright"))
  })
  
  # --- LOGIQUE ONGLET ANALYSE SECTORIELLE ---
  data_secteur_filtree <- reactive({
    
    df <- master_df_historique %>%
      filter(annee == input$filtre_annee_secteur, !is.na(secteur_activite))
    
    if (input$filtre_taille_secteur != "Toutes les tailles") {
      df <- df %>% filter(tranche_effectifs == input$filtre_taille_secteur)
    }
    
    if (isTRUE(input$afficher_tous_secteurs)) {
      return(df)
    }
    if (!is.null(input$filtre_secteurs) && length(input$filtre_secteurs) > 0) {
      return(df %>% filter(secteur_activite %in% input$filtre_secteurs))
    }
    
    sector_summary <- df %>%
      group_by(secteur_activite) %>%
      summarise(median_score = median(index, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 10) %>% arrange(desc(median_score))
    
    df %>%
      filter(secteur_activite %in% c(head(sector_summary$secteur_activite, 5), tail(sector_summary$secteur_activite, 5)))
  })
  
  plot_height_react <- reactiveVal(450)
  
  output$plot_sector_ui <- renderUI({
    plotlyOutput("plot_secteur", height = paste0(plot_height_react(), "px"))
  })
  
  output$plot_secteur <- renderPlotly({
    
    df_plot <- data_secteur_filtree()
    req(nrow(df_plot) > 0, cancelOutput = TRUE)
    
    sector_summary <- df_plot %>%
      group_by(secteur_activite) %>%
      summarise(score_median = median(index, na.rm = TRUE), .groups = "drop") %>%
      arrange(score_median)
    
    n_cols <- n_distinct(sector_summary$secteur_activite)
    palette_cols <- if (isTRUE(input$color_switch_secteur)) {
      rep(palette_accessible, length.out = n_cols)
    } else {
      rep(RColorBrewer::brewer.pal(9, "Set1"), length.out = n_cols)
    }
    sector_summary$col <- palette_cols
    
    plot_height <- max(400, 40 * nrow(sector_summary))
    left_margin <- max(150, 6 * max(nchar(sector_summary$secteur_activite)))
    plot_height_react(plot_height)
    
    g <- ggplot(sector_summary,
                aes(x = score_median,
                    y = reorder(str_wrap(secteur_activite, 40), score_median),
                    text = sprintf("<b>Secteur :</b> %s<br><b>Médiane :</b> %.1f",
                                   secteur_activite, score_median))) +
      geom_segment(aes(x = 0, xend = score_median,
                       yend = reorder(str_wrap(secteur_activite, 40),
                                      score_median)),
                   linewidth = 1.2, colour = "#BDBDBD") +
      geom_point(aes(fill = col),
                 colour = "black", shape = 21, size = 7, stroke = .6) +
      geom_vline(xintercept = 85, linetype = "dashed",
                 linewidth = 1.2, colour = "red") +
      scale_fill_identity() +
      guides(colour = "none", fill = "none") +
      labs(x = "Score Egapro (médiane)", y = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.y = element_blank())
    
    ggplotly(g, tooltip = "text") %>%
      layout(margin = list(l = left_margin, t = 10, r = 20, b = 10),
             height = plot_height) %>%
      config(displayModeBar = FALSE)
  })
  
  output$table_secteur <- DT::renderDataTable({
    summary_df <- data_secteur_filtree() %>%
      group_by(secteur_activite) %>%
      summarise(`Nb entreprises` = n(),
                `Score Moyen`    = mean(index,  na.rm = TRUE),
                `Score Médian`   = median(index, na.rm = TRUE),
                `% < 85`         = mean(index < 85, na.rm = TRUE),
                .groups          = "drop") %>%
      arrange(desc(`Score Moyen`))
    
    datatable(summary_df,
              rownames = FALSE, extensions = "Buttons",
              options  = list(pageLength = 10, dom = "Bfrtip",
                              buttons   = list("copy", "csv", "excel"),
                              language  = list(url =
                                                 "//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"))) %>%
      formatRound(columns = 2,   digits = 0) %>%    
      formatRound(columns = 3:4, digits = 1) %>%    
      formatPercentage(columns = 5, digits = 1)
  })
  
  
  # --- LOGIQUE ONGLET ANALYSE SOCIO-DÉMOGRAPHIQUE ---
  data_sd <- reactive({
    master_df_historique |>
      filter(
        annee == input$filtre_annee_socio,
        if (length(input$filtre_ept_sd) > 0)
          ept_name %in% input$filtre_ept_sd else TRUE
      ) |>
      group_by(ept_name) |>
      summarise(
        score_moyen = weighted.mean(index, poids, na.rm = TRUE),
        socio_val   = mean(.data[[input$socio_variable]], na.rm = TRUE),
        n           = n(),
        .groups     = "drop"
      ) })
  
  output$titre_socio_ui <- renderUI({
    h3(paste(
      "Score Egapro moyen vs.",
      socio_variable_labels[input$socio_variable]
    ))
  })
  
  output$plot_socio <- renderPlot({
    df <- data_sd()
    validate(need(nrow(df) > 0, "Aucune donnée disponible."))
    
    ggplot(df, aes(x = socio_val, y = score_moyen)) +
      geom_smooth(method = "lm", se = FALSE,
                  colour = "#E74C3C", linetype = "dashed") +
      geom_point(aes(size = n), colour = "#2980B9", alpha = .8) +
      geom_text_repel(aes(label = ept_name)) +
      labs(
        x    = socio_variable_labels[input$socio_variable],
        y    = "Score Egapro moyen pondéré",
        size = "Nb d'entreprises",
        caption = "Ligne rouge : régression linéaire."
      ) +
      theme_minimal(base_size = 14) +
      coord_cartesian(ylim = c(80, 100))
  })
  
  output$corr_banner <- renderUI({
    df <- data_sd()
    if (nrow(df) < 3) return(NULL)
    
    ct <- cor.test(df$socio_val, df$score_moyen)
    r  <- round(ct$estimate, 2)
    p  <- ct$p.value
    
    couleur <- dplyr::case_when(
      p < .001 ~ "#1a9850",
      p < .05  ~ "#66bd63",
      p < .1   ~ "#fdae61",
      TRUE     ~ "#bdbdbd"
    )
    tendance <- dplyr::case_when(
      r >  .4 ~ "positive marquée",
      r >  .15~ "positive faible",
      r < -.4 ~ "négative marquée",
      r < -.15~ "négative faible",
      TRUE    ~ "quasi nulle"
    )
    
    div(
      style = paste0(
        "background:", couleur,
        ";color:white;padding:8px;font-weight:600;",
        "border-radius:6px;margin-bottom:10px;"
      ),
      paste0("Corrélation de Pearson : ", r,
             " (p-value = ", formatC(p, digits = 3, format = "f"),
             "). Tendance ", tendance, ".")
    )
  })
  
  output$table_sd <- DT::renderDT({
    master_df_historique |>
      filter(
        annee == input$filtre_annee_socio,
        if (length(input$filtre_ept_sd) > 0)
          ept_name %in% input$filtre_ept_sd else TRUE
      ) |>
      summarise(
        `Taux activité F`          = mean(taux_activite_femmes,      na.rm = TRUE),
        `Femmes cadres`            = mean(part_femmes_cadres,        na.rm = TRUE),
        `Femmes prof. inter.`      = mean(part_femmes_prof_inter,    na.rm = TRUE),
        `Taux féminisation cadres` = mean(taux_femmes_parmi_cadres,  na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(everything(),
                          names_to  = "Indicateur",
                          values_to = "Valeur") |>
      mutate(Valeur = sprintf("%.1f %%", Valeur)) |>
      datatable(options = list(dom = "t"), rownames = FALSE)
  })
  
  output$alert_paris_sd <- renderUI({
    if ("Ville de Paris" %in% input$filtre_ept_sd) {
      tags$div(
        style = "margin-top:8px; font-size: 0.85em; color:#8a6d3b; 
               background:#fcf8e3; border:1px solid #faebcc; 
               border-radius:4px; padding:6px;",
        icon("info-circle"), 
        HTML("La <strong>Ville de Paris</strong> ne dispose pas<br>
           de données socio-démographiques<br>
           (sauf taux de féminisation des cadres).<br>
           Son inclusion peut biaiser les distributions<br>
           et les corrélations.")
      )
    } else {
      NULL      
    }
  })
  # --- LOGIQUE ONGLET HISTORIQUE ---
  data_historique_agg <- reactive({
    req(input$filtre_ept_historique)
    
    df_base <- master_df_historique
    
    if (input$filtre_taille_historique != "Toutes les tailles") {
      df_base <- df_base %>% filter(tranche_effectifs == input$filtre_taille_historique)
    }
    
    df_base %>% 
      filter(ept_name %in% input$filtre_ept_historique) %>%
      group_by(annee, ept_name) %>% 
      summarise(score_moyen = weighted.mean(index, poids, na.rm = TRUE), .groups = "drop") %>%
      arrange(ept_name, annee)
  })
  
  output$kpi_historique_ui <- renderUI({
    df <- data_historique_agg(); req(nrow(df) > 1)
    annee_debut <- min(df$annee); annee_fin <- max(df$annee)
    progression_df <- df %>% group_by(ept_name) %>% summarise(score_start = score_moyen[annee == annee_debut], score_end = score_moyen[annee == annee_fin], .groups = "drop") %>% filter(!is.na(score_start) & !is.na(score_end))
    if (nrow(progression_df) == 0) return(tags$div(class="alert alert-warning", "Données insuffisantes pour calculer la progression."))
    mean_progression <- mean(progression_df$score_end - progression_df$score_start, na.rm = TRUE)
    progression_texte <- sprintf("%+.1f", mean_progression)
    kpi_style <- "background-color: #2C3E50; padding: 25px; border-radius: 12px; text-align: center; margin-top: 0px; box-shadow: 0 4px 12px rgba(0,0,0,0.15);"
    tags$div(style = kpi_style,
             tags$h2(style="margin: 0; font-size: 3em; font-weight: bold; color: #ECF0F1;", progression_texte),
             tags$p(style="margin: 0; font-size: 1.2em; color: #ECF0F1;", paste("Progression moyenne entre", annee_debut, "et", annee_fin))
    )
  })
  
  output$plot_historique_interactif <- renderPlotly({
    df_plot <- data_historique_agg()
    req(
      nrow(df_plot) > 0,
      cancelOutput = TRUE         
    )
    if (nrow(df_plot) == 0) {
      showNotification("Veuillez sélectionner au moins un territoire.",
                       type = "message", duration = 5)
      return(NULL)
    }
    
      df_plot_with_tooltip <- df_plot %>%
      mutate(
        tooltip_text = paste0("<b>Territoire:</b> ", as.character(ept_name), 
                              "<br><b>Année:</b> ", annee, 
                              "<br><b>Score:</b> ", round(score_moyen, 2))
      )
    

    palette_a_utiliser <- if (isTRUE(input$color_switch_historique)) {
      palette_accessible[seq_len(n_distinct(df_plot_with_tooltip$ept_name))]
    } else {
      suppressWarnings(
        RColorBrewer::brewer.pal(max(3, n_distinct(df_plot_with_tooltip$ept_name)), "Set1")
      )[seq_len(n_distinct(df_plot_with_tooltip$ept_name))]
    }
    
    plot_ly(
      data = df_plot_with_tooltip,
      x = ~annee,
      y = ~score_moyen,
      color = ~ept_name, # Crée une ligne par 'ept_name'
      colors = palette_a_utiliser,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~tooltip_text,
      hoverinfo = 'text'
    ) %>%
      layout(
        title = list(text = "Évolution Comparée des Scores Egapro", x = 0.5),
        xaxis = list(title = "Année"),
        yaxis = list(title = "Score Egapro moyen pondéré"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$table_historique <- DT::renderDataTable({
    df <- data_historique_agg(); if (nrow(df) == 0) return(datatable(data.frame(Message = "Aucune donnée disponible"), options = list(dom = 't')))
    df_table <- df %>% select(Territoire = ept_name, Année = annee, `Score Moyen` = score_moyen) %>% pivot_wider(names_from = Année, values_from = `Score Moyen`)
    datatable(df_table, rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip', ordering = FALSE, buttons = list('copy', 'csv', 'excel'), language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json')),
              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color: #333; font-size: 1.2em; font-weight: bold;', 'Données détaillées')) %>%
      formatRound(columns = which(sapply(df_table, is.numeric)), digits = 2)
  })
  
}

# ==============================================================================
# LANCEMENT DE L'APPLICATION
# ==============================================================================
shinyApp(ui, server)