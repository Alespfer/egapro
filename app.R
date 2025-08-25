# ==============================================================================
# app.R : Point d'entrée de l'application (Version CORRIGÉE avec interactivité)
# ==============================================================================

source("global.R", local = TRUE)

# ==============================================================================
# INTERFACE UTILISATEUR (UI)
# ==============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bg = "#F0F2F5",
    fg = "#1E2A3A",
    primary = "#7B61FF",
    secondary = "#495057",
    base_font = bslib::font_google("Inter", local = TRUE),
    "font-size-base" = "0.95rem"
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Baromètre de la Parité en Entreprise – Territoires du Grand Paris"),
  
  shiny::uiOutput("data_freshness_banner_ui"),
  
  # On ajoute un ID à la barre de navigation pour la contrôler
  navbarPage("Navigation", id = "main_navbar",
             tabPanel("Carte & Territoires", icon = icon("map-marked-alt"), carte_ui("carte", master_df_historique)),
             tabPanel("Analyse Sectorielle", icon = icon("industry"), sectoriel_ui("sectoriel", master_df_historique)),
             tabPanel("Analyse des Indicateurs", icon = icon("magnifying-glass-chart"), indicateurs_ui("indicateurs", master_df_historique)),
             tabPanel("Socio-démographique", icon = icon("users"), socio_dem_ui("socio_dem", master_df_historique)),
             tabPanel("Historique & Évolutions", icon = icon("chart-line"), historique_ui("historique", master_df_historique)),
             tabPanel("Documentation", icon = icon("book"), documentation_content)
  )
)

# ==============================================================================
# SERVEUR (LOGIQUE DE L'APPLICATION)
# ==============================================================================
server <- function(input, output, session) {
  
  output$data_freshness_banner_ui <- shiny::renderUI({
    if (!data_status$is_fresh) {
      shiny::div(class = "alert alert-warning", role = "alert",
                 style = "margin: 15px; text-align: center; font-weight: bold;",
                 shiny::icon("triangle-exclamation"),
                 data_status$message
      )
    } else {
      NULL
    }
  })
  
  # --- AJOUT ---: Logique pour le bouton de mise à jour
  shiny::observeEvent(input$trigger_data_update, {
    # On appelle notre nouvelle fonction de mise à jour
    run_data_update_pipeline()
    
    # On notifie l'utilisateur que c'est terminé
    shiny::showNotification("Mise à jour des données terminée avec succès. Rechargement de l'application...", 
                            type = "success", duration = 5)
    
    # On recharge la session pour que l'application utilise les nouvelles données
    session$reload()
  })
  
  # --- CORRECTION ---: Création de l'objet de communication et passage aux modules
  shared_state <- shiny::reactiveValues()
  
  carte_server("carte", master_df_historique, map_ept, map_dep, map_ze, palette_accessible, shared_state)
  sectoriel_server("sectoriel", master_df_historique, palette_accessible, shared_state)
  
  # Les autres modules n'ont pas besoin de l'état partagé
  indicateurs_server("indicateurs", master_df_historique, map_ept, map_dep, map_ze)
  socio_dem_server("socio_dem", master_df_historique, socio_variable_labels)
  historique_server("historique", master_df_historique, palette_accessible)
  
}

# ==============================================================================
# LANCEMENT DE L'APPLICATION
# ==============================================================================
shinyApp(ui, server)