# ==============================================================================
# app.R : Point d'entrée de l'application Shiny
# Rôle :
#   1. Définir la structure de l'interface (UI) en assemblant les modules.
#   2. Lancer la logique du serveur (Server) en appelant les modules.
#   3. Lancer l'application.
#
# Toute la configuration est gérée par le fichier global.R.
# ==============================================================================

source("global.R", local = TRUE)


# ==============================================================================
# INTERFACE UTILISATEUR (UI)
# ==============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bg = "#F0F2F5",       # Fond gris ardoise clair
    fg = "#1E2A3A",       # Texte bleu ardoise sombre
    primary = "#7B61FF",  # Accent violet vif
    secondary = "#495057",# Gris neutre pour les éléments secondaires
    base_font = bslib::font_google("Inter", local = TRUE), # Une police moderne et très lisible
    "font-size-base" = "0.95rem" # On ajuste légèrement la taille de la police
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Baromètre de la Parité en Entreprise – Territoires du Grand Paris"),
  
  shiny::uiOutput("data_freshness_banner_ui"),
  
  
  navbarPage("Navigation",
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
  
  # --- AJOUTER CE BLOC AU DÉBUT DU SERVEUR ---
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
  
  carte_server("carte", master_df_historique, map_ept, map_dep, palette_accessible)
  sectoriel_server("sectoriel", master_df_historique, palette_accessible)
  indicateurs_server("indicateurs", master_df_historique, map_ept, map_dep)
  socio_dem_server("socio_dem", master_df_historique, socio_variable_labels)
  historique_server("historique", master_df_historique, palette_accessible)
  
}

# ==============================================================================
# LANCEMENT DE L'APPLICATION
# ==============================================================================
shinyApp(ui, server)