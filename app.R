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
  theme = "bootstrap",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Baromètre de la Parité en Entreprise – Territoires du Grand Paris"),
  
  navbarPage("Navigation",
             tabPanel("Carte & Territoires", icon = icon("map-marked-alt"), carte_ui("carte", master_df_historique)),
             tabPanel("Analyse Sectorielle", icon = icon("industry"), sectoriel_ui("sectoriel", master_df_historique)),
             tabPanel("Socio-démographique", icon = icon("users"), socio_dem_ui("socio_dem", master_df_historique)),
             tabPanel("Historique & Évolutions", icon = icon("chart-line"), historique_ui("historique", master_df_historique)),
             tabPanel("Documentation", icon = icon("book"), documentation_content)
  )
)

# ==============================================================================
# SERVEUR (LOGIQUE DE L'APPLICATION)
# ==============================================================================
server <- function(input, output, session) {
  
  carte_server("carte", master_df_historique, map_ept, map_dep, palette_accessible)
  sectoriel_server("sectoriel", master_df_historique, palette_accessible)
  socio_dem_server("socio_dem", master_df_historique, socio_variable_labels)
  historique_server("historique", master_df_historique, palette_accessible)
  
}

# ==============================================================================
# LANCEMENT DE L'APPLICATION
# ==============================================================================
shinyApp(ui, server)