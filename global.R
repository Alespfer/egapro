# ==============================================================================
# global.R : Configuration, chargement des données et des sources.
# Version finale et correcte.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Chargement des librairies
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# 2. Chargement des sources (AVEC LE BON CHEMIN)
# ------------------------------------------------------------------------------
source("R/utils.R", local = TRUE)
source("R/documentation.R", local = TRUE) 
source("R/carte_module.R", local = TRUE)
source("R/sectoriel_module.R", local = TRUE)
source("R/socio_dem_module.R", local = TRUE)
source("R/historique_module.R", local = TRUE)

# ------------------------------------------------------------------------------
# 3. Données et objets globaux
# ------------------------------------------------------------------------------

# -- Alias --
validate <- shiny::validate
need     <- shiny::need

# -- Données --
master_df_historique <- readRDS("data_shiny/master_df_historique.RDS")
map_ept <- readRDS("data_shiny/map_ept.RDS")
map_dep <- readRDS("data_shiny/map_dep.RDS")

# -- Pré-calculs --
map_ept <- sf::st_transform(map_ept, crs = 4326)
map_dep <- sf::st_transform(map_dep, crs = 4326)
eff_key <- c("50 à 250" = 150, "251 à 999" = 625, "1000 et plus" = 1500)
master_df_historique <- master_df_historique |>
  dplyr::mutate(poids = unname(eff_key[tranche_effectifs]))


# -- Objets statiques --
palette_accessible <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
socio_variable_labels <- c(
  "part_femmes_cadres"        = "Part de femmes cadres parmi les femmes actives (%)",
  "part_femmes_prof_inter"    = "Part de femmes en prof. inter. parmi les femmes actives (%)",
  "taux_femmes_parmi_cadres"  = "Taux de féminisation des postes de cadres (%)",
  "taux_activite_femmes"      = "Taux d'activité des femmes 15-64 ans (%)"
)