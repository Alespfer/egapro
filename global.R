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
library(bslib) # Ajouter cette ligne
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
library(writexl)
library(weights)

# ------------------------------------------------------------------------------
# 2. Chargement des sources (AVEC LE BON CHEMIN)
# ------------------------------------------------------------------------------
source("R/utils.R", local = TRUE)
source("R/documentation.R", local = TRUE) 
source("R/carte_module.R", local = TRUE)
source("R/sectoriel_module.R", local = TRUE)
source("R/socio_dem_module.R", local = TRUE)
source("R/historique_module.R", local = TRUE)
source("R/indicateurs_module.R", local = TRUE)


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
map_ze <- readRDS("data_shiny/map_ze.RDS") # <-- AJOUTER CETTE LIGNE


# -- Pré-calculs --
map_ept <- sf::st_transform(map_ept, crs = 4326)
map_dep <- sf::st_transform(map_dep, crs = 4326)
map_ze <- sf::st_transform(map_ze, crs = 4326) # <-- AJOUTER CETTE LIGNE

eff_key <- c("50 à 250" = 150, "251 à 999" = 625, "1000 et plus" = 1500)
master_df_historique <- master_df_historique |>
  dplyr::mutate(poids = unname(eff_key[tranche_effectifs]))


data_status <- check_data_freshness()


# -- Objets statiques --
palette_accessible <- c("#0072B2","#F0E442", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")
socio_variable_labels <- c(
  "part_femmes_cadres"        = "Part de femmes cadres parmi les femmes actives (%)",
  "part_femmes_prof_inter"    = "Part de femmes en prof. inter. parmi les femmes actives (%)",
  "taux_femmes_parmi_cadres"  = "Taux de féminisation des postes de cadres (%)",
  "taux_activite_femmes"      = "Taux d'activité des femmes 15-64 ans (%)"
)


# --- AJOUT ---: Labels pour le nouveau module
indicateur_labels <- c(
  "Écart de Rémunération (sur 40)" = "note_remuneration",
  "Écart d'Augmentations (sur 20)" = "note_augmentation",
  "Écart de Promotions (sur 15)" = "note_promotion",
  "Augmentation au retour de congé maternité (sur 15)" = "note_conge_mat",
  "Part de femmes dans les 10 plus hautes rémunérations (sur 10)" = "note_hautes_rem"
)

# --- AJOUT ---: Descriptions détaillées pour les infobulles du module Indicateurs
indicateur_descriptions <- list(
  "note_remuneration" = "<strong>Écart de rémunération (40 pts) :</strong> Compare la rémunération moyenne des femmes et des hommes, par tranche d'âge et par catégorie de postes équivalents. C'est l'indicateur avec le plus de poids dans l'Index.",
  "note_augmentation" = "<strong>Écart d'augmentations individuelles (20 pts) :</strong> Compare le pourcentage de femmes et d'hommes ayant bénéficié d'une augmentation individuelle (hors promotions).",
  "note_promotion" = "<strong>Écart de promotions (15 pts) :</strong> Compare le pourcentage de femmes et d'hommes ayant été promus. Cet indicateur ne concerne que les entreprises de plus de 250 salariés.",
  "note_conge_mat" = "<strong>Augmentations au retour de congé maternité (15 pts) :</strong> Vérifie que les salariées, à leur retour de congé maternité, ont bien bénéficié des augmentations (générales et individuelles) perçues par les autres salariés pendant leur absence.",
  "note_hautes_rem" = "<strong>Hautes rémunérations (10 pts) :</strong> Mesure la parité parmi les 10 salariés ayant perçu les plus hautes rémunérations dans l'entreprise."
)