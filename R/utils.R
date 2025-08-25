# ==============================================================================
# Fichier d'Utilitaires (utils.R)
# Version pipe %>% et qualifiée
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1 : Fonctions d'importation des données brutes
# ------------------------------------------------------------------------------
import_latest_egapro <- function() {
  message("--- Importation des données Egapro depuis data.gouv.fr ---")
  
  res <- httr::GET(
    "https://www.data.gouv.fr/api/1/datasets/",
    query = list(q = "Index Egalite Professionnelle F/H", page_size = 1)
  )
  httr::stop_for_status(res)
  
  ds_list <- httr::content(res, as = "parsed", encoding = "UTF-8")$data
  if (length(ds_list) == 0) stop("Le dataset Egapro est introuvable sur data.gouv.fr.")
  ds <- ds_list[[1]]
  
  message("-> Dataset trouvé : '", ds$title, "' (modifié le ", ds$last_modified, ")")
  
  resources <- ds$resources
  formats_ok <- c("json", "csv", "xlsx", "xls") 
  idx <- which(tolower(sapply(resources, `[[`, "format")) %in% formats_ok)[1]
  if (is.na(idx)) stop("Aucune ressource téléchargeable (JSON, CSV, XLSX) trouvée.")
  
  chosen <- resources[[idx]]
  url <- chosen$url
  fmt <- tolower(chosen$format)
  
  message("-> Téléchargement de la ressource au format '", fmt, "'...")
  
  if (fmt == "json") {
    req <- httr::GET(url)
    httr::stop_for_status(req)
    data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"))
  } else if (fmt == "csv") {
    data <- readr::read_csv2(url, show_col_types = FALSE)
  } else {
    tmp <- tempfile(fileext = paste0(".", fmt))
    httr::GET(url, httr::write_disk(tmp, overwrite = TRUE))
    data <- readxl::read_excel(tmp, sheet = 1)
  }
  
  message("✅ Import Egapro terminé : ", nrow(data), " lignes.")
  return(data)
}

import_sirene_idf <- function() {
  message("--- Importation des données SIRENE (IDF, +50 salariés) ---")
  
  tranches_urls <- list(
    "50-99"     = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsetablissement%3A%2250%20%C3%A0%2099%20salari%C3%A9s%22",
    "100-199"   = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsetablissement%3A%22100%20%C3%A0%20199%20salari%C3%A9s%22",
    "200-249"   = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsetablissement%3A%22200%20%C3%A0%20249%20salari%C3%A9s%22",
    "250-499"   = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsetablissement%3A%22250%20%C3%A0%20499%20salari%C3%A9s%22",
    "500-999"   = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsetablissement%3A%22500%20%C3%A0%20999%20salari%C3%A9s%22",
    "1000-1999" = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsetablissement%3A%221%20000%20%C3%A0%201%20999%20salari%C3%A9s%22",
    "2000-4999" = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsetablissement%3A%222%20000%20%C3%A0%204%20999%20salari%C3%A9s%22",
    "5000-9999" = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsetablissement%3A%225%20000%20%C3%A0%209%20999%20salari%C3%A9s%22",
    "10000+"    = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsetablissement%3A%2210%20000%20salari%C3%A9s%20et%20plus%22"
  )
  
  liste_de_dataframes <- lapply(names(tranches_urls), function(nom_tranche) {
    url <- tranches_urls[[nom_tranche]]
    message("-> Chargement de la tranche : ", nom_tranche, "...")
    tryCatch({ jsonlite::fromJSON(url) }, 
             error = function(e) { message("  /!\\ Erreur pour la tranche '", nom_tranche, "'. Elle sera ignorée."); NULL })
  })
  
  data_final <- dplyr::bind_rows(liste_de_dataframes)
  message("✅ Import SIRENE terminé : ", nrow(data_final), " lignes collectées.")
  return(data_final)
}

import_xlsx_from_zip <- function(zip_path, sheet = 1, file_pattern = "\\.xlsx$", skip = 5) {
  message("--- Importation des données du Recensement depuis un ZIP ---")
  if (!file.exists(zip_path)) stop("Le fichier ZIP est introuvable : ", zip_path)
  
  tmp_dir <- tempfile()
  utils::unzip(zip_path, exdir = tmp_dir)
  
  xlsx_file <- list.files(tmp_dir, pattern = file_pattern, full.names = TRUE, recursive = TRUE)[1]
  if (is.na(xlsx_file)) stop("Aucun fichier Excel trouvé dans : ", zip_path)
  
  df <- readxl::read_xlsx(xlsx_file, sheet = sheet, skip = skip) %>% janitor::clean_names()
  
  unlink(tmp_dir, recursive = TRUE)
  
  message("✅ Import terminé : ", nrow(df), " lignes.")
  df
}

# ------------------------------------------------------------------------------
# SECTION 2 : Fonctions de préparation et de nettoyage
# ------------------------------------------------------------------------------
get_secteur_from_naf <- function(code_naf) {
  section_num <- as.integer(substr(code_naf, 1, 2))
  
  dplyr::case_when(
    is.na(section_num) ~ "Non défini",
    section_num >= 1 & section_num <= 3 ~ "Agriculture, sylviculture et pêche",
    section_num >= 5 & section_num <= 9 ~ "Industries extractives",
    section_num >= 10 & section_num <= 33 ~ "Industrie manufacturière",
    section_num == 35 ~ "Production et distribution d'électricité, de gaz, de vapeur...",
    section_num >= 36 & section_num <= 39 ~ "Production et distribution d'eau, assainissement...",
    section_num >= 41 & section_num <= 43 ~ "Construction",
    section_num >= 45 & section_num <= 47 ~ "Commerce, réparation d'automobiles et de motocycles",
    section_num >= 49 & section_num <= 53 ~ "Transports et entreposage",
    section_num >= 55 & section_num <= 56 ~ "Hébergement et restauration",
    section_num >= 58 & section_num <= 63 ~ "Information et communication",
    section_num >= 64 & section_num <= 66 ~ "Activités financières et d'assurance",
    section_num == 68 ~ "Activités immobilières",
    section_num >= 69 & section_num <= 75 ~ "Activités spécialisées, scientifiques et techniques",
    section_num >= 77 & section_num <= 82 ~ "Activités de services administratifs et de soutien",
    section_num == 84 ~ "Administration publique",
    section_num == 85 ~ "Enseignement",
    section_num >= 86 & section_num <= 88 ~ "Santé humaine et action sociale",
    section_num >= 90 & section_num <= 93 ~ "Arts, spectacles et activités récréatives",
    section_num >= 94 & section_num <= 96 ~ "Autres activités de services",
    TRUE ~ "Non défini"
  )
}

prepare_egapro_data <- function(raw_egapro_df) {
  message("--- Préparation et standardisation des données Egapro ---")
  
  df_clean <- raw_egapro_df %>% janitor::clean_names()
  
  naf_col <- intersect(c("code_naf", "code_naf_ape"), names(df_clean))[1]
  effectifs_col <- intersect(c("tranche_deffectifs", "tranche_d_effectifs", "tranche_effectifs"), names(df_clean))[1]
  if (is.na(naf_col) || is.na(effectifs_col)) stop("Colonnes NAF ou effectifs introuvables.")
  
  cols_aug_existantes <- intersect(c("note_ecart_taux_daugmentation_hors_promotion", "note_ecart_taux_daugmentation"), names(df_clean))
  
  df_prepared <- df_clean %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("note"), as.character),
      dplyr::across(dplyr::starts_with("note"), as.numeric),
      siren = stringr::str_pad(siren, 9, side = "left", pad = "0"),
      annee = as.integer(annee)
    )
  
  if (length(cols_aug_existantes) > 0) {
    df_prepared <- df_prepared %>% dplyr::mutate(note_augmentation_unifiee = dplyr::coalesce(!!!rlang::syms(cols_aug_existantes)))
  } else {
    df_prepared$note_augmentation_unifiee <- NA_real_
  }
  
  df_prepared <- df_prepared %>%
    dplyr::filter(structure == "Entreprise", !is.na(note_index)) %>%
    dplyr::rename(
      index = note_index, tranche_effectifs = dplyr::all_of(effectifs_col), code_naf = dplyr::all_of(naf_col), 
      note_remuneration = note_ecart_remuneration, note_augmentation = dplyr::any_of("note_augmentation_unifiee"),
      note_promotion = note_ecart_taux_de_promotion, note_conge_mat = note_retour_conge_maternite,
      note_hautes_rem = note_hautes_remunerations
    ) %>%
    dplyr::mutate(secteur_activite = get_secteur_from_naf(code_naf)) %>%
    dplyr::select(siren, annee, index, raison_sociale, tranche_effectifs, code_naf, secteur_activite,
                  dplyr::any_of(c("note_remuneration", "note_augmentation", "note_promotion", "note_conge_mat", "note_hautes_rem"))) %>%
    dplyr::distinct(siren, annee, .keep_all = TRUE)
  
  message("✅ Préparation Egapro terminée : ", nrow(df_prepared), " observations standardisées.")
  return(df_prepared)
}

clean_sirene_data <- function(raw_sirene_df) {
  message("--- Nettoyage des données SIRENE ---")
  sirene_clean <- raw_sirene_df %>%
    janitor::clean_names() %>%
    dplyr::filter(etablissementsiege == "oui") %>%
    dplyr::select(siren, code_commune = codecommuneetablissement, geolocetablissement) %>%
    dplyr::distinct(siren, .keep_all = TRUE) %>%
    tidyr::unnest(geolocetablissement) %>%
    dplyr::filter(!is.na(lat) & !is.na(lon)) %>%
    dplyr::rename(latitude = lat, longitude = lon)
  
  message("✅ Nettoyage SIRENE OK : ", nrow(sirene_clean), " sièges sociaux géolocalisés.")
  return(sirene_clean)
}

#' Standardise le nom de la colonne du code commune.
#'
#' @description Les fichiers de l'INSEE ont des noms de colonne variables pour
#' le code commune ('codgeo', 'com', etc.). Cette fonction les renomme en 'code_commune'.
#' @param df Un dataframe issu des données de l'INSEE.
#' @return Le même dataframe avec la colonne du code commune renommée.
rename_to_com <- function(df) {
  # Trouve la colonne qui correspond au code commune
  # en cherchant des noms potentiels
  col_names <- names(df)
  commune_col <- intersect(c("codgeo", "com", "cod_com"), col_names)[1]
  
  if (is.na(commune_col)) {
    warning("Impossible de trouver une colonne de code commune ('codgeo', 'com', 'cod_com'). Le dataframe est retourné tel quel.")
    return(df)
  }
  
  # Renomme la colonne trouvée en 'code_commune'
  df %>%
    dplyr::rename(code_commune = !!rlang::sym(commune_col))
}


#' Calcule les indicateurs socio-démographiques au niveau communal.
#'
#' @description Fusionne plusieurs tables du recensement INSEE pour créer des indicateurs clés.
#' @param df_pop_structure Non utilisé dans cette version, mais gardé pour compatibilité.
#' @param df_ic Dataframe "base-ic-activite-residents-2021".
#' @param df_act5 Dataframe "TD_ACT5_2021" avec le détail par sexe, âge et CSP.
#' @return Un dataframe avec un indicateur par ligne et par commune.
create_socio_features <- function(df_pop_structure, df_ic, df_act5) {
  
  message("-> Démarrage du calcul des indicateurs socio-démographiques...")
  
  # Nettoyage des noms de colonnes pour la robustesse
  df_ic <- df_ic %>% janitor::clean_names()
  df_act5 <- df_act5 %>% janitor::clean_names()
  
  # 1. Taux d'activité des femmes (source: df_ic "Activité des résidents")
  taux_activite <- df_ic %>%
    dplyr::select(code_commune = com, p21_fact1564, p21_f1564) %>%
    dplyr::group_by(code_commune) %>%
    dplyr::summarise(
      total_femmes_actives = sum(p21_fact1564, na.rm = TRUE),
      total_femmes_pop = sum(p21_f1564, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      taux_activite_femmes = (total_femmes_actives / total_femmes_pop) * 100
    ) %>%
    dplyr::select(code_commune, taux_activite_femmes)
  
  # 2. Structure de l'emploi (source: df_act5)
  # NOTE: La nomenclature INSEE standard pour les CSP est :
  # CS1: Agriculteurs, CS2: Artisans/Commerçants, CS3: Cadres, CS4: Prof. Inter., CS5: Employés, CS6: Ouvriers
  # Dans ce fichier, les colonnes semblent être numérotées de 61 à 66.
  # Nous allons supposer que cs1_63 = Cadres (CS3) et cs1_64 = Prof. Inter. (CS4)
  
  structure_emploi <- df_act5 %>%
    # S'assurer que le code commune est bien nommé
    rename_to_com() %>%
    # Calculer les totaux en additionnant les colonnes par sexe et CSP
    dplyr::mutate(
      # Femmes (sexe2)
      femmes_cadres = rowSums(dplyr::select(., dplyr::starts_with("cs1_63") & dplyr::ends_with("_sexe2")), na.rm = TRUE),
      femmes_prof_inter = rowSums(dplyr::select(., dplyr::starts_with("cs1_64") & dplyr::ends_with("_sexe2")), na.rm = TRUE),
      femmes_actives_occupees_total = rowSums(dplyr::select(., dplyr::starts_with("cs1_") & dplyr::ends_with("_sexe2")), na.rm = TRUE),
      
      # Ensemble (sexe1 + sexe2)
      ensemble_cadres = rowSums(dplyr::select(., dplyr::starts_with("cs1_63")), na.rm = TRUE)
    ) %>%
    # Calculer les indicateurs finaux
    dplyr::mutate(
      part_femmes_cadres = (femmes_cadres / femmes_actives_occupees_total) * 100,
      part_femmes_prof_inter = (femmes_prof_inter / femmes_actives_occupees_total) * 100,
      taux_femmes_parmi_cadres = (femmes_cadres / ensemble_cadres) * 100
    ) %>%
    dplyr::select(code_commune, part_femmes_cadres, part_femmes_prof_inter, taux_femmes_parmi_cadres)
  
  # 3. Fusion des indicateurs
  communes_features <- taux_activite %>%
    dplyr::full_join(structure_emploi, by = "code_commune") %>%
    # Remplacer les NaN ou Inf potentiels générés par des divisions par zéro
    dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA)))
  
  message("✅ Création des indicateurs socio-démographiques terminée.")
  return(communes_features)
}

# ------------------------------------------------------------------------------
# SECTION 3 : Fonctions de gestion géographique
# ------------------------------------------------------------------------------
#' Charge et prépare le fond de carte des communes.
#'
#' @description Charge les données, filtre sur le périmètre, harmonise
#' et fusionne les arrondissements de Paris en une seule commune "Paris" (75056).
#' @return Un objet sf contenant les polygones des communes, avec Paris unifié.
load_and_prepare_map <- function() {
  message("--- Chargement et préparation du fond de carte (Île-de-France complète) ---")
  
  # 1. Chargement de la table de correspondance Commune -> Zone d'Emploi
  path_ze_ref <- "data/raw/commune_ze_2020.xlsx"
  if (!file.exists(path_ze_ref)) {
    stop("Fichier 'commune_ze_2020.xlsx' introuvable dans data/raw/. Veuillez le télécharger depuis le site de l'INSEE.")
  }
  # --- CORRECTION ---: On utilise read_excel car le fichier est un XLSX
  ze_ref <- readxl::read_excel(path_ze_ref) %>%
    dplyr::select(com_code = CODGEO, ze_code = ZE2020, ze_name = LIBZE2020)
  
  # 2. Chargement du fond de carte des communes d'Île-de-France
  myURL <- "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/georef-france-commune-arrondissement-municipal-millesime/exports/geojson?lang=fr&refine=reg_name%3A%22%C3%8Ele-de-France%22&refine=year%3A%222020%22"
  
  map <- sf::st_read(myURL, quiet = TRUE) %>%
    dplyr::mutate(
      is_paris_arrondissement = stringr::str_starts(com_arm_code, "751"),
      com_arm_code = ifelse(is_paris_arrondissement, "75056", com_arm_code),
      com_arm_name = ifelse(is_paris_arrondissement, "Paris", com_arm_name),
      dplyr::across(c(com_arm_code, com_arm_name, ept_code, ept_name, dep_code, dep_name), as.character)
    ) %>%
    dplyr::select(com_code = com_arm_code, com_name = com_arm_name, ept_code, ept_name, dep_code, dep_name, geometry)
  
  map$ept_name <- ifelse(map$ept_name == "character(0)" | map$com_code == "75056", "Ville de Paris", map$ept_name)
  map$ept_code <- ifelse(map$ept_name == "Ville de Paris", "T1", map$ept_code)
  
  map <- map %>%
    dplyr::group_by(com_code, com_name, ept_code, ept_name, dep_code, dep_name) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
  
  # 3. Enrichissement du fond de carte avec les Zones d'Emploi
  map_enriched <- map %>%
    dplyr::left_join(ze_ref, by = "com_code")
  
  message("✅ Fond de carte communal IDF OK : ", nrow(map_enriched), " communes (Paris unifié) enrichies avec les Zones d'Emploi.")
  return(map_enriched)
}



aggregate_map <- function(map_com_sf, level = "ept") {
  group_vars <- if (level == "ept") c("ept_code", "ept_name") else c("dep_code", "dep_name")
  
  map_com_sf %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
}

# ------------------------------------------------------------------------------
# SECTION 4 : Fonctions utilitaires pour l'application Shiny
# ------------------------------------------------------------------------------
generate_company_popup <- function(df_row) {
  format_note <- function(note, max_points) {
    if (is.na(as.numeric(note))) 'NC' else paste(note, "/", max_points)
  }
  
  popup_html <- paste0(
    "<h4>", df_row["raison_sociale"], "</h4><hr>",
    "<p><strong>Score Egapro (", df_row["annee"], ") : </strong>", 
    "<strong style='font-size: 1.2em; color: #08519c;'>", format_note(df_row["index"], 100), "</strong></p>",
    "<p><strong>Détail des indicateurs :</strong></p>",
    "<ul>",
    "<li>Rémunération : ", format_note(df_row["note_remuneration"], 40), "</li>",
    "<li>Augmentations : ", format_note(df_row["note_augmentation"], 20), "</li>",
    "<li>Promotions : ", format_note(df_row["note_promotion"], 15), "</li>",
    "<li>Congé maternité : ", format_note(df_row["note_conge_mat"], 15), "</li>",
    "<li>Hautes rémunérations : ", format_note(df_row["note_hautes_rem"], 10), "</li>",
    "</ul><hr>",
    "<p><strong>Taille : </strong>", df_row["tranche_effectifs"], "</p>",
    "<p><strong>Secteur : </strong>", df_row["secteur_activite"], "</p>",
    "<p><strong>SIREN : </strong>", df_row["siren"], "</p>"
  )
  
  return(htmltools::HTML(popup_html))
}

# --- MODIFICATION ---: Le style a été retiré pour permettre un positionnement
# flexible du widget via les classes CSS de son conteneur parent (ex: flexbox).
color_switch_ui <- function(id) {
  shinyWidgets::switchInput(
    inputId = id, 
    label = "Palette accessible", 
    onLabel = "Oui", 
    offLabel = "Non", 
    value = FALSE,
    size = "small", 
    inline = TRUE, 
    width = "auto"
  )
}


# ------------------------------------------------------------------------------
# SECTION 5 : Fonctions de gestion de l'application
# ------------------------------------------------------------------------------

#' Vérifie la fraîcheur d'un fichier de données.
#'
#' @param file_path Chemin vers le fichier à vérifier.
#' @param threshold_days Nombre de jours au-delà duquel les données sont considérées comme obsolètes.
#' @return Une liste contenant l'état des données (is_fresh), leur âge et un message.
check_data_freshness <- function(file_path = "data_shiny/master_df_historique.RDS", threshold_days = 30) {
  
  if (!file.exists(file_path)) {
    stop("Fichier de données principal introuvable à l'emplacement : ", file_path)
  }
  
  last_modified_time <- file.info(file_path)$mtime
  age_days <- as.numeric(difftime(Sys.time(), last_modified_time, units = "days"))
  
  is_fresh <- age_days <= threshold_days
  
  message_text <- if (is_fresh) {
    paste0("Les données sont à jour (dernière mise à jour il y a ", floor(age_days), " jours).")
  } else {
    paste0("Les données datent de plus de ", threshold_days, " jours (dernière mise à jour il y a ", floor(age_days), " jours). Il est recommandé de les rafraîchir.")
  }
  
  # Affiche le statut dans la console au démarrage pour le développeur
  message("Vérification des données : ", message_text)
  
  return(
    list(
      is_fresh = is_fresh,
      last_modified = as.Date(last_modified_time),
      age_days = floor(age_days),
      message = message_text
    )
  )
}







# --- AJOUT ---: Procédure complète de mise à jour des données
run_data_update_pipeline <- function() {
  
  # Utilise `shiny::withProgress` pour montrer une barre de progression dans l'UI
  shiny::withProgress(message = 'Mise à jour des données en cours...', value = 0, {
    
    # Étape 1: Importation
    shiny::incProgress(0.1, detail = "Téléchargement des données Egapro...")
    raw_egapro <- import_latest_egapro()
    
    shiny::incProgress(0.1, detail = "Téléchargement des données SIRENE...")
    raw_sirene <- import_sirene_idf()
    
    shiny::incProgress(0.1, detail = "Lecture des données INSEE locales...")
    zip_path_pop_structure <- "data/raw/base-cc-evol-struct-pop-2021_xlsx.zip"
    zip_path_activite_reside <- "data/raw/base-ic-activite-residents-2021_xlsx (1).zip"
    zip_path_act5 <- "data/raw/TD_ACT5_2021_xlsx.zip"
    raw_pop_structure <- import_xlsx_from_zip(zip_path_pop_structure)
    raw_ic_activite <- import_xlsx_from_zip(zip_path_activite_reside)
    raw_act5 <- import_xlsx_from_zip(zip_path_act5, skip = 9) %>% rename_to_com()
    
    # Étape 2: Préparation
    shiny::incProgress(0.2, detail = "Préparation et nettoyage des données...")
    egapro_prepared <- prepare_egapro_data(raw_egapro)
    sirene_clean <- clean_sirene_data(raw_sirene)
    sirene_clean <- sirene_clean %>%
      dplyr::mutate(
        code_commune = if_else(stringr::str_starts(code_commune, "751"), "75056", code_commune)
      )
    communes_features <- create_socio_features(
      df_pop_structure = raw_pop_structure,
      df_ic = raw_ic_activite,
      df_act5 = raw_act5
    )
    paris_commune_data <- communes_features %>% filter(code_commune == "75056")
    
    if(nrow(paris_commune_data) > 0) {
      codes_arrondissements <- sprintf("751%02d", 1:20)
      paris_arrondissements_data <- paris_commune_data[rep(1, 20), ] %>%
        mutate(code_commune = codes_arrondissements)
      communes_features <- bind_rows(communes_features, paris_arrondissements_data)
    }
    
    # Étape 3: Géographie
    shiny::incProgress(0.1, detail = "Préparation des données géographiques...")
    map_com_prepared <- load_and_prepare_map()
    
    # Étape 4: Création de la table finale
    shiny::incProgress(0.1, detail = "Création de la table de données finale...")
    master_df_historique <- egapro_prepared %>%
      inner_join(sirene_clean, by = "siren") %>%
      inner_join(st_drop_geometry(map_com_prepared), by = c("code_commune" = "com_code")) %>%
      left_join(communes_features, by = "code_commune")
    
    # Étape 5: Sauvegarde des fichiers
    shiny::incProgress(0.2, detail = "Sauvegarde des fichiers pour l'application...")
    output_dir <- "data_shiny"
    if (!dir.exists(output_dir)) dir.create(output_dir)
    
    saveRDS(master_df_historique, file.path(output_dir, "master_df_historique.RDS"))
    saveRDS(map_com_prepared, file.path(output_dir, "map_com.RDS"))
    
    map_ept_prepared <- aggregate_map(map_com_prepared, level = "ept")
    saveRDS(map_ept_prepared, file.path(output_dir, "map_ept.RDS"))
    
    map_dep_prepared <- aggregate_map(map_com_prepared, level = "dep")
    saveRDS(map_dep_prepared, file.path(output_dir, "map_dep.RDS"))
    
    map_ze_prepared <- aggregate_map(map_com_prepared, level = "ze")
    saveRDS(map_ze_prepared, file.path(output_dir, "map_ze.RDS"))
    
    shiny::incProgress(0.1, detail = "Mise à jour terminée !")
    Sys.sleep(2) # Petite pause pour que l'utilisateur voie le message final
  })
}