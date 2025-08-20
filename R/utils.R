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

# ------------------------------------------------------------------------------
# SECTION 3 : Fonctions de gestion géographique
# ------------------------------------------------------------------------------
load_and_prepare_map <- function() {
  message("--- Chargement et préparation du fond de carte ---")
  myURL <- "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/georef-france-commune-arrondissement-municipal-millesime/exports/geojson?lang=fr&refine=reg_name%3A%22%C3%8Ele-de-France%22&refine=year%3A%222020%22"
  
  map <- sf::st_read(myURL, quiet = TRUE) %>%
    dplyr::filter(dep_code %in% c("75", "92", "93", "94")) %>%
    dplyr::mutate(dplyr::across(c(com_arm_code, com_arm_name, ept_code, ept_name, dep_code, dep_name), as.character)) %>%
    dplyr::select(com_code = com_arm_code, com_name = com_arm_name, ept_code, ept_name, dep_code, dep_name)
  
  map$ept_name <- ifelse(map$ept_name == "character(0)", "Ville de Paris", map$ept_name)
  map$ept_code <- ifelse(map$ept_name == "Ville de Paris", "T1", map$ept_code)
  
  message("✅ Fond de carte communal OK : ", nrow(map), " communes.")
  return(map)
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

color_switch_ui <- function(id) {
  shiny::div(style = "text-align: right; margin-top: 15px;",
             shinyWidgets::switchInput(inputId = id, label = "Palette accessible", onLabel = "Oui", offLabel = "Non", value = FALSE,
                                       size = "small", inline = TRUE, width = "auto"))
}