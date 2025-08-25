
# ==============================================================================
# Fichier R/documentation.R (MODIFIÉ AVEC L'INTERFACE DE MISE À JOUR)
# ==============================================================================
documentation_content <- fluidPage(
  style = "padding: 20px; max-width: 900px; margin: auto;",
  
  # --- SECTION DE MISE À JOUR (NOUVEAU) ---
  # Ce panneau ne sera visible que si l'application est lancée en local
  # (c'est une sécurité pour ne pas l'afficher en production sur shinyapps.io)
  if (Sys.getenv("SHINY_PORT") == "") {
    bslib::card(
      bslib::card_header(
        shiny::h4("Panneau d'Administration : Mise à Jour des Données")
      ),
      bslib::card_body(
        shiny::p("Cliquez sur le bouton ci-dessous pour lancer manuellement le processus de mise à jour. L'opération peut prendre plusieurs minutes. L'application se rechargera automatiquement une fois la mise à jour terminée."),
        # Le bouton qui déclenchera la mise à jour
        shiny::actionButton("trigger_data_update", "Lancer la mise à jour des données", 
                            icon = shiny::icon("sync"), class = "btn-warning")
      )
    )
  }
  ,
  
  # --- CONTENU EXISTANT DE LA DOCUMENTATION ---
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