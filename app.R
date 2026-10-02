# ==============================================================================
#  app.R — Dashboard SIG « Accès à la végétation » (Marseille)
#  Règle 3-30-300 & Vulnérabilités socio-environnementales | SESSTIM 2026
#
#  Implémente le prompt de construction validé : 4 vues (Vue d'ensemble +
#  3 modules), filtre arrondissements global, sélection multiple des
#  composantes avec logique d'intersection, panneau de détail au clic.
#
#  LANCEMENT
#    1. Vérifier DATA_DIR ci-dessous (doit pointer vers dashboard/data).
#    2. install.packages(c("shiny","bslib","sf","dplyr","tidyr","readr",
#         "leaflet","plotly","DT","ggplot2","scales","stringr","purrr"))
#    3. shiny::runApp("app.R")  ou bouton « Run App » dans RStudio.
#
#  IMPORTANT — À LIRE AU PREMIER LANCEMENT
#    Les noms exacts de colonnes de VEG_BATIMENTS / carreaux / MESE n'ont pas
#    pu être vérifiés au moment d'écrire ce code (données sur la machine de
#    l'utilisateur). L'app RÉSOUT les colonnes au démarrage en essayant
#    plusieurs patrons, et imprime dans la console un DIAGNOSTIC listant, pour
#    chaque colonne logique, le nom réel trouvé (ou « MANQUANT »). Lire ce
#    diagnostic en premier : si une colonne clé est manquante, compléter la
#    liste de patrons correspondante dans la section « RÉSOLUTION DES
#    COLONNES » plus bas, plutôt que de renommer les données.
#
#    Chaque panneau se dégrade proprement (message à la place du graphique)
#    si sa donnée est absente : l'app ne plante pas si un fichier manque.
#
#  AUTEUR : GALBONI Kenneth / QuanTIM — UMR 1252 SESSTIM (AMU, INSERM, IRD)
#
#  MISES À JOUR 28/07/2026 (suite BILAN_SESSION_DASHBOARD_2026-07-28.md) :
#    - Fonds de carte multiples (clair / OSM / satellite / relief) sur les 7
#      cartes leaflet, via un sélecteur de couche natif.
#    - Palette rouge-jaune-vert fixe (0-100%) pour les taux de conformité
#      univariés (carreaux, IRIS) : elle était en dégradé vert monochrome.
#    - Palette bivariée remplacée par les 16 teintes exactes du mémoire
#      (rouge-gris-vert), extraites de la figure source, à la place du
#      bleu-rouge générique.
#    - Boxplot ajouté sous l'histogramme du panneau Distribution (module 1).
#    - Clic carreaux corrigé : les polygones carreaux n'avaient aucun
#      layerId, et le panneau de détail ne regardait de toute façon que la
#      couche IRIS quelle que soit la couche active.
#    - Onglet Profils : la jointure profil_valide échouait sans message
#      exploitable ; un statut diagnostic (PROFIL_STATUT) précise maintenant
#      dans l'interface même quel maillon a cassé (fichier / colonnes /
#      correspondances), avec repli de chemin iris/stats/ puis iris/.
#
#  MISES À JOUR 28/07/2026 (2e passe, sur inspection réelle des données) :
#    - Bâtiments non fonctionnels, cause identifiée : les patrons de colonne
#      "binaire" (^regle_X$) supposaient que "regle_3" était le nom entier de
#      la colonne. Les vraies colonnes sont préfixées ("lidar_h7m_regle_3",
#      "lidar_h7m_regle_3_30"...) : aucun patron ne matchait jamais, colonne
#      toujours résolue à NA, carte bâtiments toujours en erreur. Patrons
#      corrigés en suffixe ; vérifié sur les 7 codes contre la vraie liste de
#      colonnes de batiments_indicateurs.csv.
#    - Profils non fonctionnels, cause identifiée : cumul_deficits_par_IRIS.csv
#      (et distribution_profils.csv, profil_moyen_indicateurs.csv) sont
#      réellement dans profils_vulnerabilite/, jamais dans iris/stats/. Chemin
#      corrigé (f_prof()), avec repli en cascade jusqu'à
#      PROFILS_VULNERABILITE_IRIS.gpkg si aucun CSV n'est trouvé.
#    - Filtre arrondissement désormais actif sur la couche bâtiments (repli
#      via batiments_concordance_brute.csv, seule source confirmée à porter
#      CODE_IRIS pour ces données).
#    - Étiquette carreau au clic : utilise IdINSPIRE (confirmé réel) au lieu
#      de retomber systématiquement sur un simple numéro de ligne.
#
#  MISES À JOUR 28/07/2026 (3e passe) :
#    - Cause réelle du rendu "étoile filante" sur la carte bâtiments trouvée :
#      read_gpkg() tentait une reprojection vers WGS84 mais avalait l'échec en
#      silence et continuait avec l'objet non transformé (toujours en
#      Lambert-93, coordonnées en mètres) tout en journalisant "OK" — leaflet
#      traitait alors des mètres comme des degrés. Corrigé avec repli
#      st_make_valid() + st_transform(), et échec franc (NULL, diagnostic
#      explicite) si la reprojection reste impossible plutôt que de continuer
#      avec des coordonnées fausses.
#    - leafgl retiré entièrement : trois erreurs distinctes et imprévisibles
#      en trois passes (argument data manquant, exigence de cast POLYGON, puis
#      ce rendu aberrant) sans pouvoir vérifier son comportement réel en
#      environnement R. La couche bâtiments repose maintenant uniquement sur
#      addPolygons() classique, fiable à chaque test ; au-delà du plafond,
#      message clair invitant à réduire la sélection d'arrondissements.
#    - KPI "Composante la plus limitante" : séparateur "·" remplacé par ":"
#      (lisait comme un point isolé, surtout au retour à la ligne).
#    - Droite de régression du nuage de points (module bivarié) en rouge.
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(sf); library(dplyr); library(tidyr)
  library(readr); library(leaflet); library(plotly); library(DT)
  library(ggplot2); library(scales); library(stringr); library(purrr)
})

# leafgl (WebGL) rend la couche bâtiments à l'échelle de la ville (10^5
# polygones en quelques secondes), là où le rendu vectoriel de leaflet sature
# le navigateur dès quelques milliers d'entités. Chargé s'il est présent, sinon
# repli propre sur leaflet avec plafond (voir la branche bâtiments du Module 1).
# Installation : install.packages("leafgl").
HAS_LEAFGL <- requireNamespace("leafgl", quietly = TRUE)
if (HAS_LEAFGL) suppressPackageStartupMessages(library(leafgl))

# ==============================================================================
# 0. CONFIGURATION (seule section à adapter)
# ==============================================================================

DATA_DIR <- path.expand("data")

# Décisions §10 du prompt, modifiables ici :
OPT_CARREAU_FILTRE     <- "centroide"   # "centroide" | "intersection"
OPT_QUARTILES_FIXES    <- TRUE          # bornes bivariées fixées sur les 393 IRIS
OPT_LST_RADAR          <- c("lst_moy_2021", "lst_max_2025")  # clés résolues dans COL$mese (cf. §3)
OPT_CAP_BATIMENTS_LEAFLET <- 25000L     # plafond dur du rendu leaflet SVG (si leafgl absent)
OPT_SVG_BATIMENTS         <- 6000L      # au-delà, bascule WebGL (leafgl) : SVG devient trop lent au clic

# Conventions verrouillées (mémoire validé) — couleurs et numérotation ne
# pas modifier. PROFIL_LABELS_FR/EN : proposition à ajuster si besoin, basée
# sur les variables les plus discriminantes de chaque profil dans
# HCPC_desc_var_par_profil.csv (classées par |v.test|) :
#  1 = BuR/Déf.R30/Déf.R3/HRE/NO2 tous élevés -> tissu dense, peu végétalisé
#  2 = EDI/LST/PM2.5 élevés, Déf.R300 élevé   -> défavorisation, chaleur, peu de parcs
#  3 = Déf.R30/R3/EDI tous sous la moyenne    -> situation intermédiaire, favorable de proximité
#  4 = signature BsR/WaR extrême, LST basse, Déf.R3 élevé, n=11 -> petit groupe atypique
#  5 = RoR/NO2/PM/LST tous sous la moyenne, Déf.R300 bas -> résidentiel aéré, bien desservi
PROFIL_COULEURS <- c("1"="#B71C1C","2"="#E65100","3"="#F9A825","4"="#66BB6A","5"="#1B5E20")
PROFIL_LABELS_FR <- c("1"="Cœur dense, peu végétalisé","2"="Précarité sociale et chaleur",
                      "3"="Situation intermédiaire","4"="Périphérie atypique",
                      "5"="Résidentiel aéré et végétalisé")
PROFIL_LABELS_EN <- c("1"="Dense core, low greenery","2"="Social deprivation & heat",
                      "3"="Intermediate profile","4"="Atypical periphery",
                      "5"="Green residential areas")
PROFIL_LABELS <- PROFIL_LABELS_FR
COMP_LABELS     <- c(c3="Composante 3", c30="Composante 30", c300="Composante 300",
                     regle="Règle 3-30-300")
ARR_CODES  <- sprintf("132%02d", 1:16)                 # 13201 .. 13216
ARR_LABELS <- setNames(paste0(1:16, "ᵉ arr."), ARR_CODES)

# ==============================================================================
# 1. UTILITAIRES DE CHARGEMENT ET DE RÉSOLUTION
# ==============================================================================

f_ref  <- function(...) file.path(DATA_DIR, "reference", ...)
f_bat  <- function(...) file.path(DATA_DIR, "batiments", ...)
f_car  <- function(...) file.path(DATA_DIR, "carreaux", ...)
f_iris <- function(...) file.path(DATA_DIR, "iris", ...)
f_stat <- function(...) file.path(DATA_DIR, "iris", "stats", ...)
f_arr  <- function(...) file.path(DATA_DIR, "arrondissement", ...)
# Emplacement réel confirmé par 00_import_inspection_data.R (28/07) : les
# CSV de profils sont dans profils_vulnerabilite/, jamais iris/stats/.
f_prof <- function(...) file.path(DATA_DIR, "profils_vulnerabilite", ...)

DIAG <- list()   # journal de diagnostic (rempli au chargement)
note <- function(cat_, cle, valeur, statut) {
  DIAG[[length(DIAG) + 1L]] <<- data.frame(categorie = cat_, cle = cle,
    valeur = valeur %||% "-", statut = statut, stringsAsFactors = FALSE)
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1]) || identical(a, "")) b else a

read_gpkg <- function(path, to_wgs84 = TRUE, cat_ = "spatial") {
  if (!file.exists(path)) { note(cat_, basename(path), NA, "MANQUANT"); return(NULL) }
  obj <- tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) NULL)
  if (is.null(obj)) { note(cat_, basename(path), NA, "ERREUR_LECTURE"); return(NULL) }
  # Détection d'un CRS mal étiqueté : certains GeoPackage déclarent EPSG:4326
  # (ou aucun CRS) alors qu'ils portent des coordonnées Lambert-93 en mètres.
  # Le contrôle "epsg != 4326" seul saute alors la reprojection et le pipeline
  # traite des mètres comme des degrés (rendu "étoile" / hors carte). On teste
  # l'ordre de grandeur réel : au-delà de 180, ce sont des mètres, pas des
  # degrés. On réétiquette en 2154 (sans reprojeter) pour laisser la
  # reprojection ci-dessous faire son travail correctement.
  bb_raw <- tryCatch(sf::st_bbox(obj), error = function(e) NULL)
  if (!is.null(bb_raw) && is.finite(max(abs(bb_raw))) && max(abs(bb_raw)) > 180) {
    ep <- sf::st_crs(obj)$epsg %||% NA
    if (is.na(ep) || ep == 4326) suppressWarnings(sf::st_crs(obj) <- 2154)
  }
  if (to_wgs84 && !is.na(sf::st_crs(obj)) && (sf::st_crs(obj)$epsg %||% 0) != 4326) {
    obj_t <- tryCatch(sf::st_transform(obj, 4326), error = function(e) NULL)
    if (is.null(obj_t)) {
      # Repli : géométries invalides (fréquent sur de gros jeux, ex. bâtiments
      # BD TOPO) font échouer st_transform(). Sans ce repli, l'ancien code
      # renvoyait silencieusement l'objet non transformé (toujours en
      # Lambert-93, coordonnées en mètres) en journalisant "OK" quand même :
      # c'est exactement ce qui produisait le rendu "étoile filante" sur la
      # carte bâtiments (mètres traités comme degrés par la suite du pipeline
      # leaflet). st_make_valid() corrige la géométrie avant nouvelle tentative.
      obj_t <- tryCatch(sf::st_transform(sf::st_make_valid(obj), 4326), error = function(e) NULL)
    }
    if (!is.null(obj_t)) {
      obj <- obj_t
    } else {
      note(cat_, basename(path), NA, "ERREUR_PROJECTION"); return(NULL)
    }
  }
  note(cat_, basename(path), paste(nrow(obj), "entités"), "OK")
  obj
}
read_tbl <- function(path, cat_ = "table") {
  if (!file.exists(path)) { note(cat_, basename(path), NA, "MANQUANT"); return(NULL) }
  df <- tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  note(cat_, basename(path), if (is.null(df)) NA else paste(nrow(df), "lignes"),
       if (is.null(df)) "ERREUR_LECTURE" else "OK")
  df
}

# Trouve le premier nom de colonne correspondant à l'un des patrons (regex).
find_col <- function(cols, patterns) {
  for (p in patterns) {
    hit <- grep(p, cols, value = TRUE, ignore.case = TRUE, perl = TRUE)
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

# Normalise un CODE_IRIS en chaîne à 9 caractères (préserve le zéro initial).
norm_code_iris <- function(x) stringr::str_pad(as.character(x), 9, pad = "0")
arr_of <- function(code_iris) substr(norm_code_iris(code_iris), 1, 5)

# Détecte la présence de la composante n (3, 30 ou 300) dans un libellé de
# stats_conformite_batiments.csv. Le fichier réel mélange deux conventions :
# "Règle N — ..." pour les lignes simples, "RN" pour les lignes croisées
# ("R3 ∩ R30 ∩ R300"). Un lookahead négatif exclut "3" à l'intérieur de "30"
# ou "300" (et "30" à l'intérieur de "300"), quel que soit le séparateur
# utilisé entre plusieurs composantes (∩, +, virgule...).
has_component <- function(label, n) {
  pat <- sprintf("(?:R[eè]gle|R)\\s*%d(?!\\d)", n)
  grepl(pat, label, ignore.case = TRUE, perl = TRUE, useBytes = FALSE)
}
# Renvoie la ligne de stat_conf_bat dont l'ensemble EXACT des composantes
# présentes correspond à `want` (ex. c(3) -> ligne "Règle 3" seule ;
# c(3,30,300) -> ligne "R3 ∩ R30 ∩ R300"). NULL si stat_conf_bat absent ou
# aucune ligne ne correspond exactement.
stat_bat_row <- function(want) {
  if (is.null(stat_conf_bat) || !nrow(stat_conf_bat)) return(NULL)
  lab <- as.character(stat_conf_bat[[1]])
  want <- sort(as.numeric(want))
  hit <- vapply(lab, function(l) {
    present <- c(3, 30, 300)[vapply(c(3, 30, 300), has_component, logical(1), label = l)]
    identical(present, want)
  }, logical(1))
  if (!any(hit)) return(NULL)
  stat_conf_bat[which(hit)[1], ]
}
# "% conforme" (déjà formaté "NN.N%") de la ligne correspondant à `want`,
# ou NA si absente. Colonnes : Indicateur, N conforme, % conforme (fixe).
stat_bat_pct <- function(want) {
  r <- stat_bat_row(want)
  if (is.null(r)) NA_character_ else as.character(r[["% conforme"]])
}

# ==============================================================================
# 2. CHARGEMENT DES DONNÉES (une fois au démarrage)
# ==============================================================================

message("\n== Chargement des données du dashboard ==")

# --- Couches de référence -----------------------------------------------------
arr_geo   <- read_gpkg(f_ref("arrondissements.gpkg"), cat_ = "reference")
iris_geo  <- read_gpkg(f_ref("iris_boundaries.gpkg"),  cat_ = "reference")
# iris_geo n'est PLUS la source des cartes IRIS : iris_mese.gpkg porte déjà sa
# propre géométrie (confirmé : objet sf, 393 entités). iris_boundaries.gpkg
# reste chargé s'il existe (utile en secours/vérif), mais n'est plus une
# dépendance dure — voir iris_geo_f() plus bas, qui utilise iris_mese direct.
car_geo   <- read_gpkg(f_ref("carreaux_boundaries.gpkg"), cat_ = "reference")
parcs_geo <- read_gpkg(f_ref("parcs_espaces_verts.gpkg"), cat_ = "reference")

# --- Bâtiments ----------------------------------------------------------------
bat_geo   <- read_gpkg(f_bat("bat_geom.gpkg"), cat_ = "batiments")
bat_ind   <- read_tbl(f_bat("bat_ind.csv"), cat_ = "batiments")
# batiments_indicateurs.csv n'a pas de CODE_IRIS (confirmé) : sans lui, le
# filtre arrondissement global ne peut pas s'appliquer à la couche bâtiments.
# batiments_concordance_brute.csv porte CODE_IRIS sur la même clé ID_BAT ;
# col_select limite la lecture à 2 colonnes plutôt que les 65 du fichier.
bat_arr_lookup <- tryCatch({
  p <- f_bat("bat_arr.csv")
  if (file.exists(p)) readr::read_csv(p, col_select = c("ID_BAT","CODE_IRIS"), show_col_types = FALSE) else NULL
}, error = function(e) NULL)

# --- Carreaux -----------------------------------------------------------------
car_dat   <- read_gpkg(f_car("carreaux_200m.gpkg"), cat_ = "carreaux")

# --- IRIS ---------------------------------------------------------------------
iris_mese <- read_gpkg(f_iris("iris_mese.gpkg"), cat_ = "iris")
iris_lstmax <- read_tbl(f_iris("iris_lst_max_2021_2025.csv"), cat_ = "iris")

# Assignation de profil validée (AFM+HCPC, numérotation n=88,103,140,11,50).
# NE PAS utiliser iris_icvse$cluster : cette colonne vient de la méthodologie
# ICVSE (script 06), rejetée dans le mémoire au profit de l'AFM+HCPC. La seule
# numérotation à utiliser est celle de cumul_deficits_par_IRIS.csv, jointe ici
# sur CODE_IRIS. PROFIL_STATUT retient un diagnostic lisible de chaque étape,
# affiché directement dans l'onglet "Profils" si la jointure échoue.
# Chemin réel confirmé par 00_import_inspection_data.R (28/07) :
# profils_vulnerabilite/cumul_deficits_par_IRIS.csv — PAS iris/stats/, qui
# n'existe pas chez l'utilisateur. Les anciens chemins restent en repli au
# cas où l'arborescence diffère d'une machine à l'autre.
profils_valides <- read_tbl(f_prof("cumul_deficits_par_IRIS.csv"), cat_ = "stats")
if (is.null(profils_valides)) profils_valides <- read_tbl(f_stat("cumul_deficits_par_IRIS.csv"), cat_ = "stats")
if (is.null(profils_valides)) profils_valides <- read_tbl(f_iris("cumul_deficits_par_IRIS.csv"), cat_ = "stats")
if (is.null(profils_valides)) {
  # Dernier repli : PROFILS_VULNERABILITE_IRIS.gpkg porte déjà le profil
  # validé (confirmé : 393 entités, colonne 'profil' identique à celle de
  # cumul_deficits_par_IRIS.csv). On n'en extrait que CODE_IRIS + profil,
  # sans en faire la source de iris_mese (éviter un MESE potentiellement
  # désynchronisé si ce gpkg n'est pas régénéré aussi souvent que iris_mese).
  g_prof <- read_gpkg(f_prof("PROFILS_VULNERABILITE_IRIS.gpkg"), cat_ = "stats")
  if (!is.null(g_prof) && "profil" %in% names(g_prof)) {
    ccode <- find_col(names(g_prof), c("^CODE_IRIS$","^code_iris$"))
    if (!is.na(ccode)) profils_valides <- data.frame(
      CODE_IRIS = g_prof[[ccode]], profil = g_prof$profil, stringsAsFactors = FALSE)
  }
}
PROFIL_STATUT <- if (is.null(iris_mese)) {
  "iris_mese.gpkg non chargé (voir diagnostic spatial)."
} else if (is.null(profils_valides)) {
  "fichier cumul_deficits_par_IRIS.csv introuvable (cherché dans profils_vulnerabilite/, iris/stats/, iris/, et PROFILS_VULNERABILITE_IRIS.gpkg)."
} else NA_character_
if (!is.null(iris_mese) && !is.null(profils_valides)) {
  col_code_prof <- find_col(names(profils_valides), c("^CODE_IRIS$", "^code_iris$"))
  col_profil    <- find_col(names(profils_valides), c("^profil$"))
  col_iris_code <- find_col(names(iris_mese), c("^CODE_IRIS$","^code_iris$"))
  if (is.na(col_code_prof) || is.na(col_profil)) {
    PROFIL_STATUT <- sprintf("colonnes CODE_IRIS/profil non identifiées dans cumul_deficits_par_IRIS.csv (colonnes présentes : %s).",
                             paste(names(profils_valides), collapse = ", "))
  } else if (is.na(col_iris_code)) {
    PROFIL_STATUT <- "CODE_IRIS introuvable dans iris_mese.gpkg : jointure impossible."
  } else {
    prof_lookup <- setNames(
      profils_valides[[col_profil]],
      stringr::str_pad(as.character(profils_valides[[col_code_prof]]), 9, pad = "0")
    )
    iris_mese$profil_valide <- unname(prof_lookup[stringr::str_pad(as.character(iris_mese[[col_iris_code]]), 9, pad = "0")])
    n_ok <- sum(!is.na(iris_mese$profil_valide))
    PROFIL_STATUT <- if (n_ok > 0) sprintf("OK : %d / %d IRIS", n_ok, nrow(iris_mese)) else
      "jointure exécutée mais 0 correspondance (format de CODE_IRIS à vérifier)."
    message("  Profil validé joint à iris_mese : ", n_ok, " / ", nrow(iris_mese), " IRIS")
  }
}

# --- Tables statistiques ------------------------------------------------------
# stats_conformite_batiments / stats_desc_regles_IRIS / spearman_veg_mese /
# spearman_veg_lstmax : chemin non vérifié contre les données réelles (pas
# dans le périmètre de 00_import_inspection_data.R) — laissés en iris/stats/
# tant qu'aucun symptôme ne signale un problème. distribution_profils.csv et
# profil_moyen_indicateurs.csv, en revanche, sont confirmés dans
# profils_vulnerabilite/, pas iris/stats/ : corrigés, avec repli conservé.
stat_conf_bat  <- read_tbl(f_stat("stats_conformite_batiments.csv"), cat_ = "stats")
stat_desc_iris <- read_tbl(f_stat("stats_desc_regles_IRIS.csv"), cat_ = "stats")
distrib_prof   <- read_tbl(f_prof("distribution_profils.csv"), cat_ = "stats")
if (is.null(distrib_prof)) distrib_prof <- read_tbl(f_stat("distribution_profils.csv"), cat_ = "stats")
prof_moy       <- read_tbl(f_prof("profil_moyen_indicateurs.csv"), cat_ = "stats")
if (is.null(prof_moy)) prof_moy <- read_tbl(f_stat("profil_moyen_indicateurs.csv"), cat_ = "stats")
spearman_mese  <- read_tbl(f_stat("spearman_veg_mese.csv"), cat_ = "stats")
spearman_lst   <- read_tbl(f_stat("spearman_veg_lstmax.csv"), cat_ = "stats")

# --- Parcs (panneau Composante 300) ------------------------------------------
# Détail bâtiment x espace vert (parcs, Calanques, Nerthe, réserves) : une
# ligne par bâtiment (256 407), avec pour chaque catégorie sa distance/nom/
# surface, plus l'espace retenu (le plus proche, toutes catégories confondues)
# et sa distance. Remplace parcs_desserte_par_parc.csv / _statistiques.csv,
# fichiers absents du jeu de données ; les statistiques par espace sont
# dérivées directement de cette table dans output$m1_parcs.
regle300_bat <- read_tbl(f_bat("bat_parcs.csv"), cat_ = "batiments")

# ==============================================================================
# 3. RÉSOLUTION DES COLONNES (adapter les patrons ici si le diagnostic signale
#    des MANQUANT sur des colonnes utilisées par l'interface)
# ==============================================================================

CODES <- c("3","30","300","3_30","3_300","30_300","3_30_300")
SEL_TO_CODE <- list("c3"="3","c30"="30","c300"="300",
                    "c3.c30"="3_30","c3.c300"="3_300","c30.c300"="30_300",
                    "c3.c30.c300"="3_30_300")

# Patrons pour un indicateur végétation donné (bin = binaire bâtiment ;
# rate = taux agrégé carreau/IRIS). L'ancrage de fin ($) évite qu'un code
# court (r3) capture un code long (r3_30).
patterns_veg <- function(code, type = c("bin","rate")) {
  type <- match.arg(type)
  if (type == "rate") {
    c(paste0("lidar_h7m_r", code, "_tx$"), paste0("r", code, "_tx$"),
      paste0("tx_r", code, "$"), paste0("taux_r", code, "$"),
      paste0("pct_r", code, "$"), paste0("_r", code, "_tx$"),
      paste0("r", code, "_taux$"))
  } else {
    # Confirmé sur batiments_indicateurs.csv réel : "lidar_h7m_regle_3",
    # "lidar_h7m_regle_3_30", etc. — jamais "regle_3" seul. Les anciens
    # patrons ancraient ^...$ sur "regle_X" en entier et ne matchaient donc
    # jamais rien (colonne toujours résolue à NA, carte bâtiments toujours en
    # erreur). Patrons corrigés en suffixe, comme pour "rate" ci-dessus.
    c(paste0("^lidar_h7m_regle_", code, "$"),   # référence LiDAR HD h7m (préférée)
      paste0("_regle_", code, "$"),             # repli : bdtopo/cosia/sfn/ndvi... même convention
      paste0("^regle_", code, "$"),
      paste0("^r", code, "$"), paste0("_r", code, "$"))
  }
}
# Colonne « effectif de bâtiments » d'un carreau/IRIS (dénominateur du taux).
patterns_nbat <- c("^n_?bat$", "^nb_?bat", "n_batiments", "nbre_bat", "^n_bati")

# Résout, pour un jeu de colonnes, la colonne de chaque code (bin ou rate).
resolve_veg <- function(cols, type) {
  setNames(lapply(CODES, function(cd) find_col(cols, patterns_veg(cd, type))), CODES)
}

COL <- list()
if (!is.null(bat_ind))   COL$bat_bin  <- resolve_veg(names(bat_ind), "bin")
if (!is.null(car_dat))   COL$car_rate <- resolve_veg(names(car_dat), "rate")
if (!is.null(iris_mese)) COL$iris_rate <- resolve_veg(names(iris_mese), "rate")

COL$bat_id   <- if (!is.null(bat_ind)) find_col(names(bat_ind), c("^ID_BAT$","^ID$","^id_bat$","^id$")) else NA
COL$bat_geoid<- if (!is.null(bat_geo)) find_col(names(bat_geo), c("^ID$","^ID_BAT$","^id$")) else NA
COL$iris_code<- if (!is.null(iris_mese)) find_col(names(iris_mese), c("^CODE_IRIS$","^code_iris$","^DCOMIRIS$","^CODE$")) else NA
COL$iris_nom <- if (!is.null(iris_mese)) find_col(names(iris_mese), c("^NOM_IRIS$","^nom_iris$")) else NA
COL$iris_geocode <- if (!is.null(iris_geo)) find_col(names(iris_geo), c("^CODE_IRIS$","^code_iris$","^DCOMIRIS$")) else NA
# "profil_valide" (joint depuis cumul_deficits_par_IRIS.csv ci-dessus) est
# TOUJOURS prioritaire : "cluster"/"classe" bruts dans iris_mese ou un gpkg
# tiers peuvent porter la numérotation ICVSE rejetée, jamais à afficher.
COL$iris_prof <- if (!is.null(iris_mese) && "profil_valide" %in% names(iris_mese)) {
  "profil_valide"
} else if (!is.null(iris_mese)) {
  find_col(names(iris_mese), c("^profil$", "^profile$"))  # "cluster"/"classe" retirés exprès
} else NA
# Existence de la colonne ne suffit pas (une jointure à 0 correspondance la
# laisse présente mais entièrement NA) : PROFIL_OK vérifie aussi qu'au moins
# une valeur réelle est disponible. Utilisé pour les messages de l'onglet
# "Profils de vulnérabilité", conjointement à PROFIL_STATUT (texte explicatif).
PROFIL_OK <- !is.null(iris_mese) && !is.na(COL$iris_prof %||% NA) &&
             any(!is.na(iris_mese[[COL$iris_prof]]))
if (PROFIL_OK) PROFIL_STATUT <- sprintf("OK : %d / %d IRIS (colonne '%s')",
  sum(!is.na(iris_mese[[COL$iris_prof]])), nrow(iris_mese), COL$iris_prof)
COL$arr_code  <- if (!is.null(arr_geo)) find_col(names(arr_geo), c("^INSEE_COM$","^CODE_COMM$","^insee_com$","^DEPCOM$")) else NA
COL$car_nbat  <- if (!is.null(car_dat)) find_col(names(car_dat), patterns_nbat) else NA
COL$iris_nbat <- if (!is.null(iris_mese)) find_col(names(iris_mese), patterns_nbat) else NA
COL$car_id    <- if (!is.null(car_dat)) find_col(names(car_dat), c("^idinspire$","^id_?carreau$","^idcar","^code_?carreau$")) else NA

# Variables MESE (module 2 et 3) — patrons corrigés d'après les vrais noms de
# colonnes confirmés dans iris_mese.gpkg (diagnostic du 28/07 : icair_med_2021,
# no2_med_ugm3, pm10_med_moy_ugm3, lst_med_2021 — pas les noms initialement
# devinés). Fallbacks conservés au cas où une régénération future du feature
# store change légèrement les suffixes.
MESE_VARS <- list(
  edi     = c("^edi_score$","^edi$","fdep_edi","^EDI"),
  lcz_bur = c("^lcz_bur_med$","^bur$","built.?up","lcz_bur","bati_ratio"),
  icair365= c("^icair_med_2021$","icair.*2021","icair.?365","^icair"),
  no2     = c("^no2_med_ugm3$","^no2$","^no2_","_no2$"),
  pm10    = c("^pm10_med_moy_ugm3$","^pm10_p90_ugm3$","^pm10$","^pm10_","_pm10$"),
  lst_moy_2021 = c("^lst_med_2021$","lst.*2021.*med","lst.*2021.*moy","lst_moy.*2021","lst.*moy")
)
COL$mese <- if (!is.null(iris_mese)) lapply(MESE_VARS, function(p) find_col(names(iris_mese), p)) else list()

# LST max (module 3, radar) : rejoint depuis iris_lstmax si iris_mese ne les
# porte pas déjà. Colonnes réelles confirmées : lst_max_med_2021 à _2025.
if (!is.null(iris_mese) && !is.null(iris_lstmax)) {
  col_code_lstmax <- find_col(names(iris_lstmax), c("^CODE_IRIS$","^code_iris$"))
  if (!is.na(col_code_lstmax)) {
    key_lstmax <- stringr::str_pad(as.character(iris_lstmax[[col_code_lstmax]]), 9, pad = "0")
    key_mese   <- stringr::str_pad(as.character(iris_mese[[
      find_col(names(iris_mese), c("^CODE_IRIS$","^code_iris$")) %||% "CODE_IRIS"
    ]]), 9, pad = "0")
    for (cln in grep("^lst_max_med_", names(iris_lstmax), value = TRUE)) {
      if (!cln %in% names(iris_mese))
        iris_mese[[cln]] <- iris_lstmax[[cln]][match(key_mese, key_lstmax)]
    }
  }
}
MESE_VARS$lst_max_2021 <- c("^lst_max_med_2021$")
MESE_VARS$lst_max_2025 <- c("^lst_max_med_2025$")
COL$mese$lst_max_2021 <- if (!is.null(iris_mese)) find_col(names(iris_mese), MESE_VARS$lst_max_2021) else NA
COL$mese$lst_max_2025 <- if (!is.null(iris_mese)) find_col(names(iris_mese), MESE_VARS$lst_max_2025) else NA

# Consigne le résultat de résolution dans le diagnostic
res_log <- function(lbl, val) note("colonnes", lbl, val, if (is.na(val)) "MANQUANT" else "OK")
res_log("bâtiment: colonne ID", COL$bat_id)
res_log("bâtiment: Composante 3 (bin)",  COL$bat_bin[["3"]] %||% NA)
res_log("bâtiment: Règle 3-30-300 (bin)", COL$bat_bin[["3_30_300"]] %||% NA)
res_log("carreau: Composante 3 (taux)",  COL$car_rate[["3"]] %||% NA)
res_log("IRIS: Composante 3 (taux)",     COL$iris_rate[["3"]] %||% NA)
res_log("IRIS: CODE_IRIS",  COL$iris_code)
res_log("IRIS: profil",     COL$iris_prof)
note("stats", "profil_valide (jointure)", PROFIL_STATUT, if (isTRUE(PROFIL_OK)) "OK" else "ATTENTION")
res_log("IRIS: EDI",        COL$mese$edi %||% NA)
res_log("IRIS: ICAIR365",   COL$mese$icair365 %||% NA)

# Quintile EDI local à Marseille (demande K, module 3 : le quintile déjà
# présent dans les données sources, edi_Q_nat/edi_Q_num — confirmé dans
# 02_construction_MESE_complete_v12.R, colonnes edi_quintile_nat/edi_Q_nat/
# edi_Q_num — est calculé au niveau NATIONAL et ne doit pas être utilisé ici.
# On recalcule un quintile propre aux 393 IRIS marseillais, cohérent avec le
# verrou méthodologique "edi_Q_local = ntile(edi_score, 5L) sur les 393 IRIS
# marseillais (calcul local, jamais un classement national)". Calculé une
# seule fois ici, sur l'ensemble des 393 IRIS avant tout filtre
# arrondissement : l'appartenance à un quintile ne doit pas changer selon la
# sélection de districts affichée. Q1 = scores EDI les plus bas, Q5 = les
# plus hauts (convention F-EDI : score élevé = défavorisation plus forte).
if (!is.null(iris_mese) && !is.na(COL$mese$edi %||% NA)) {
  iris_mese$edi_Q_mrs <- dplyr::ntile(iris_mese[[COL$mese$edi]], 5L)
}
COL$mese$edi_Q_mrs <- if (!is.null(iris_mese) && "edi_Q_mrs" %in% names(iris_mese)) "edi_Q_mrs" else NA
res_log("IRIS: EDI quintile Marseille (calculé)", COL$mese$edi_Q_mrs %||% NA)

# --- Préparation des géométries de mappage (arrondissement rattaché) ----------
if (!is.null(iris_geo) && !is.na(COL$iris_geocode)) {
  iris_geo$.code <- norm_code_iris(iris_geo[[COL$iris_geocode]])
  iris_geo$.arr  <- substr(iris_geo$.code, 1, 5)
}
if (!is.null(iris_mese) && !is.na(COL$iris_code)) {
  iris_mese$.code <- norm_code_iris(iris_mese[[COL$iris_code]])
  iris_mese$.arr  <- substr(iris_mese$.code, 1, 5)
}
# Carreaux : rattachement arrondissement par centroïde ou intersection
if (!is.null(car_dat) && !is.null(arr_geo) && !is.na(COL$arr_code)) {
  car_dat <- tryCatch({
    pts <- if (OPT_CARREAU_FILTRE == "centroide") suppressWarnings(sf::st_centroid(car_dat)) else car_dat
    j <- sf::st_join(pts["geometry" %in% names(pts)], arr_geo[COL$arr_code],
                     join = sf::st_intersects, left = TRUE)
    car_dat$.arr <- as.character(j[[COL$arr_code]])
    car_dat
  }, error = function(e) { car_dat$.arr <- NA_character_; car_dat })
}

# Impression du diagnostic
diag_df <- if (length(DIAG)) do.call(rbind, DIAG) else data.frame()
message("\n== DIAGNOSTIC DE CHARGEMENT ==")
if (nrow(diag_df)) apply(diag_df, 1, function(r)
  message(sprintf("  [%-9s] %-32s %-14s %s", r["statut"], r["cle"],
                  substr(r["valeur"],1,14), r["categorie"])))
message("== fin diagnostic ==\n")

# ==============================================================================
# 4. HELPERS DE CALCUL
# ==============================================================================

# Nom d'affichage d'un IRIS (NOM_IRIS) plutôt que son CODE_IRIS brut. Le code
# reste la clé interne (layerId, jointures) ; seul l'affichage change.
iris_nom_of <- function(code) {
  if (is.null(iris_mese) || is.na(COL$iris_nom %||% NA)) return(code)
  nm <- iris_mese[[COL$iris_nom]][match(code, iris_mese$.code)]
  ifelse(is.na(nm) | nm == "", code, nm)
}

# Ensemble de composantes -> code d'intersection
sel_to_code <- function(sel) {
  if (length(sel) == 0) return(NA_character_)
  key <- paste(sort(sel), collapse = ".")
  SEL_TO_CODE[[key]] %||% NA_character_
}

# Filtre un data.frame/sf sur les arrondissements actifs via une colonne .arr
filtre_arr <- function(x, arr_actifs) {
  if (is.null(x) || is.null(arr_actifs) || length(arr_actifs) == 16) return(x)
  if (!".arr" %in% names(x)) return(x)
  x[x$.arr %in% arr_actifs, ]
}

# Palette bivariée 4x4 (lignes = quartile de la variable croisée croissant ;
# colonnes = quartile végétation croissant). Reprend exactement le schéma
# rouge (mauvais) / gris (neutre) / vert (bon) déjà validé dans le mémoire
# (figure "R3 x EDI"), couleurs extraites des nuances de la légende source —
# pas une approximation. Valide pour EDI, LCZ_bur, ICAIR365 et LST : dans les
# 4 cas, "variable croisée élevée" est défavorable, donc même orientation.
BIV_PAL <- matrix(c(
  "#e8e8e8","#a3cdb0","#5fb179","#1a9641",   # var Q1 (faible)
  "#c99b9b","#968e76","#638150","#2f742b",   # var Q2
  "#aa4d4d","#884f3b","#665028","#455216",   # var Q3
  "#8b0000","#7b1000","#6a2000","#5a3000"),  # var Q4 (élevée)
  nrow = 4, byrow = TRUE)
biv_class <- function(vx, vy) {  # vx, vy : quartiles 1..4
  ifelse(is.na(vx) | is.na(vy), NA_character_, BIV_PAL[cbind(vy, vx)])
}
quartile <- function(x, breaks = NULL) {
  if (is.null(breaks)) breaks <- quantile(x, c(0,.25,.5,.75,1), na.rm = TRUE)
  cut(x, unique(breaks), include.lowest = TRUE, labels = FALSE)
}

# Palette rouge-jaune-vert fixe (0-100%) pour tous les taux de conformité
# univariés (carreaux, IRIS) : mêmes teintes que la légende "Taux conformité"
# du mémoire. Domaine fixé (pas recalculé par sélection d'arrondissements) :
# une couleur donnée garde toujours le même sens, quel que soit le filtre.
PAL_CONFORMITE <- colorNumeric(
  palette = c("#d73027","#fc8d59","#fee08b","#91cf60","#1a9850"),
  domain = c(0, 100), na.color = "#eeeeee")

# Fonds de carte multiples (sélecteur natif Leaflet, en haut à droite de
# chaque carte) : plan clair, OpenStreetMap, satellite, relief.
BASEMAPS <- c("Fond clair" = "CartoDB.Positron", "OpenStreetMap" = "OpenStreetMap",
             "Satellite" = "Esri.WorldImagery", "Relief" = "Esri.WorldTopoMap")
add_basemaps <- function(m) {
  for (nm in names(BASEMAPS)) m <- addProviderTiles(m, unname(BASEMAPS[nm]), group = nm)
  addLayersControl(m, baseGroups = names(BASEMAPS),
                   options = layersControlOptions(collapsed = TRUE, position = "topright"))
}

# Bouton plein écran explicite, en haut à droite de la carte. bslib propose un
# agrandissement au survol (coin bas-droit) trop discret pour un public non
# expert : ce contrôle affiche une icône claire qui bascule le conteneur de la
# carte en plein écran via l'API Fullscreen du navigateur, avec redimensionnement
# de la carte à l'entrée comme à la sortie.
add_fullscreen_btn <- function(m) {
  btn <- htmltools::HTML(paste0(
    "<a href='#' id='fs-btn' role='button' title='Plein écran / Fullscreen' ",
    "aria-label='Plein écran' style='background:#fff;width:34px;height:34px;",
    "display:flex;align-items:center;justify-content:center;box-shadow:0 1px 4px ",
    "rgba(0,0,0,.3);border-radius:4px;font-size:20px;line-height:1;text-decoration:",
    "none;color:#333'>&#x26F6;</a>"))
  m |>
    leaflet::addControl(html = btn, position = "topright", className = "fs-ctrl") |>
    htmlwidgets::onRender("
      function(el, x) {
        // Plein écran en CSS pur (position:fixed sur le conteneur du widget),
        // plutôt que l'API Fullscreen native du navigateur. L'API native est
        // refusée silencieusement quand l'app tourne dans une iframe sans
        // l'attribut allow=\"fullscreen\" (cas du Viewer intégré de RStudio :
        // le clic déclenche bien le handler mais requestFullscreen() ne fait
        // rien). L'approche CSS fonctionne à l'identique dans le Viewer, un
        // navigateur classique, ou une fois l'app publiée.
        var STYLE_ID = 'claude-fs-style';
        if (!document.getElementById(STYLE_ID)) {
          var st = document.createElement('style');
          st.id = STYLE_ID;
          st.innerHTML =
            '.claude-map-fullscreen{position:fixed !important;top:0 !important;' +
            'left:0 !important;right:0 !important;bottom:0 !important;' +
            'width:100vw !important;height:100vh !important;z-index:99999 !important;' +
            'background:#fff;}' +
            'body.claude-fs-lock{overflow:hidden !important;}' +
            '.claude-map-fullscreen #fs-btn{background:#2962FF !important;color:#fff !important;}';
          document.head.appendChild(st);
        }
        var map = this;
        var doInvalidate = function(){
          setTimeout(function(){ try { map.invalidateSize(); } catch (e) {} }, 260);
        };
        var toggle = function(){
          var on = el.classList.toggle('claude-map-fullscreen');
          document.body.classList.toggle('claude-fs-lock', on);
          var b = el.querySelector('#fs-btn');
          if (b) b.title = on
            ? 'Quitter le plein écran / Exit fullscreen'
            : 'Plein écran / Fullscreen';
          doInvalidate();
        };
        if (!el.dataset.fsWired) {
          el.dataset.fsWired = '1';
          document.addEventListener('keydown', function(e){
            if (e.key === 'Escape' && el.classList.contains('claude-map-fullscreen')) toggle();
          });
        }
        setTimeout(function(){
          var b = el.querySelector('#fs-btn');
          if (!b) return;
          b.onclick = function(e){ e.preventDefault(); e.stopPropagation(); toggle(); };
        }, 300);
      }")
}

# ==============================================================================
# 5. INTERFACE
# ==============================================================================

# --- Internationalisation (EN par défaut, FR via ?lang=fr) -------------------
# Simple et robuste : la langue vient du paramètre d'URL, l'UI entière est
# reconstruite par requête (ui <- function(req)), donc même les titres
# d'onglets (statiques par nature dans Shiny) suivent la langue choisie. Le
# switch de langue est un lien classique (rechargement de page) : pas besoin
# de JS ni de package i18n externe pour un résultat fiable.
get_lang <- function(qs) {
  qs <- qs %||% ""
  if (!startsWith(qs, "?")) qs <- paste0("?", qs)
  q <- tryCatch(shiny::parseQueryString(qs), error = function(e) list())
  if (!is.null(q$lang) && tolower(q$lang) %in% c("fr","en")) tolower(q$lang) else "en"
}

I18N <- list(
  fr = list(
    app_title = "Accès à la végétation — Marseille",
    arr_label = "Arrondissements affichés", arr_placeholder = "Tous les arrondissements",
    btn_all = "Tout", btn_none = "Aucun",
    nav_overview = "Vue d'ensemble", nav_m1 = "Analyse univariée",
    nav_m2 = "Analyse bivariée", nav_m3 = "Profils de vulnérabilité",
    kpi_conf_bat = "Conformité 3-30-300 (bâtiments)", kpi_conf_iris = "Conformité 3-30-300 (IRIS, moy.)",
    kpi_limit_title = "Composante la plus limitante", kpi_niris = "IRIS analysés", kpi_nbat = "Bâtiments",
    ov_map_header = "Conformité Règle 3-30-300 — aperçu IRIS",
    ov_prof_header = "Répartition des 5 profils de vulnérabilité",
    m1_comp_label = "Composantes (sélection multiple = intersection)",
    m1_regle_label = "Règle 3-30-300 (les 3 ensemble)", m1_couche_label = "Couche cartographique",
    couche_bat = "Bâtiments", couche_car = "Carreaux", couche_iris = "IRIS",
    m1_help = "Les statistiques suivent la couche choisie.",
    click_header = "Détail au clic", distrib_header = "Distribution", conf_table_header = "Taux de respect et effectifs",
    m2_scale_note = "Échelle IRIS uniquement.", m2_veg_label = "Indicateur végétation",
    m2_var_label = "Variable croisée",
    var_edi = "EDI (défaveur sociale)", var_lcz = "LCZ (bâti)",
    var_icair = "ICAIR365 (pollution)", var_lst = "LST (température)",
    m2_lst_label = "Millésime LST",
    lst_moy_2021 = "Moyenne 2021", lst_max_2021 = "Max 2021", lst_max_2022 = "Max 2022",
    lst_max_2023 = "Max 2023", lst_max_2024 = "Max 2024", lst_max_2025 = "Max 2025",
    m2_map_header = "Carte bivariée — quartiles croisés (4×4)",
    m2_scatter_header = "Nuage de points", m2_corr_header = "Corrélation",
    m3_help = "Cliquer un IRIS sur la carte pour l'ajouter à la comparaison.",
    m3_reset = "Réinitialiser la sélection", m3_map_header = "Carte des 5 profils",
    m3_radar_header = "Radar comparatif", m3_table_header = "Tableau comparatif des IRIS sélectionnés",
    m3_profils_title = "Profils",
    comp3 = "Composante 3", comp30 = "Composante 30", comp300 = "Composante 300",
    regle = "Règle 3-30-300", inter_3_30 = "Composantes 3 ∩ 30", inter_3_300 = "Composantes 3 ∩ 300",
    inter_30_300 = "Composantes 30 ∩ 300", inter_3_30_300 = "Règle 3-30-300",
    click_empty = "Cliquez une entité sur la carte.", click_empty_iris = "Cliquez un IRIS sur la carte.",
    click_empty_m3 = "Cliquez un ou plusieurs IRIS sur la carte.",
    bat_cap_msg = "Trop de bâtiments à afficher (%s). Réduisez la sélection d'arrondissements pour les voir un par un.",
    bat_titre = "Bâtiment #%d", bat_statut = "Statut", conforme = "Conforme", non_conforme = "Non conforme",
    carreau_titre = "Carreau %s", variable_col = "Variable", profil_col = "Profil",
    tx_conformite = "Taux de conformité (%)", effectif = "Effectif", batiments = "Bâtiments",
    pct_iris = "% des IRIS", ind_veg = "Indicateur végétation", var_croisee = "Variable croisée",
    lang_switch = "FR"
  ),
  en = list(
    app_title = "Vegetation Access — Marseille",
    arr_label = "Districts shown", arr_placeholder = "All districts",
    btn_all = "All", btn_none = "None",
    nav_overview = "Overview", nav_m1 = "Univariate analysis",
    nav_m2 = "Bivariate analysis", nav_m3 = "Vulnerability profiles",
    kpi_conf_bat = "3-30-300 compliance (buildings)", kpi_conf_iris = "3-30-300 compliance (IRIS, avg.)",
    kpi_limit_title = "Most limiting component", kpi_niris = "IRIS analyzed", kpi_nbat = "Buildings",
    ov_map_header = "3-30-300 rule compliance — IRIS overview",
    ov_prof_header = "Distribution of the 5 vulnerability profiles",
    m1_comp_label = "Components (multiple selection = intersection)",
    m1_regle_label = "3-30-300 rule (all three together)", m1_couche_label = "Map layer",
    couche_bat = "Buildings", couche_car = "Grid cells", couche_iris = "IRIS",
    m1_help = "Statistics follow the selected layer.",
    click_header = "Click detail", distrib_header = "Distribution", conf_table_header = "Compliance rate and counts",
    m2_scale_note = "IRIS scale only.", m2_veg_label = "Vegetation indicator",
    m2_var_label = "Crossed variable",
    var_edi = "EDI (social deprivation)", var_lcz = "LCZ (built form)",
    var_icair = "ICAIR365 (air pollution)", var_lst = "LST (temperature)",
    m2_lst_label = "LST vintage",
    lst_moy_2021 = "2021 average", lst_max_2021 = "2021 max", lst_max_2022 = "2022 max",
    lst_max_2023 = "2023 max", lst_max_2024 = "2024 max", lst_max_2025 = "2025 max",
    m2_map_header = "Bivariate map — crossed quartiles (4×4)",
    m2_scatter_header = "Scatter plot", m2_corr_header = "Correlation",
    m3_help = "Click an IRIS on the map to add it to the comparison.",
    m3_reset = "Reset selection", m3_map_header = "Map of the 5 profiles",
    m3_radar_header = "Comparative radar", m3_table_header = "Comparison table of selected IRIS",
    m3_profils_title = "Profiles",
    comp3 = "Component 3", comp30 = "Component 30", comp300 = "Component 300",
    regle = "3-30-300 rule", inter_3_30 = "Components 3 and 30", inter_3_300 = "Components 3 and 300",
    inter_30_300 = "Components 30 and 300", inter_3_30_300 = "3-30-300 rule",
    click_empty = "Click an entity on the map.", click_empty_iris = "Click an IRIS on the map.",
    click_empty_m3 = "Click one or more IRIS on the map.",
    bat_cap_msg = "Too many buildings to display (%s). Narrow the district selection to see them individually.",
    bat_titre = "Building #%d", bat_statut = "Status", conforme = "Compliant", non_conforme = "Non-compliant",
    carreau_titre = "Grid cell %s", variable_col = "Variable", profil_col = "Profile",
    tx_conformite = "Compliance rate (%)", effectif = "Count", batiments = "Buildings",
    pct_iris = "% of IRIS", ind_veg = "Vegetation indicator", var_croisee = "Crossed variable",
    lang_switch = "EN"
  )
)
tr <- function(key, lang = "en") {
  v <- I18N[[lang]][[key]]
  if (is.null(v)) I18N[["en"]][[key]] %||% key else v
}

# Fichiers confirmés dans www/ (capture d'écran K) : quantim.png, sesstim.png,
# logo_amu.png, et une variante "sesstim-...-tutelles.png" (nom tronqué à la
# capture) : plusieurs candidats essayés, le premier trouvé est utilisé.
www_file <- function(candidates) {
  hit <- candidates[file.exists(file.path("www", candidates))]
  if (length(hit)) hit[1] else NA_character_
}
logo_tag <- function() {
  # ISSPAM retiré (demande K, 30/07 : aucun lien entre ce projet et
  # l'ISSPAM). logo_amu.png désigne bien AMU (vérifié par K : preview Finder
  # confirmant "amU / Aix Marseille Université", sans ISSPAM).
  # Anti-cache : chaque <img src=...> reçoit un paramètre ?v=<horodatage de
  # modification du fichier>. Même nom de fichier, contenu remplacé -> URL
  # différente -> le navigateur ne peut pas servir une version en cache
  # périmée. Corrige la confusion répétée observée sur logo_amu.png (le nom
  # de fichier avait porté plusieurs contenus différents au fil des sessions).
  img <- function(f, h = "32px") {
    mtime <- tryCatch(as.integer(file.info(file.path("www", f))$mtime),
                      error = function(e) NA_integer_)
    src <- if (!is.na(mtime)) paste0(f, "?v=", mtime) else f
    tags$img(src = src, height = h, style = "vertical-align:middle;")
  }
  lien <- function(url, ...) tags$a(href = url, target = "_blank",
                                    rel = "noopener noreferrer",
                                    style = "margin-left:12px;display:inline-block;", ...)
  f_amu     <- www_file(c("logo_amu.png", "logo_amu_seul.png", "logo_partenaire_noir_RVB.png"))
  f_sesstim <- www_file(c("sesstim.png","sesstim-umr1252-tutelles.png","sesstimumr1252tutelles.png"))
  f_quantim <- www_file(c("quantim.png"))
  f_marseille <- www_file(c("Armoiries_de_Marseille.svg","armoiries_de_marseille.svg",
                            "armoiries-marseille.svg","logo_marseille.svg","logo_marseille.png"))
  f_spf <- www_file(c("SpF.svg","spf.svg","SPF.svg","logo_spf.svg","logo_spf.png"))
  tagList(
    if (!is.na(f_amu))      lien("https://www.univ-amu.fr/", img(f_amu)),
    if (!is.na(f_sesstim))  lien("https://sesstim.univ-amu.fr/fr", img(f_sesstim)),
    if (!is.na(f_quantim))  lien("https://sesstim.univ-amu.fr/fr/equipe-quantim", img(f_quantim)),
    if (!is.na(f_marseille)) lien("https://www.marseille.fr/", img(f_marseille)),
    if (!is.na(f_spf))      lien("https://www.santepubliquefrance.fr/", img(f_spf))
  )
}

ui <- function(req) {
lang <- get_lang(req$QUERY_STRING)
tt <- function(key) tr(key, lang)
page_navbar(
  # Le titre ne va plus dans le navbar-brand (toujours à gauche par défaut,
  # non centrable proprement à côté des onglets) : il est déplacé dans le
  # bandeau ci-dessous, seul, centré, comme "Le SESSTIM" sur le site de
  # référence. `title=NULL` laisse le navbar-brand vide.
  title = NULL,
  # Couleurs SESSTIM mesurées directement sur la page d'accueil réelle
  # (sesstim.univ-amu.fr), pixel par pixel, pas estimées : bleu #4472B5
  # (titre "Le SESSTIM" ET pastille "L'enseignement", deux occurrences
  # concordantes), navy #2A3052 (bandeau d'en-tête ET texte de nav), orange
  # #D75A38 (pastille "La recherche"). Remplace l'approximation précédente
  # tirée du seul fichier logo.
  theme = bs_theme(version = 5, primary = "#4472B5", warning = "#D75A38"),
  id = "nav",
  header = tagList(
    tags$style(HTML(
      ".arr-bar{padding:8px 14px;background:#eef3f9;border-bottom:1px solid #d7e3ef;}
       .navbar{border-bottom:3px solid #2A3052;}
       .navbar-brand{color:#0D2A4A !important;font-weight:600;}
       .nav-link.active{color:#4472B5 !important;border-color:#4472B5 !important;
                        border-bottom:3px solid #4472B5 !important;}
       .app-hero{width:100%;background:#ffffff;text-align:center;
                 padding:22px 12px 16px;border-bottom:1px solid #e5eaf1;}
       .app-hero h1{margin:0;font-weight:700;font-size:2.1rem;color:#4472B5;
                    letter-spacing:.2px;}")),
    # Bandeau titre, en haut, centré, bilingue via tt(\"app_title\") (bascule
    # FR/EN au clic sur le sélecteur de langue, cf. lang_switch plus bas :
    # rechargement de page avec ?lang=..., donc titre entièrement retraduit).
    # Fond blanc comme la section "Le SESSTIM" de référence : la barre de nav
    # elle-même reste claire (fond par défaut), car logo_partenaire_noir_RVB.png
    # est explicitement la variante SOMBRE du logo ("noir" dans son nom) —
    # confirmé par le site SESSTIM réel, qui utilise un fichier "amu-blanc.png"
    # séparé spécifiquement pour sa propre barre navy. Foncer notre barre de
    # nav aurait rendu ce logo illisible.
    div(class = "app-hero", tags$h1(tt("app_title"))),
    div(class = "arr-bar",
      fluidRow(
        column(8, selectizeInput("arr", tt("arr_label"),
          choices = setNames(ARR_CODES, ARR_LABELS), selected = ARR_CODES,
          multiple = TRUE, width = "100%",
          options = list(plugins = list("remove_button"),
                         placeholder = tt("arr_placeholder")))),
        column(4, div(style = "padding-top:26px;",
          actionButton("arr_all", tt("btn_all"), class = "btn-sm"),
          actionButton("arr_none", tt("btn_none"), class = "btn-sm"),
          span(textOutput("arr_resume", inline = TRUE), style = "margin-left:8px;color:#607d8b;")))
      ))
  ),

  # ---- Vue d'ensemble --------------------------------------------------------
  nav_panel(tt("nav_overview"), icon = icon("house"),
    layout_columns(fill = FALSE,
      value_box(tt("kpi_conf_bat"), textOutput("kpi_bat"), theme = "success"),
      value_box(tt("kpi_conf_iris"), textOutput("kpi_iris"), theme = "success"),
      value_box(tt("kpi_limit_title"), uiOutput("kpi_limit"), theme = "warning"),
      value_box(tt("kpi_niris"), textOutput("kpi_niris")),
      value_box(tt("kpi_nbat"), textOutput("kpi_nbat"))
    ),
    layout_columns(
      card(card_header(tt("ov_map_header")), leafletOutput("ov_map", height = 340)),
      card(card_header(tt("ov_prof_header")), plotlyOutput("ov_prof", height = 340))
    )
  ),

  # ---- Module 1 : univarié ---------------------------------------------------
  nav_panel(tt("nav_m1"), icon = icon("map"),
    layout_sidebar(
      sidebar = sidebar(width = 300,
        checkboxGroupInput("m1_comp", tt("m1_comp_label"),
          choices = setNames(c("c3","c30","c300"), c(tt("comp3"),tt("comp30"),tt("comp300"))),
          selected = "c3"),
        checkboxInput("m1_regle", tt("m1_regle_label"), FALSE),
        hr(),
        radioButtons("m1_couche", tt("m1_couche_label"),
          choices = setNames(c("bat","car","iris"), c(tt("couche_bat"),tt("couche_car"),tt("couche_iris"))),
          selected = "iris"),
        helpText(tt("m1_help"))
      ),
      layout_columns(col_widths = c(7,5),
        card(card_header(textOutput("m1_map_titre")), leafletOutput("m1_map", height = 460)),
        div(
          card(card_header(tt("click_header")), uiOutput("m1_click")),
          card(card_header(tt("distrib_header")), plotlyOutput("m1_distrib", height = 300)),
          card(card_header(tt("conf_table_header")), uiOutput("m1_conf_table")),
          uiOutput("m1_parcs_card")
        )
      )
    )
  ),

  # ---- Module 2 : bivarié ----------------------------------------------------
  nav_panel(tt("nav_m2"), icon = icon("braille"),
    layout_sidebar(
      sidebar = sidebar(width = 300,
        p(icon("map-pin"), tt("m2_scale_note")),
        radioButtons("m2_veg", tt("m2_veg_label"),
          choices = setNames(c("3","30","300","3_30_300"),
                             c(tt("comp3"),tt("comp30"),tt("comp300"),tt("regle"))),
          selected = "3_30_300"),
        radioButtons("m2_var", tt("m2_var_label"),
          choices = setNames(c("edi","lcz_bur","icair365","lst"),
                             c(tt("var_edi"),tt("var_lcz"),tt("var_icair"),tt("var_lst"))),
          selected = "edi"),
        conditionalPanel("input.m2_var == 'lst'",
          selectInput("m2_lst", tt("m2_lst_label"),
            choices = setNames(
              c("lst_moy_2021","lst_max_2021","lst_max_2022","lst_max_2023","lst_max_2024","lst_max_2025"),
              c(tt("lst_moy_2021"),tt("lst_max_2021"),tt("lst_max_2022"),tt("lst_max_2023"),tt("lst_max_2024"),tt("lst_max_2025")))))
      ),
      layout_columns(col_widths = c(7,5),
        card(card_header(tt("m2_map_header")), leafletOutput("m2_map", height = 460)),
        div(
          card(card_header(tt("click_header")), uiOutput("m2_click")),
          card(card_header(tt("m2_scatter_header")), plotlyOutput("m2_scatter", height = 300)),
          card(card_header(tt("m2_corr_header")), uiOutput("m2_corr"))
        )
      )
    )
  ),

  # ---- Module 3 : profils ----------------------------------------------------
  nav_panel(tt("nav_m3"), icon = icon("layer-group"),
    layout_sidebar(
      sidebar = sidebar(width = 300,
        p(tt("m3_help")),
        actionButton("m3_reset", tt("m3_reset"), class = "btn-sm"),
        hr(), uiOutput("m3_legende")
      ),
      layout_columns(col_widths = c(7,5),
        card(card_header(tt("m3_map_header")), leafletOutput("m3_map", height = 460)),
        card(card_header(tt("m3_radar_header")), plotlyOutput("m3_radar", height = 340))
      ),
      card(card_header(tt("m3_table_header")), DTOutput("m3_table"))
    )
  ),

  nav_spacer(),
  nav_item(tags$span(
    if (lang == "fr") tags$b("FR", style = "color:#0D2A4A;")
    else tags$a(href = "?lang=fr", "FR", style = "color:#4472B5;"),
    tags$span(" / ", style = "color:#0D2A4A;"),
    if (lang == "en") tags$b("EN", style = "color:#0D2A4A;")
    else tags$a(href = "?lang=en", "EN", style = "color:#4472B5;"),
    style = "margin-right:10px;"
  )),
  nav_item(logo_tag())
)
}

# ==============================================================================
# 6. SERVEUR
# ==============================================================================

server <- function(input, output, session) {

  lang <- reactive(get_lang(session$clientData$url_search))
  tt <- function(key) tr(key, lang())
  profil_lbl <- function() if (lang() == "fr") PROFIL_LABELS_FR else PROFIL_LABELS_EN

  # ---- Filtre arrondissements global ----------------------------------------
  arr_actifs <- reactive(input$arr %||% ARR_CODES)
  observeEvent(input$arr_all,  updateSelectizeInput(session, "arr", selected = ARR_CODES))
  observeEvent(input$arr_none, updateSelectizeInput(session, "arr", selected = character(0)))
  output$arr_resume <- renderText({
    n <- length(arr_actifs())
    if (n == 16) tt("arr_label") else paste0(n, " / 16")
  })

  iris_f <- reactive(filtre_arr(iris_mese, arr_actifs()))
  car_f  <- reactive(filtre_arr(car_dat, arr_actifs()))
  # Source de géométrie IRIS = iris_mese directement (il porte déjà sa propre
  # géométrie), pas iris_boundaries.gpkg : élimine la dépendance à un fichier
  # de contour séparé, qui s'est révélé fragile (écriture gpkg interrompue).
  iris_geo_f <- reactive({
    if (is.null(iris_mese)) return(NULL)
    filtre_arr(iris_mese, arr_actifs())
  })

  # bat_ind filtré par arrondissement actif : même jointure ID_BAT ->
  # CODE_IRIS -> arrondissement que m1_bat_data(), extraite ici pour être
  # réutilisable par les KPI de la vue d'ensemble (résultat brut par bâtiment,
  # jamais une moyenne d'IRIS, cf. kpi_bat / kpi_limit plus bas).
  bat_ind_arr <- reactive({
    if (is.null(bat_ind)) return(NULL)
    d <- bat_ind
    if (length(arr_actifs()) < 16 && !is.null(bat_arr_lookup)) {
      arr_vec <- arr_of(bat_arr_lookup$CODE_IRIS[match(d$ID_BAT, bat_arr_lookup$ID_BAT)])
      keep <- arr_vec %in% arr_actifs(); keep[is.na(keep)] <- FALSE
      d <- d[keep, ]
    }
    d
  })

  # ===========================================================================
  # VUE D'ENSEMBLE
  # ===========================================================================
  output$kpi_bat  <- renderText({
    # Ville entière : la table de stats verrouillée (mémoire) fait foi.
    if (length(arr_actifs()) == 16) {
      p <- stat_bat_pct(c(3, 30, 300))
      if (!is.na(p)) return(p)
    }
    # Sélection partielle d'arrondissements, ou table absente : calcul direct
    # sur bat_ind filtré, jamais une moyenne d'IRIS (les deux sont des
    # statistiques différentes : voir kpi_iris, qui reste volontairement la
    # moyenne des taux par IRIS).
    d <- bat_ind_arr(); cc <- COL$bat_bin[["3_30_300"]]
    if (is.null(d) || is.na(cc %||% NA) || !nrow(d)) return("—")
    sprintf("%.1f%%", 100 * mean(d[[cc]] == 1, na.rm = TRUE))
  })
  output$kpi_iris <- renderText({
    d <- iris_f(); cc <- COL$iris_rate[["3_30_300"]]
    if (is.null(d) || is.na(cc %||% NA)) return("—")
    v <- mean(d[[cc]], na.rm = TRUE); sprintf("%.1f%%", v * ifelse(v <= 1, 100, 1))
  })
  output$kpi_limit <- renderUI({
    labs  <- c("3"=tt("comp3"), "30"=tt("comp30"), "300"=tt("comp300"))
    codes <- list("3"=3, "30"=30, "300"=300)
    vide  <- HTML(sprintf("%s:<br>—", tt("comp300")))

    vals <- NA_real_
    if (length(arr_actifs()) == 16) {
      p <- vapply(codes, function(w) stat_bat_pct(w), character(1))
      vals <- suppressWarnings(as.numeric(sub("%$", "", p)))
    }
    if (all(is.na(vals))) {
      d <- bat_ind_arr()
      if (is.null(d) || !nrow(d)) return(vide)
      vals <- vapply(names(labs), function(cd) {
        cc <- COL$bat_bin[[cd]]
        if (is.na(cc %||% NA)) return(NA_real_)
        100 * mean(d[[cc]] == 1, na.rm = TRUE)
      }, numeric(1))
    }
    if (all(is.na(vals))) return(vide)
    i <- which.min(vals)
    HTML(sprintf("%s:<br>%.1f%%", labs[i], vals[i]))
  })
  output$kpi_niris <- renderText({ d <- iris_f(); if (is.null(d)) "393" else as.character(nrow(d)) })
  output$kpi_nbat  <- renderText({
    if (is.null(bat_ind)) return("256 407")
    format(nrow(bat_ind), big.mark = " ")
  })

  output$ov_prof <- renderPlotly({
    validate(need(!is.null(distrib_prof), if (lang()=="fr") "distribution_profils.csv introuvable (attendu dans profils_vulnerabilite/)." else "distribution_profils.csv not found (expected in profils_vulnerabilite/)."))
    d <- distrib_prof
    pc <- find_col(names(d), c("^profil$","^cluster$")); nc <- find_col(names(d), c("^pct$","pourc"))
    validate(need(!is.na(pc) && !is.na(nc), "Colonnes profil/pct non identifiées dans distribution_profils.csv."))
    pl <- profil_lbl()
    d$col <- PROFIL_COULEURS[as.character(d[[pc]])]
    d$lbl <- factor(pl[as.character(d[[pc]])], levels = pl)
    plot_ly(d, x = ~get(nc), y = ~lbl, type = "bar", orientation = "h",
            marker = list(color = ~col), hovertemplate = "%{x}%<extra></extra>") |>
      layout(xaxis = list(title = tt("pct_iris")), yaxis = list(title = ""), showlegend = FALSE)
  })

  output$ov_map <- renderLeaflet({
    g <- iris_geo_f(); validate(need(!is.null(g), "Contour IRIS manquant"))
    cc <- COL$iris_rate[["3_30_300"]]
    if (!is.null(iris_mese) && !is.na(cc %||% NA)) {
      vals <- iris_mese[[cc]][match(g$.code, iris_mese$.code)]
      mult <- ifelse(max(vals, na.rm = TRUE) <= 1, 100, 1)
      vals <- vals * mult
      leaflet(g) |> add_basemaps() |> add_fullscreen_btn() |>
        addPolygons(fillColor = ~PAL_CONFORMITE(vals), fillOpacity = .75, weight = .5, color = "#ffffff",
                    label = ~sprintf("%s : %.0f%%", iris_nom_of(.code), vals)) |>
        addLegend(pal = PAL_CONFORMITE, values = c(0, 100), title = "Conformité",
                  labFormat = labelFormat(suffix = " %"))
    } else {
      leaflet(g) |> add_basemaps() |> add_fullscreen_btn() |>
        addPolygons(weight = .5, color = "#1D9E75", fillOpacity = .3)
    }
  })

  # ===========================================================================
  # MODULE 1 — UNIVARIÉ
  # ===========================================================================
  # Synchronisation composantes <-> Règle 3-30-300
  observeEvent(input$m1_comp, {
    all3 <- setequal(input$m1_comp, c("c3","c30","c300"))
    if (all3 != isTRUE(input$m1_regle)) updateCheckboxInput(session, "m1_regle", value = all3)
  }, ignoreNULL = FALSE)
  observeEvent(input$m1_regle, {
    if (isTRUE(input$m1_regle) && !setequal(input$m1_comp, c("c3","c30","c300")))
      updateCheckboxGroupInput(session, "m1_comp", selected = c("c3","c30","c300"))
    if (!isTRUE(input$m1_regle) && setequal(input$m1_comp, c("c3","c30","c300")))
      updateCheckboxGroupInput(session, "m1_comp", selected = character(0))
  })

  m1_code <- reactive({
    sel <- input$m1_comp
    if (isTRUE(input$m1_regle)) sel <- c("c3","c30","c300")
    sel_to_code(sel)
  })
  m1_lbl <- reactive({
    cd <- m1_code(); if (is.na(cd)) return("—")
    map <- c("3"=tt("comp3"),"30"=tt("comp30"),"300"=tt("comp300"),
             "3_30"=tt("inter_3_30"),"3_300"=tt("inter_3_300"),
             "30_300"=tt("inter_30_300"),"3_30_300"=tt("regle"))
    unname(map[cd])
  })

  output$m1_map_titre <- renderText(sprintf("%s — %s (%s)",
    if (lang() == "fr") "Carte" else "Map", m1_lbl(),
    c(bat=tt("couche_bat"),car=tt("couche_car"),iris=tt("couche_iris"))[input$m1_couche]))

  # Géométrie + valeurs bâtiments, filtrées par arrondissement : calculé une
  # fois, réutilisé par la carte, le panneau de détail et l'histogramme (qui
  # ignorait le filtre arrondissement jusqu'ici, incohérent avec la carte).
  m1_bat_data <- reactive({
    cd <- m1_code(); if (is.na(cd)) return(NULL)
    if (is.null(bat_geo) || is.null(bat_ind)) return(NULL)
    cc <- COL$bat_bin[[cd]]; if (is.na(cc %||% NA)) return(NULL)
    idg <- COL$bat_geoid; idb <- COL$bat_id
    g <- bat_geo
    if (length(arr_actifs()) < 16) {
      ci <- find_col(names(bat_ind), c("^CODE_IRIS$","^code_iris$"))
      arr_vec <- if (!is.na(ci)) {
        arr_of(bat_ind[[ci]][match(g[[idg]], bat_ind[[idb]])])
      } else if (!is.null(bat_arr_lookup)) {
        arr_of(bat_arr_lookup$CODE_IRIS[match(g[[idg]], bat_arr_lookup$ID_BAT)])
      } else NULL
      if (!is.null(arr_vec)) {
        keep <- arr_vec %in% arr_actifs(); keep[is.na(keep)] <- FALSE
        g <- g[keep, ]
      }
    }
    # leaflet/leafgl refusent les MULTIPOLYGON ("Can only handle POLYGONs,
    # please cast..."). st_cast peut dupliquer des lignes pour les rares
    # bâtiments réellement multi-parties (attributs répétés, comportement
    # normal) : val est donc recalculé après coup depuis le g final, jamais
    # suivi à travers le cast, pour rester juste quel que soit le nombre de
    # lignes obtenu.
    g <- tryCatch(sf::st_cast(g, "POLYGON", warn = FALSE), error = function(e) g)
    val <- bat_ind[[cc]][match(g[[idg]], bat_ind[[idb]])]
    # Les géométries vides survivent parfois au cast (bâtiments dégénérés de la
    # BD TOPO). Laissées en place, elles font échouer le calcul des bornes à la
    # sérialisation du widget, hors du tryCatch de rendu : c'est le "missing
    # value where TRUE/FALSE needed" observé sur une sélection d'un seul
    # arrondissement. On les écarte ici, en gardant val aligné.
    ok <- !sf::st_is_empty(g)
    if (any(!ok)) { g <- g[ok, ]; val <- val[ok] }
    val <- suppressWarnings(as.integer(round(as.numeric(val))))  # 0/1, NA conservé
    gid <- g[[idg]]   # identifiant bâtiment d'origine, avant duplication par le cast
    g$.id <- seq_len(nrow(g))
    list(g = g, val = val, gid = gid)
  })
  m1_clicked <- reactiveVal(NULL)

  output$m1_map <- renderLeaflet({
    cd <- m1_code(); validate(need(!is.na(cd), "Sélectionnez au moins une composante."))
    sel <- m1_clicked()
    if (input$m1_couche == "iris") {
      g <- iris_geo_f(); validate(need(!is.null(g), "Contour IRIS manquant"))
      cc <- COL$iris_rate[[cd]]; validate(need(!is.na(cc %||% NA), "Colonne taux IRIS introuvable (voir diagnostic)."))
      vals <- iris_mese[[cc]][match(g$.code, iris_mese$.code)]
      mult <- ifelse(max(vals, na.rm = TRUE) <= 1, 100, 1)
      vals <- vals * mult
      m <- leaflet(g) |> add_basemaps() |> add_fullscreen_btn() |>
        addPolygons(layerId = ~.code, fillColor = ~PAL_CONFORMITE(vals), fillOpacity = .8,
                    weight = .6, color = "#ffffff",
                    highlightOptions = highlightOptions(weight = 2, color = "#333333", bringToFront = TRUE),
                    label = ~sprintf("%s : %.0f%%", iris_nom_of(.code), vals)) |>
        addLegend(pal = PAL_CONFORMITE, values = c(0, 100), title = "Taux (%)",
                  labFormat = labelFormat(suffix = " %"))
      if (!is.null(sel)) {
        gg <- g[g$.code == sel, ]
        if (nrow(gg) > 0) m <- m |> addPolygons(data = gg, fill = FALSE, color = "#2962FF",
                                                weight = 4, opacity = 1, group = "sel")
      }
      m
    } else if (input$m1_couche == "car") {
      g <- car_f(); validate(need(!is.null(g), "Carreaux manquants"))
      cc <- COL$car_rate[[cd]]; validate(need(!is.na(cc %||% NA), "Colonne taux carreau introuvable (voir diagnostic)."))
      g$.id <- seq_len(nrow(g))  # identifiant de secours : aucun ID carreau confirmé, sert au clic
      vals <- g[[cc]]; mult <- ifelse(max(vals, na.rm = TRUE) <= 1, 100, 1)
      vals <- vals * mult
      m <- leaflet(g) |> add_basemaps() |> add_fullscreen_btn() |>
        addPolygons(layerId = ~.id, fillColor = ~PAL_CONFORMITE(vals), fillOpacity = .8, weight = .3, color = "#ffffff",
                    highlightOptions = highlightOptions(weight = 2, color = "#333333", bringToFront = TRUE),
                    label = ~sprintf("%.0f%%", vals)) |>
        addLegend(pal = PAL_CONFORMITE, values = c(0, 100), title = "Taux (%)",
                  labFormat = labelFormat(suffix = " %"))
      idx_sel <- suppressWarnings(as.integer(sel))
      if (length(idx_sel) == 1 && !is.na(idx_sel) && idx_sel >= 1 && idx_sel <= nrow(g)) {
        m <- m |> addPolygons(data = g[idx_sel, ], fill = FALSE, color = "#2962FF",
                              weight = 4, opacity = 1, group = "sel")
      }
      m
    } else {
      tryCatch({
        d <- m1_bat_data(); validate(need(!is.null(d), "Données bâtiment manquantes ou colonne introuvable (voir diagnostic)."))
        g <- d$g; val <- d$val
        validate(need(nrow(g) > 0L,
          if (lang() == "fr") "Aucun bâtiment dans la sélection." else "No buildings in this selection."))

        # Couleur pré-calculée en hexadécimal, NA -> gris. Un vecteur de
        # couleurs concret évite que colorFactor (SVG) comme leafgl (WebGL)
        # butent sur des NA dans les valeurs de conformité.
        fill_hex <- ifelse(is.na(val), "#cccccc", ifelse(val >= 1L, "#1a9641", "#d73027"))

        idx_sel <- suppressWarnings(as.integer(sel))
        sel_ok  <- length(idx_sel) == 1 && !is.na(idx_sel) && idx_sel >= 1 && idx_sel <= nrow(g)
        # Surlignage du bâtiment cliqué. En mode SVG (empreintes), on trace le
        # contour du polygone. En mode WebGL (centroïdes), on pose un marqueur
        # sur le centroïde déjà calculé, jamais un polygone re-dérivé : cela
        # évite qu'une géométrie source dégénérée fasse échouer la sérialisation
        # du widget (erreur "missing value where TRUE/FALSE needed" au clic).
        overlay_poly <- function(m) {
          if (!sel_ok) return(m)
          gsel <- g[idx_sel, ]
          if (nrow(gsel) == 0 || any(sf::st_is_empty(gsel))) return(m)
          m |> addPolygons(data = gsel, fill = FALSE, color = "#2962FF",
                           weight = 4, opacity = 1, group = "sel")
        }
        overlay_point <- function(m, cpts) {
          if (!sel_ok) return(m)
          cc <- sf::st_coordinates(cpts[match(idx_sel, cpts$.id), ])
          if (nrow(cc) == 0 || any(!is.finite(cc[1, ]))) return(m)
          m |> addCircleMarkers(lng = cc[1, 1], lat = cc[1, 2], radius = 9,
                                color = "#2962FF", weight = 3, opacity = 1,
                                fill = FALSE, group = "sel")
        }

        if (nrow(g) <= OPT_SVG_BATIMENTS) {
          # Volume modéré : rendu vectoriel classique, survol et clic bâtiment actifs.
          g$.fill <- fill_hex
          leaflet(g) |> add_basemaps() |> add_fullscreen_btn() |>
            addPolygons(layerId = ~.id, fillColor = ~.fill, fillOpacity = .9,
                        weight = .3, color = "#ffffff",
                        highlightOptions = highlightOptions(weight = 2, color = "#333333", bringToFront = TRUE)) |>
            overlay_poly()
        } else if (HAS_LEAFGL) {
          # Gros volume : rendu WebGL sur les centroïdes (un point par bâtiment).
          # Deux raisons d'éviter addGlPolygons : sa triangulation produit des
          # artefacts en étoile sur un gros lot, et en 0.2.4 il ignore une
          # couleur hexadécimale par entité (retombe sur le bleu par défaut).
          # addGlPoints n'a pas de triangulation et applique fill_hex par point.
          # Le clic reste servi via layerId (input$m1_map_glify_click$id).
          cpts <- sf::st_sf(.id = g$.id,
                            geometry = suppressWarnings(sf::st_centroid(sf::st_geometry(g))))
          leaflet() |> add_basemaps() |> add_fullscreen_btn() |>
            leafgl::addGlPoints(data = cpts, fillColor = fill_hex, radius = 4,
                                fillOpacity = .9, layerId = cpts$.id) |>
            overlay_point(cpts)
        } else {
          # leafgl indisponible : message propre plutôt que de figer le rendu SVG.
          validate(need(FALSE, sprintf(tt("bat_cap_msg"), format(nrow(g), big.mark = " "))))
        }
      }, error = function(e) {
        # validate()/need() lèvent une shiny.silent.error : on la laisse remonter
        # telle quelle, sinon le message de plafond ou d'absence de bâtiment
        # serait ré-emballé en dump de diagnostic illisible.
        if (inherits(e, "shiny.silent.error")) stop(e)
        d_diag <- tryCatch(m1_bat_data(), error = function(e2) NULL)
        diag <- paste0(
          "message = ", conditionMessage(e),
          " | nrow(g) = ", tryCatch(nrow(d_diag$g), error = function(e2) "NA"),
          " | class(val) = ", tryCatch(paste(class(d_diag$val), collapse=","), error = function(e2) "NA"),
          " | n_NA(val) = ", tryCatch(sum(is.na(d_diag$val)), error = function(e2) "NA"),
          " | leafgl = ", HAS_LEAFGL,
          " | class(sel) = ", paste(class(sel), collapse=","),
          " | length(sel) = ", length(sel),
          " | sel = ", if (is.null(sel)) "NULL" else paste(as.character(sel), collapse=",")
        )
        validate(paste0("Erreur carte bâtiments — diagnostic : ", diag))
      })
    }
  })

  # Panneau de détail au clic. Deux sources selon le moteur de rendu : couches
  # vectorielles (leaflet, *_shape_click) et couche bâtiments WebGL (leafgl,
  # *_glify_click). Les deux renvoient .id comme identifiant, traité à l'identique.
  observeEvent(input$m1_map_shape_click, m1_clicked(input$m1_map_shape_click$id))
  observeEvent(input$m1_map_glify_click, m1_clicked(input$m1_map_glify_click$id))
  # Un id de carreau/bâtiment (index de ligne) ou d'IRIS (CODE_IRIS) d'une
  # couche ne veut rien dire dans l'autre : on efface la sélection dès que la
  # couche ou le filtre d'arrondissement change, plutôt que de laisser un id
  # périmé se faire chercher au mauvais endroit.
  observeEvent(input$m1_couche, m1_clicked(NULL), ignoreInit = TRUE)
  observeEvent(input$arr, m1_clicked(NULL), ignoreInit = TRUE)

  output$m1_click <- renderUI({
    click_vide <- HTML(sprintf("<span style='color:#607d8b'>%s</span>", tt("click_empty")))
    codes4 <- c("3","30","300","3_30_300")
    noms <- c(tt("comp3"), tt("comp30"), tt("comp300"), tt("regle"))

    if (input$m1_couche == "bat") {
      raw <- m1_clicked(); d <- m1_bat_data()
      if (is.null(raw) || is.null(d)) return(click_vide)
      idx <- suppressWarnings(as.integer(raw))
      if (length(idx) != 1 || is.na(idx) || idx < 1 || idx > nrow(d$g)) return(click_vide)
      # Statut du bâtiment pour les trois composantes et la règle complète, lu
      # directement dans bat_ind via l'ID du bâtiment (pas seulement la
      # composante affichée sur la carte).
      gid  <- d$g[[COL$bat_geoid]][idx]
      irow <- match(gid, bat_ind[[COL$bat_id]])
      rows <- lapply(seq_along(codes4), function(k) {
        cc <- COL$bat_bin[[codes4[k]]]
        v  <- if (!is.na(cc %||% NA) && !is.na(irow)) bat_ind[[cc]][irow] else NA
        st <- if (is.na(v)) "—" else if (v == 1) tt("conforme") else tt("non_conforme")
        col <- if (is.na(v)) "#607d8b" else if (v == 1) "#1a9641" else "#d73027"
        sprintf("<tr><td>%s</td><td style='text-align:right;color:%s;font-weight:600'>%s</td></tr>",
                noms[k], col, st)
      })
      return(HTML(sprintf("<b>%s</b><table class='table table-sm'>%s</table>",
                          sprintf(tt("bat_titre"), idx), paste(rows, collapse = ""))))
    }

    if (input$m1_couche == "car") {
      raw <- m1_clicked(); g <- car_f()
      if (is.null(raw) || is.null(g)) return(click_vide)
      idx <- suppressWarnings(as.integer(raw))
      if (length(idx) != 1 || is.na(idx) || idx < 1 || idx > nrow(g)) return(click_vide)
      rows <- lapply(seq_along(codes4), function(k) {
        cc <- COL$car_rate[[codes4[k]]]
        v <- if (!is.na(cc %||% NA)) g[[cc]][idx] else NA
        mult <- if (!is.na(v) && v <= 1) 100 else 1
        sprintf("<tr><td>%s</td><td style='text-align:right'>%s</td></tr>", noms[k],
                if (is.na(v)) "—" else sprintf("%.0f%%", v*mult))
      })
      titre <- if (!is.na(COL$car_id %||% NA)) as.character(g[[COL$car_id]][idx]) else sprintf(tt("carreau_titre"), idx)
      return(HTML(paste0("<b>", titre, "</b><table class='table table-sm'>", paste(rows, collapse=""), "</table>")))
    }

    id <- m1_clicked()
    if (is.null(id)) return(click_vide)
    rows <- lapply(seq_along(codes4), function(i) {
      cc <- COL$iris_rate[[codes4[i]]]
      v <- if (!is.na(cc %||% NA)) iris_mese[[cc]][match(id, iris_mese$.code)] else NA
      mult <- if (!is.na(v) && v <= 1) 100 else 1
      sprintf("<tr><td>%s</td><td style='text-align:right'>%s</td></tr>", noms[i],
              if (is.na(v)) "—" else sprintf("%.0f%%", v*mult))
    })
    HTML(paste0("<b>", iris_nom_of(id), "</b><table class='table table-sm'>", paste(rows, collapse=""), "</table>"))
  })

  output$m1_distrib <- renderPlotly({
    cd <- m1_code(); validate(need(!is.na(cd), " "))
    if (input$m1_couche == "bat") {
      d <- m1_bat_data(); validate(need(!is.null(d), "Colonne binaire bâtiment introuvable (voir diagnostic)."))
      # Un bâtiment MULTIPOLYGON à plusieurs parties est scindé en plusieurs
      # lignes de rendu par st_cast() dans m1_bat_data(), pour que chaque partie
      # s'affiche sur la carte. Compter les lignes compterait ce bâtiment deux
      # fois (256 408 au lieu de 256 407) : on ne garde qu'une occurrence par
      # identifiant bâtiment (gid) pour les statistiques.
      v <- d$val[!duplicated(d$gid)]
      df <- data.frame(statut = c(tt("non_conforme"), tt("conforme")),
                       n = c(sum(v == 0, na.rm=TRUE), sum(v == 1, na.rm=TRUE)))
      plot_ly(df, x = ~statut, y = ~n, type = "bar",
              marker = list(color = c("#d73027","#1a9641"))) |>
        layout(yaxis = list(title = tt("batiments")), xaxis = list(title = ""))
    } else {
      dat <- if (input$m1_couche == "car") car_f() else iris_f()
      cc <- if (input$m1_couche == "car") COL$car_rate[[cd]] else COL$iris_rate[[cd]]
      validate(need(!is.null(dat) && !is.na(cc %||% NA), "Taux introuvable."))
      v <- dat[[cc]]; if (max(v, na.rm=TRUE) <= 1) v <- v*100
      p_hist <- plot_ly(x = ~v, type = "histogram", marker = list(color = "#1D9E75"), showlegend = FALSE) |>
        layout(xaxis = list(title = ""), yaxis = list(title = tt("effectif")))
      p_box <- plot_ly(x = ~v, type = "box", boxpoints = FALSE,
                       fillcolor = "rgba(29,158,117,.35)", line = list(color = "#1D9E75"),
                       showlegend = FALSE) |>
        layout(xaxis = list(title = tt("tx_conformite")), yaxis = list(showticklabels = FALSE))
      plotly::subplot(p_hist, p_box, nrows = 2, heights = c(0.72, 0.28),
                      shareX = TRUE, titleX = TRUE, titleY = TRUE) |>
        layout(showlegend = FALSE)
    }
  })

  # Tableau taux de respect + effectifs, cohérent avec la couche sélectionnée.
  # Bâtiments : conformes / non conformes en nombre et en part. Carreaux et IRIS
  # portent un taux par zone : on résume alors en nombre de zones et taux moyen.
  output$m1_conf_table <- renderUI({
    cd <- m1_code()
    vide <- HTML(sprintf("<span style='color:#607d8b'>%s</span>", tt("click_empty")))
    if (is.na(cd)) return(vide)
    lbl_statut <- if (lang() == "fr") "Statut" else "Status"
    lbl_n      <- tt("batiments")
    lbl_pct    <- if (lang() == "fr") "Part" else "Share"

    if (input$m1_couche == "bat") {
      d <- m1_bat_data(); if (is.null(d)) return(vide)
      # Même dédoublonnage que le graphique en barres : un bâtiment scindé en
      # plusieurs lignes de rendu (MULTIPOLYGON multi-parties) ne doit être
      # compté qu'une seule fois.
      v <- d$val[!duplicated(d$gid)]; tot <- sum(!is.na(v))
      if (tot == 0) return(vide)
      nc <- sum(v == 1, na.rm = TRUE); nn <- sum(v == 0, na.rm = TRUE)
      f  <- function(n) sprintf("%s", format(n, big.mark = " "))
      p  <- function(n) sprintf("%.1f%%", 100 * n / tot)
      rows <- paste0(
        sprintf("<tr><td style='color:#1a9641;font-weight:600'>%s</td><td style='text-align:right'>%s</td><td style='text-align:right'>%s</td></tr>",
                tt("conforme"), f(nc), p(nc)),
        sprintf("<tr><td style='color:#d73027;font-weight:600'>%s</td><td style='text-align:right'>%s</td><td style='text-align:right'>%s</td></tr>",
                tt("non_conforme"), f(nn), p(nn)),
        sprintf("<tr style='border-top:2px solid #ddd'><td><b>%s</b></td><td style='text-align:right'><b>%s</b></td><td style='text-align:right'>100%%</td></tr>",
                if (lang()=="fr") "Total" else "Total", f(tot)))
      return(HTML(sprintf(
        "<table class='table table-sm'><thead><tr><th>%s</th><th style='text-align:right'>%s</th><th style='text-align:right'>%s</th></tr></thead><tbody>%s</tbody></table>",
        lbl_statut, lbl_n, lbl_pct, rows)))
    }

    # Carreaux / IRIS : taux de conformité par zone.
    dat <- if (input$m1_couche == "car") car_f() else iris_f()
    cc  <- if (input$m1_couche == "car") COL$car_rate[[cd]] else COL$iris_rate[[cd]]
    if (is.null(dat) || is.na(cc %||% NA)) return(vide)
    v <- dat[[cc]]; v <- v[!is.na(v)]; if (length(v) == 0) return(vide)
    if (max(v) <= 1) v <- v * 100
    lbl_zone <- if (input$m1_couche == "car")
      (if (lang()=="fr") "carreaux" else "grid cells") else "IRIS"
    HTML(sprintf(
      "<table class='table table-sm'><tr><td>%s</td><td style='text-align:right'><b>%s</b></td></tr><tr><td>%s</td><td style='text-align:right'><b>%.1f%%</b></td></tr><tr><td>%s</td><td style='text-align:right'><b>%.1f%%</b></td></tr></table>",
      sprintf(if (lang()=="fr") "Nombre de %s" else "Number of %s", lbl_zone), format(length(v), big.mark = " "),
      if (lang()=="fr") "Taux moyen" else "Mean rate", mean(v),
      if (lang()=="fr") "Taux médian" else "Median rate", median(v)))
  })

  # Panneau parcs (Composante 300 active)
  output$m1_parcs_card <- renderUI({
    c300_actif <- "c300" %in% input$m1_comp || isTRUE(input$m1_regle)
    if (!c300_actif) return(NULL)
    card(card_header(if (lang() == "fr") "Statistiques parcs (Composante 300)" else "Park statistics (Component 300)"), uiOutput("m1_parcs"))
  })
  output$m1_parcs <- renderUI({
    if (is.null(regle300_bat)) return(HTML(sprintf("<span style='color:#607d8b'>%s</span>",
      if (lang()=="fr") "Table bâtiment x espace vert absente (bat_parcs.csv)." else "Building-to-green-space table missing (bat_parcs.csv).")))

    d <- regle300_bat
    # Filtre arrondissement, cohérent avec le reste du module 1 : même
    # jointure ID bâtiment -> CODE_IRIS -> arrondissement que m1_bat_data().
    if (length(arr_actifs()) < 16 && !is.null(bat_arr_lookup)) {
      arr_vec <- arr_of(bat_arr_lookup$CODE_IRIS[match(d$ID, bat_arr_lookup$ID_BAT)])
      keep <- arr_vec %in% arr_actifs(); keep[is.na(keep)] <- FALSE
      d <- d[keep, ]
    }

    served <- d[!is.na(d$nom_espace_retenu), ]
    n_spaces     <- length(unique(served$nom_espace_retenu))
    n_bat_served <- nrow(served)
    moy <- if (n_spaces > 0) round(n_bat_served / n_spaces) else NA

    lbl1 <- if (lang()=="fr") "espaces desservants" else "serving green spaces"
    lbl2 <- if (lang()=="fr") "bât./espace en moyenne" else "avg. buildings/space"

    # Répartition par catégorie (parcs / Calanques / Nerthe / réserves), lue
    # dans label_espace_retenu : quatre valeurs fixes correspondant aux
    # familles de colonnes regle_300_parcs / _calanques / _nerthe / _reserves.
    cats     <- c("parcs","calanques","nerthe","reserves")
    cats_lbl <- if (lang()=="fr")
      c(parcs="Parcs", calanques="Calanques", nerthe="Nerthe", reserves="Réserves")
    else
      c(parcs="Parks", calanques="Calanques", nerthe="Nerthe", reserves="Reserves")
    tab  <- table(factor(served$label_espace_retenu, levels = cats))
    rows <- paste(sprintf("<tr><td>%s</td><td style='text-align:right'>%s</td></tr>",
                          cats_lbl[cats], format(as.integer(tab), big.mark = " ")),
                 collapse = "")

    HTML(sprintf(paste0(
      "<div style='display:flex;gap:18px;margin-bottom:10px'>",
      "<span><b>%d</b> %s</span><span><b>%s</b> %s</span></div>",
      "<table class='table table-sm'><thead><tr><th>%s</th>",
      "<th style='text-align:right'>%s</th></tr></thead><tbody>%s</tbody></table>"),
      n_spaces, lbl1, if (is.na(moy)) "—" else format(moy, big.mark = " "), lbl2,
      if (lang()=="fr") "Catégorie" else "Category",
      if (lang()=="fr") "Bâtiments desservis" else "Buildings served",
      rows))
  })

  # ===========================================================================
  # MODULE 2 — BIVARIÉ
  # ===========================================================================
  m2_varcol <- reactive({
    if (input$m2_var == "lst") {
      cle <- input$m2_lst %||% "lst_moy_2021"
      COL$mese[[cle]] %||% find_col(names(iris_mese), paste0("^lst_max_med_", sub("lst_max_", "", cle), "$"))
    } else COL$mese[[input$m2_var]]
  })
  m2_vegcol <- reactive(COL$iris_rate[[input$m2_veg]])

  m2_data <- reactive({
    validate(need(!is.null(iris_mese), "MESE IRIS manquante."))
    vc <- m2_vegcol(); mc <- m2_varcol()
    validate(need(!is.na(vc %||% NA), "Colonne végétation introuvable (voir diagnostic)."),
             need(!is.na(mc %||% NA), "Colonne variable croisée introuvable (voir diagnostic)."))
    d <- data.frame(code = iris_mese$.code, arr = iris_mese$.arr,
                    veg = iris_mese[[vc]], var = iris_mese[[mc]])
    # bornes de quartile fixées sur les 393 IRIS si option activée
    bv <- if (OPT_QUARTILES_FIXES) quantile(d$veg, c(0,.25,.5,.75,1), na.rm=TRUE) else NULL
    bm <- if (OPT_QUARTILES_FIXES) quantile(d$var, c(0,.25,.5,.75,1), na.rm=TRUE) else NULL
    if (length(arr_actifs()) < 16) d <- d[d$arr %in% arr_actifs(), ]
    d$qv <- quartile(d$veg, bv); d$qm <- quartile(d$var, bm)
    d$col <- biv_class(d$qv, d$qm)
    d
  })

  m2_clicked <- reactiveVal(NULL)
  observeEvent(input$m2_map_shape_click, m2_clicked(input$m2_map_shape_click$id))
  observeEvent(input$arr, m2_clicked(NULL), ignoreInit = TRUE)

  output$m2_map <- renderLeaflet({
    g <- iris_geo_f(); validate(need(!is.null(g), "Contour IRIS manquant"))
    d <- m2_data()
    g$col <- d$col[match(g$.code, d$code)]
    m <- leaflet(g) |> add_basemaps() |> add_fullscreen_btn() |>
      addPolygons(layerId = ~.code, fillColor = ~ifelse(is.na(col), "#eeeeee", col), fillOpacity = .85,
                  weight = .5, color = "#ffffff",
                  highlightOptions = highlightOptions(weight = 2, color = "#333333", bringToFront = TRUE),
                  label = ~iris_nom_of(.code))
    sel <- m2_clicked()
    if (!is.null(sel)) {
      gg <- g[g$.code == sel, ]
      if (nrow(gg) > 0) m <- m |> addPolygons(data = gg, fill = FALSE, color = "#2962FF",
                                              weight = 4, opacity = 1, group = "sel")
    }
    m
  })

  output$m2_click <- renderUI({
    id <- m2_clicked()
    click_vide <- HTML(sprintf("<span style='color:#607d8b'>%s</span>", tt("click_empty_iris")))
    if (is.null(id)) return(click_vide)
    d <- m2_data(); row <- d[d$code == id, ]
    if (nrow(row) == 0) return(click_vide)
    veg_lbl <- c("3"=tt("comp3"),"30"=tt("comp30"),"300"=tt("comp300"),
                "3_30_300"=tt("regle"))[input$m2_veg]
    var_lbl <- c(edi=tt("var_edi"), lcz_bur=tt("var_lcz"),
                icair365=tt("var_icair"), lst=tt("var_lst"))[input$m2_var]
    veg_mult <- if (!is.na(row$veg) && row$veg <= 1) 100 else 1
    veg_txt <- if (is.na(row$veg)) "—" else sprintf("%.1f %%", row$veg * veg_mult)
    var_txt <- if (is.na(row$var)) "—"
               else if (input$m2_var == "lst") sprintf("%.1f °C", row$var)
               else if (input$m2_var == "lcz_bur") sprintf("%.1f %%", row$var)
               else sprintf("%.2f", row$var)
    HTML(sprintf(
      "<b>%s</b><table class='table table-sm'><tr><td>%s</td><td style='text-align:right'>%s</td></tr><tr><td>%s</td><td style='text-align:right'>%s</td></tr></table>",
      iris_nom_of(id), veg_lbl, veg_txt, var_lbl, var_txt))
  })

  output$m2_scatter <- renderPlotly({
    d <- m2_data()
    d <- d[is.finite(d$veg) & is.finite(d$var), ]
    validate(need(nrow(d) >= 3, "Pas assez de valeurs valides pour tracer le nuage de points."))
    # Droite de régression calculée à part, sur les mêmes lignes déjà
    # nettoyées : évite un appariement x/y de longueurs différentes, qui
    # produisait une erreur de sérialisation ("[object Object]") côté plotly.
    fit <- stats::lm(var ~ veg, data = d)
    ord <- order(d$veg)
    reg_lbl <- if (lang() == "fr") "Régression" else "Regression"
    p <- plot_ly() |>
      add_markers(x = d$veg, y = d$var, marker = list(color = "#378ADD", opacity = .6),
                  name = "IRIS", hoverinfo = "text",
                  text = paste0(iris_nom_of(d$code), "<br>", round(d$veg, 3), " / ", round(d$var, 3))) |>
      add_lines(x = d$veg[ord], y = stats::fitted(fit)[ord],
                line = list(color = "#d73027", width = 2), name = reg_lbl)
    p |> layout(xaxis = list(title = tt("ind_veg")),
                yaxis = list(title = tt("var_croisee")), showlegend = FALSE)
  })

  output$m2_corr <- renderUI({
    d <- m2_data()
    ct <- suppressWarnings(cor.test(d$veg, d$var, method = "spearman"))
    rho_lbl <- if (lang() == "fr") "ρ de Spearman" else "Spearman's ρ"
    HTML(sprintf("<div style='font-size:15px'>%s = <b>%.3f</b><br>p = %s · N = %d IRIS</div>",
                 rho_lbl, unname(ct$estimate), format.pval(ct$p.value, digits = 2), nrow(d)))
  })

  # ===========================================================================
  # MODULE 3 — PROFILS
  # ===========================================================================
  m3_sel <- reactiveVal(character(0))
  observeEvent(input$m3_map_shape_click, {
    id <- input$m3_map_shape_click$id
    cur <- m3_sel(); m3_sel(if (id %in% cur) setdiff(cur, id) else c(cur, id))
  })
  observeEvent(input$m3_reset, m3_sel(character(0)))

  output$m3_legende <- renderUI({
    HTML(paste0("<b>", tt("m3_profils_title"), "</b><br>", paste(sprintf(
      "<span style='display:inline-block;width:12px;height:12px;background:%s;border-radius:2px'></span> %s",
      PROFIL_COULEURS, profil_lbl()), collapse = "<br>")))
  })

  output$m3_map <- renderLeaflet({
    g <- iris_geo_f(); validate(need(!is.null(g), "Contour IRIS manquant"),
                                need(isTRUE(PROFIL_OK), paste0("Profil indisponible : ", PROFIL_STATUT)))
    prof <- iris_mese[[COL$iris_prof]][match(g$.code, iris_mese$.code)]
    g$col <- PROFIL_COULEURS[as.character(prof)]
    pl <- profil_lbl()
    m <- leaflet(g) |> add_basemaps() |> add_fullscreen_btn() |>
      addPolygons(layerId = ~.code, fillColor = ~ifelse(is.na(col), "#eeeeee", col),
                  fillOpacity = .8, weight = .5, color = "#ffffff",
                  highlightOptions = highlightOptions(weight = 2, color = "#333333", bringToFront = TRUE),
                  label = ~sprintf("%s — %s", iris_nom_of(.code), pl[as.character(prof)]))
    sel <- m3_sel()
    if (length(sel) > 0) {
      gg <- g[g$.code %in% sel, ]
      if (nrow(gg) > 0) m <- m |> addPolygons(data = gg, fill = FALSE, color = "#2962FF",
                                              weight = 4, opacity = 1, group = "sel")
    }
    m
  })

  # Variables du radar / tableau
  m3_vars <- reactive({
    v <- list()
    v[[tt("comp3")]] <- COL$iris_rate[["3"]]
    v[[tt("comp30")]] <- COL$iris_rate[["30"]]
    v[[tt("comp300")]] <- COL$iris_rate[["300"]]
    v[[tt("regle")]] <- COL$iris_rate[["3_30_300"]]
    v[["EDI"]] <- COL$mese$edi
    v[["ICAIR365"]] <- COL$mese$icair365
    for (l in OPT_LST_RADAR) v[[toupper(gsub("_"," ",l))]] <- COL$mese[[l]] %||%
      find_col(names(iris_mese), gsub("_",".*",l))
    v[!vapply(v, function(x) is.na(x %||% NA), logical(1))]
  })
  m3_pct_vars <- function() c(tt("comp3"), tt("comp30"), tt("comp300"), tt("regle"))

  output$m3_radar <- renderPlotly({
    sel <- m3_sel(); validate(need(length(sel) > 0, tt("click_empty_m3")))
    vars <- m3_vars(); validate(need(length(vars) >= 3, "Trop peu de variables résolues pour un radar."))
    # normalisation min-max sur les 393 IRIS
    norm <- lapply(vars, function(cc) {
      x <- iris_mese[[cc]]; rng <- range(x, na.rm = TRUE)
      function(v) if (diff(rng) == 0) 0.5 else (v - rng[1])/diff(rng)
    })
    p <- plot_ly(type = "scatterpolar", mode = "lines", fill = "toself")
    for (id in sel) {
      i <- match(id, iris_mese$.code)
      vals <- vapply(seq_along(vars), function(k) norm[[k]](iris_mese[[vars[[k]]]][i]), numeric(1))
      p <- add_trace(p, r = c(vals, vals[1]), theta = c(names(vars), names(vars)[1]),
                     name = iris_nom_of(id))
    }
    p |> layout(polar = list(radialaxis = list(range = c(0,1), showticklabels = FALSE)))
  })

  output$m3_table <- renderDT({
    sel <- m3_sel(); validate(need(length(sel) > 0, if (lang()=="fr") "Aucun IRIS sélectionné." else "No IRIS selected."))
    vars <- m3_vars()
    idx <- match(sel, iris_mese$.code)
    pl <- profil_lbl()
    prof <- if (isTRUE(PROFIL_OK)) pl[as.character(iris_mese[[COL$iris_prof]][idx])] else rep("—", length(idx))
    pctv <- m3_pct_vars()
    # Composante 3/30/300 et Règle 3-30-300 en %, tout le reste en valeur
    # brute ; arrondi à 1 décimale sur l'ensemble du tableau (demande K :
    # "plus épuré, plus de maîtrise"). Exception : EDI est affiché en
    # quintile local Marseille (Q1-Q5, cf. edi_Q_mrs ci-dessus), plus
    # évocateur qu'une valeur brute dans un tableau (demande K) — seul le
    # TABLEAU change ; le radar garde le score continu pour sa normalisation.
    body <- lapply(names(vars), function(nm) {
      if (nm == "EDI" && !is.na(COL$mese$edi_Q_mrs %||% NA)) {
        q <- iris_mese[[COL$mese$edi_Q_mrs]][idx]
        ifelse(is.na(q), "—", sprintf("Q%d/5", q))
      } else {
        x <- iris_mese[[vars[[nm]]]][idx]
        if (nm %in% pctv) {
          mult <- ifelse(is.na(x) | x > 1, 1, 100)
          ifelse(is.na(x), "—", sprintf("%.1f%%", round(x * mult, 1)))
        } else {
          ifelse(is.na(x), "—", sprintf("%.1f", round(x, 1)))
        }
      }
    })
    tab <- as.data.frame(do.call(rbind, body)); colnames(tab) <- iris_nom_of(sel)
    tab <- rbind(Profil = prof, tab)
    lbl_var <- names(vars)
    # Libellé de ligne clarifié pour le tableau uniquement (le radar garde
    # "EDI" tel quel comme nom d'axe, puisqu'il affiche le score continu).
    lbl_var[lbl_var == "EDI"] <- if (lang()=="fr") "EDI (quintile Marseille)" else "EDI (Marseille quintile)"
    tab <- cbind(Variable = c(tt("profil_col"), lbl_var), tab)
    names(tab)[1] <- tt("variable_col")
    datatable(tab, rownames = FALSE, options = list(dom = "t", pageLength = 20,
      columnDefs = list(list(className = "dt-right", targets = 1:(ncol(tab)-1)))))
  })
}

# www/ (aux côtés d'app.R) est servi automatiquement par Shiny — plus besoin
# d'addResourcePath depuis que les logos y sont rangés (voir logo_tag()).

shinyApp(ui, server)
