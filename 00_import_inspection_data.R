# ==============================================================================
#  00_import_inspection_data.R
#  Import correct de tous les fichiers de dashboard/data + inspection de
#  structure maximale, pensée pour être copiée-collée à Claude tel quel.
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)   # read_csv
  library(dplyr)   # glimpse
  library(sf)      # st_read et fonctions spatiales
})

base_path <- path.expand("~/Desktop/Stage_2026/dashboard/data")

# --- Nettoyage du fichier journal temporaire (jamais à importer) ------------
journal_file <- file.path(base_path, "reference/iris_boundaries.gpkg-journal")
if (file.exists(journal_file)) {
  file.remove(journal_file)
  message("Fichier journal temporaire supprimé : ", journal_file)
}

# ==============================================================================
# 1. LECTURE ROBUSTE : ne bloque jamais tout le script pour un fichier absent
# ==============================================================================

lire_csv_safe <- function(chemin) {
  if (!file.exists(chemin)) { message("  MANQUANT (csv) : ", chemin); return(NULL) }
  tryCatch(read_csv(chemin, show_col_types = FALSE),
           error = function(e) { message("  ERREUR lecture ", chemin, " : ", conditionMessage(e)); NULL })
}

lire_gpkg_safe <- function(chemin) {
  if (!file.exists(chemin)) { message("  MANQUANT (gpkg) : ", chemin); return(NULL) }
  tryCatch(st_read(chemin, quiet = TRUE),
           error = function(e) { message("  ERREUR lecture ", chemin, " : ", conditionMessage(e)); NULL })
}

message("== Import des fichiers CSV ==")
manifest                    <- lire_csv_safe(file.path(base_path, "00_MANIFEST.csv"))
batiments_concordance_brute <- lire_csv_safe(file.path(base_path, "batiments/batiments_concordance_brute.csv"))
batiments_indicateurs       <- lire_csv_safe(file.path(base_path, "batiments/batiments_indicateurs.csv"))
iris_lst_max_2021_2025      <- lire_csv_safe(file.path(base_path, "iris/iris_lst_max_2021_2025.csv"))
iris_veg_seule              <- lire_csv_safe(file.path(base_path, "iris/iris_veg_seule.csv"))

# -- CSV du dossier profils_vulnerabilite --
afm_contributions_groupes   <- lire_csv_safe(file.path(base_path, "profils_vulnerabilite/AFM_contributions_groupes.csv"))
afm_coordonnees_groupes     <- lire_csv_safe(file.path(base_path, "profils_vulnerabilite/AFM_coordonnees_groupes.csv"))
afm_variance_expliquee      <- lire_csv_safe(file.path(base_path, "profils_vulnerabilite/AFM_variance_expliquee.csv"))
croise_profil_cumul_deficits<- lire_csv_safe(file.path(base_path, "profils_vulnerabilite/croise_profil_x_cumul_deficits.csv"))
cumul_deficits_par_iris     <- lire_csv_safe(file.path(base_path, "profils_vulnerabilite/cumul_deficits_par_IRIS.csv"))
distribution_profils        <- lire_csv_safe(file.path(base_path, "profils_vulnerabilite/distribution_profils.csv"))
hcpc_desc_var_par_profil    <- lire_csv_safe(file.path(base_path, "profils_vulnerabilite/HCPC_desc_var_par_profil.csv"))
poids_groupes_par_profil    <- lire_csv_safe(file.path(base_path, "profils_vulnerabilite/poids_groupes_par_profil.csv"))
profil_moyen_indicateurs    <- lire_csv_safe(file.path(base_path, "profils_vulnerabilite/profil_moyen_indicateurs.csv"))

message("\n== Import des fichiers GeoPackage (.gpkg) ==")
arrondissement_stats        <- lire_gpkg_safe(file.path(base_path, "arrondissement/arrondissement_stats.gpkg"))
batiments_geometrie         <- lire_gpkg_safe(file.path(base_path, "batiments/batiments_geometrie.gpkg"))
carreaux_200m               <- lire_gpkg_safe(file.path(base_path, "carreaux/carreaux_200m.gpkg"))
iris_icvse                  <- lire_gpkg_safe(file.path(base_path, "iris/iris_icvse.gpkg"))
iris_mese                   <- lire_gpkg_safe(file.path(base_path, "iris/iris_mese.gpkg"))
arrondissements             <- lire_gpkg_safe(file.path(base_path, "reference/arrondissements.gpkg"))
carreaux_boundaries         <- lire_gpkg_safe(file.path(base_path, "reference/carreaux_boundaries.gpkg"))
iris_boundaries             <- lire_gpkg_safe(file.path(base_path, "reference/iris_boundaries.gpkg"))
parcs_espaces_verts         <- lire_gpkg_safe(file.path(base_path, "reference/parcs_espaces_verts.gpkg"))

# -- GeoPackage du dossier profils_vulnerabilite --
profils_vulnerabilite_iris  <- lire_gpkg_safe(file.path(base_path, "profils_vulnerabilite/PROFILS_VULNERABILITE_IRIS.gpkg"))

# ==============================================================================
# 2. INSPECTION — une seule fonction, sûre sur le spatial, maximale sur le reste
# ==============================================================================

inspecter <- function(nom, x) {
  cat("\n==================================================\n")
  cat("TABLE :", nom, "\n")
  cat("==================================================\n")
  if (is.null(x)) { cat("  (non chargée — voir message d'import ci-dessus)\n"); return(invisible(NULL)) }
  
  if (inherits(x, "sf")) {
    # --- Résumé spatial : jamais de déroulé de coordonnées ---
    cat("Objet spatial (sf) —", nrow(x), "entités,", ncol(x) - 1, "attributs (+ géométrie)\n")
    cat("CRS          :", sf::st_crs(x)$input %||% "indéterminé", "\n")
    cat("Type géom.   :", paste(unique(as.character(sf::st_geometry_type(x))), collapse = ", "), "\n")
    bb <- sf::st_bbox(x)
    cat("Emprise      : xmin=", round(bb["xmin"],2), " ymin=", round(bb["ymin"],2),
        " xmax=", round(bb["xmax"],2), " ymax=", round(bb["ymax"],2), "\n", sep = "")
    cat("\n-- glimpse() des attributs (géométrie retirée) --\n")
    dplyr::glimpse(sf::st_drop_geometry(x))
    cat("\n-- str() des attributs (géométrie retirée) --\n")
    str(sf::st_drop_geometry(x))
  } else {
    # --- Table plate : str() et glimpse() sont tous les deux sûrs et rapides ---
    cat("Table —", nrow(x), "lignes,", ncol(x), "colonnes\n")
    cat("\n-- glimpse() --\n")
    dplyr::glimpse(x)
    cat("\n-- str() --\n")
    str(x)
  }
  invisible(NULL)
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

data_list <- list(
  # --- Racine & Batiments & IRIS ---
  "00_MANIFEST"                  = manifest,
  "arrondissement_stats"         = arrondissement_stats,
  "batiments_concordance_brute"  = batiments_concordance_brute,
  "batiments_geometrie"          = batiments_geometrie,
  "batiments_indicateurs"        = batiments_indicateurs,
  "carreaux_200m"                = carreaux_200m,
  "iris_icvse"                   = iris_icvse,
  "iris_lst_max_2021_2025"       = iris_lst_max_2021_2025,
  "iris_mese"                    = iris_mese,
  "iris_veg_seule"               = iris_veg_seule,
  "arrondissements"              = arrondissements,
  "carreaux_boundaries"         = carreaux_boundaries,
  "iris_boundaries"             = iris_boundaries,
  "parcs_espaces_verts"          = parcs_espaces_verts,
  
  # --- Profils de vulnérabilité ---
  "profils_vulnerabilite_iris"   = profils_vulnerabilite_iris,
  "afm_contributions_groupes"    = afm_contributions_groupes,
  "afm_coordonnees_groupes"      = afm_coordonnees_groupes,
  "afm_variance_expliquee"       = afm_variance_expliquee,
  "croise_profil_cumul_deficits" = croise_profil_cumul_deficits,
  "cumul_deficits_par_iris"      = cumul_deficits_par_iris,
  "distribution_profils"         = distribution_profils,
  "hcpc_desc_var_par_profil"     = hcpc_desc_var_par_profil,
  "poids_groupes_par_profil"     = poids_groupes_par_profil,
  "profil_moyen_indicateurs"     = profil_moyen_indicateurs
)

for (nom in names(data_list)) inspecter(nom, data_list[[nom]])

# --- Récapitulatif final ----------------------------------------------------
cat("\n==================================================\n")
cat("RÉCAPITULATIF\n")
cat("==================================================\n")
charge  <- names(data_list)[!vapply(data_list, is.null, logical(1))]
manque  <- names(data_list)[vapply(data_list, is.null, logical(1))]
cat("Chargées (", length(charge), "/", length(data_list), ") : ",
    paste(charge, collapse = ", "), "\n", sep = "")
if (length(manque)) cat("MANQUANTES : ", paste(manque, collapse = ", "), "\n", sep = "")

