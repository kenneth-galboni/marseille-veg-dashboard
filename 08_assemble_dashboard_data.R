# ==============================================================================
#  08_assemble_dashboard_data.R
#  RASSEMBLEMENT DES DONNÉES POUR LE DASHBOARD SIG DÉCIDEURS
#  Marseille | Règle 3-30-300 & Vulnérabilités socio-environnementales
#  SESSTIM 2026
#
#  RÔLE :
#    Réunit dans un unique dossier "data/", à plat et dans des formats
#    universels (GeoPackage + CSV), tout ce qui est nécessaire à un dashboard
#    aux 4 échelles (bâtiment, carreau 200m, IRIS, arrondissement), sans
#    dépendre du choix de plateforme (R/Shiny ou Python/Django).
#
#    Lecture seule des sources — aucune modification du feature store ni des
#    R/Outputs existants. Ce script ne fait que copier/agréger/exporter.
#
#  À LANCER SUR TA MACHINE (pas dans l'environnement Claude) :
#    le feature store et les R/Outputs vivent sous ~/Desktop/Stage_2026/,
#    inaccessibles depuis la conversation. Une partie de "data/" (tables déjà
#    validées du mémoire) t'a déjà été livrée à part ; ce script rapatrie le
#    reste : couches spatiales, VEG_BATIMENTS, VEG_CARREAUX_200M, MESE_IRIS_v4,
#    ICVSE_IRIS_complet, desserte des parcs (panneau Composante 300).
#
#  CHEMINS INCERTAINS (voir DATA_INVENTORY.md §6) :
#    Plusieurs conventions de chemins coexistent dans le projet (racine avec/
#    sans "LiDARD_HD_h7m", dossier ICVSE numéroté 05 ou 06, orthographe des
#    parcs, dossier de sortie de la Composante 300 sfnetworks vs étendue).
#    Ce script essaie systématiquement plusieurs chemins candidats par ordre
#    de préférence et journalise clairement ce qu'il trouve — rien n'échoue
#    silencieusement, rien ne plante le script en cours de route.
#
#  USAGE :
#    Rscript 08_assemble_dashboard_data.R
#    — ou —
#    source("08_assemble_dashboard_data.R")
#
#  AUTEUR : GALBONI Kenneth / QUANTIM — UMR 1252 SESSTIM (AMU, INSERM, IRD)
# ==============================================================================


# ==============================================================================
# BLOC 0 — PARAMÈTRES (seul bloc à modifier)
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(glue)
  library(tibble)
})

# Dossier de sortie unique, à la racine du projet de dashboard. Adapter si
# besoin — tout le reste du script est relatif à DATA_OUT.
DATA_OUT <- path.expand("~/Desktop/Stage_2026/dashboard/data")

# Racines candidates du feature store, essayées dans l'ordre (cf. §6 de
# DATA_INVENTORY.md — deux conventions coexistent dans le projet).
CANDIDATS_BASE <- path.expand(c(
  "~/Desktop/Stage_2026/LiDARD_HD_h7m",   # convention scripts 01/02/04/07 (_v12, la plus récente)
  "~/Desktop/Stage_2026"                    # convention app.R / 04_CONTEXTE_DASHBOARDS.md (plus ancienne)
))

# Sous-dossiers R/Outputs candidats pour l'ICVSE (numérotation 05 ou 06 selon
# la source documentaire — cf. §6).
CANDIDATS_ICVSE_SOUSDOSSIER <- c("06_icvse", "05_icvse")

# Sous-dossiers R/Outputs candidats pour la concordance et le bivarié
# (numérotation historique 03/04 vs actuelle 04/05 — cf. §6).
CANDIDATS_CONCORDANCE_SOUSDOSSIER <- c("04_concordance", "03_concordance")
CANDIDATS_BIVARIEE_SOUSDOSSIER    <- c("05_bivariee_mese", "04_bivariee_mese", "05_analyse_bivariee_mese")

FORCER_RECALCUL <- FALSE   # convention du projet : ne recalcule pas si le fichier de sortie existe déjà


# ==============================================================================
# BLOC 1 — FONCTIONS UTILITAIRES
# ==============================================================================

sep   <- function(char = "-", n = 78) paste(rep(char, n), collapse = "")
titre <- function(x) cat("\n", sep("="), "\n  ", x, "\n", sep("="), "\n\n", sep = "")
sous_titre <- function(x) cat("  -- ", x, " --\n", sep = "")

# Journal cumulatif : une ligne par fichier cherché, retrouvé ou non.
MANIFESTE <- tibble(
  categorie = character(), fichier_sortie = character(),
  source_utilisee = character(), statut = character(), n_lignes = integer()
)
log_manifeste <- function(categorie, fichier_sortie, source_utilisee, statut, n_lignes = NA_integer_) {
  MANIFESTE <<- bind_rows(MANIFESTE, tibble(
    categorie = categorie, fichier_sortie = fichier_sortie,
    source_utilisee = source_utilisee %||% "-", statut = statut, n_lignes = n_lignes
  ))
}

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && length(a) > 0 && a != "") a else b

# Essaie une liste de chemins candidats (déjà complets), retourne le premier
# qui existe, ou NA_character_ si aucun.
resoudre_chemin <- function(candidats) {
  ok <- candidats[file.exists(candidats)]
  if (length(ok) == 0) return(NA_character_)
  ok[1]
}

# Construit tous les chemins candidats pour un fichier situé sous
# feature_store/<sous_chemin>, en croisant CANDIDATS_BASE.
candidats_feature_store <- function(sous_chemin) {
  file.path(CANDIDATS_BASE, "feature_store", sous_chemin)
}

# Construit tous les chemins candidats pour un fichier situé sous
# R/Outputs/<sous_dossier_candidats>/<reste>, en croisant CANDIDATS_BASE et
# une liste de noms de sous-dossiers candidats.
candidats_r_outputs <- function(sous_dossiers, reste) {
  as.character(outer(CANDIDATS_BASE, sous_dossiers, function(b, s)
    file.path(b, "R/Outputs", s, reste)))
}

# Copie un CSV : lecture -> écriture propre dans data/, avec log.
copier_csv <- function(categorie, candidats, nom_sortie, dest_dir) {
  src <- resoudre_chemin(candidats)
  dest <- file.path(dest_dir, nom_sortie)
  if (is.na(src)) {
    cat(sprintf("  x MANQUANT : %s (essayé %d chemin(s))\n", nom_sortie, length(candidats)))
    log_manifeste(categorie, nom_sortie, NA_character_, "MANQUANT")
    return(invisible(NULL))
  }
  if (!FORCER_RECALCUL && file.exists(dest)) {
    cat(sprintf("  = déjà présent, non recopié : %s\n", nom_sortie))
    log_manifeste(categorie, nom_sortie, src, "DEJA_PRESENT")
    return(invisible(tryCatch(read_csv(dest, show_col_types = FALSE), error = function(e) NULL)))
  }
  df <- read_csv(src, show_col_types = FALSE)
  write_csv(df, dest)
  cat(sprintf("  OK %-45s <- %s  (%s x %d)\n", nom_sortie, src,
              format(nrow(df), big.mark = " "), ncol(df)))
  log_manifeste(categorie, nom_sortie, src, "OK", nrow(df))
  invisible(df)
}

# Copie une couche spatiale : lecture -> réécriture propre en gpkg, avec log.
# layer permet de cibler une couche précise si le fichier source en contient
# plusieurs (cas des .gpkg multi-couches produits par le paradigme sfnetworks).
copier_spatial <- function(categorie, candidats, nom_sortie, dest_dir, layer = NULL,
                           reprojeter_2154 = TRUE) {
  src <- resoudre_chemin(candidats)
  dest <- file.path(dest_dir, nom_sortie)
  if (is.na(src)) {
    cat(sprintf("  x MANQUANT : %s (essayé %d chemin(s))\n", nom_sortie, length(candidats)))
    log_manifeste(categorie, nom_sortie, NA_character_, "MANQUANT")
    return(invisible(NULL))
  }
  if (!FORCER_RECALCUL && file.exists(dest)) {
    cat(sprintf("  = déjà présent, non recopié : %s\n", nom_sortie))
    log_manifeste(categorie, nom_sortie, src, "DEJA_PRESENT")
    # Relu depuis data/ (pas depuis la source) pour rester disponible en
    # mémoire aux blocs suivants lors d'un relancement partiel du script
    # (ex. bloc 6, qui a besoin de l'objet "communes" même si son .gpkg de
    # sortie a déjà été écrit lors d'un run précédent).
    return(invisible(tryCatch(st_read(dest, quiet = TRUE), error = function(e) NULL)))
  }
  obj <- tryCatch(
    if (is.null(layer)) st_read(src, quiet = TRUE) else st_read(src, layer = layer, quiet = TRUE),
    error = function(e) { cat("    ERREUR lecture :", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(obj)) {
    log_manifeste(categorie, nom_sortie, src, "ERREUR_LECTURE")
    return(invisible(NULL))
  }
  # GeoPackage réserve le nom "fid" pour sa propre clé primaire entière.
  # Plusieurs couches IGN/INSEE (shapefiles ayant déjà transité par un export
  # gpkg) portent un champ "fid"/"FID" hérité dans leur table attributaire ;
  # GDAL refuse alors l'écriture si son type ne correspond pas exactement à
  # ce qu'attend GeoPackage ("Wrong field type for fid"). On le retire avant
  # écriture : select.sf conserve automatiquement la colonne géométrie même
  # sans la nommer explicitement.
  champs_fid <- names(obj)[tolower(names(obj)) == "fid"]
  if (length(champs_fid) > 0) {
    cat("    (champ '", champs_fid[1], "' déjà présent dans la source, retiré avant écriture gpkg)\n", sep = "")
    obj <- obj |> select(-any_of(champs_fid))
  }
  if (reprojeter_2154 && !is.na(st_crs(obj)) && st_crs(obj)$epsg != 2154)
    obj <- st_transform(obj, 2154)
  st_write(obj, dest, delete_dsn = TRUE, quiet = TRUE)
  cat(sprintf("  OK %-45s <- %s  (%s entités)\n", nom_sortie, src, format(nrow(obj), big.mark = " ")))
  log_manifeste(categorie, nom_sortie, src, "OK", nrow(obj))
  invisible(obj)
}

for (d in c(
  DATA_OUT,
  file.path(DATA_OUT, "reference"),
  file.path(DATA_OUT, "batiments"),
  file.path(DATA_OUT, "carreaux"),
  file.path(DATA_OUT, "iris"),
  file.path(DATA_OUT, "iris/stats"),
  file.path(DATA_OUT, "arrondissement")
)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

cat(sep("="), "\n")
cat("  08 - ASSEMBLAGE DES DONNÉES DASHBOARD | SESSTIM 2026\n")
cat("  Sortie :", DATA_OUT, "\n")
cat(sep("="), "\n\n")


# ==============================================================================
# BLOC 2 — COUCHES SPATIALES DE RÉFÉRENCE
# ==============================================================================

titre("2. Couches spatiales de référence")

sous_titre("2.1 Arrondissements (= communes INSEE 13201-13216)")
communes <- copier_spatial(
  "reference",
  candidats_feature_store("04_spatial/commune/COMMUNES_MARSEILLE.shp"),
  "arrondissements.gpkg", file.path(DATA_OUT, "reference"))

sous_titre("2.2 Contour IRIS (géométrie seule)")
iris_bounds <- copier_spatial(
  "reference",
  candidats_feature_store("04_spatial/iris/IRIS_MARSEILLE.shp"),
  "iris_boundaries.gpkg", file.path(DATA_OUT, "reference"))

sous_titre("2.3 Contour carreaux 200 m (géométrie seule)")
carreaux_bounds <- copier_spatial(
  "reference",
  candidats_feature_store("04_spatial/carreaux/CARREAUX_200_M_MARSEILLE.gpkg"),
  "carreaux_boundaries.gpkg", file.path(DATA_OUT, "reference"))

sous_titre("2.4 Parcs et espaces verts corrigés (couche R300)")
parcs <- copier_spatial(
  "reference",
  c(candidats_feature_store("04_spatial/commune/PARCS_JARDINS_PUBLICS_CORRIGES.gpkg"),
    candidats_feature_store("04_spatial/commune/PARCS_JARDAINS_PUBLICS_MARSEILLE_POLY_CORRIGES.gpkg")),
  "parcs_espaces_verts.gpkg", file.path(DATA_OUT, "reference"))


# ==============================================================================
# BLOC 3 — ÉCHELLE BÂTIMENT (N = 256 407)
# ==============================================================================

titre("3. Échelle bâtiment")

sous_titre("3.1 Géométrie des bâtiments")
bat_geo <- copier_spatial(
  "batiments",
  candidats_feature_store("04_spatial/batiments/BATIMENTS_MARSEILLE.gpkg"),
  "batiments_geometrie.gpkg", file.path(DATA_OUT, "batiments"))

sous_titre("3.2 Indicateurs par bâtiment (VEG_BATIMENTS)")
bat_ind <- copier_csv(
  "batiments",
  candidats_feature_store("05_analyses_finales/VEG_BATIMENTS.csv"),
  "batiments_indicateurs.csv", file.path(DATA_OUT, "batiments"))

# Vérification de la clé de jointure géométrie <-> attributs, essentielle
# pour que le dashboard puisse cartographier les bâtiments individuellement.
if (!is.null(bat_geo) && !is.null(bat_ind)) {
  col_id_geo <- intersect(c("ID", "ID_BAT", "id"), names(bat_geo))[1]
  if (is.na(col_id_geo)) {
    cat("  ! Impossible de vérifier la jointure : pas de colonne ID/ID_BAT trouvée dans la géométrie.\n")
  } else {
    n_match <- length(intersect(bat_geo[[col_id_geo]], bat_ind$ID_BAT))
    cat(sprintf("  Vérification jointure : %s bâtiments avec ID commun (colonne géométrie '%s') sur %s attendus\n",
                format(n_match, big.mark = " "), col_id_geo, format(nrow(bat_ind), big.mark = " ")))
    if (n_match < 0.95 * nrow(bat_ind))
      cat("  ! ATTENTION : moins de 95% de correspondance — vérifier le nom de la colonne ID côté géométrie.\n")
  }
}

sous_titre("3.3 Concordance brute par bâtiment (complément diagnostic)")
copier_csv(
  "batiments",
  candidats_feature_store("05_analyses_finales/CONCORDANCE_BATIMENT_regle3_regle30.csv"),
  "batiments_concordance_brute.csv", file.path(DATA_OUT, "batiments"))


# ==============================================================================
# BLOC 3bis — DESSERTE DES PARCS (panneau Composante 300 du dashboard)
# ==============================================================================
#
# Deux versions de la Composante 300 coexistent dans le projet, et ne
# répondent pas au même besoin :
#   - "sfn" (sfnetworks, une seule couche : parcs/jardins ≥ 0,5 ha) — c'est
#     CETTE version qui donne, par parc, le nombre de bâtiments desservis :
#     exactement ce qu'il faut pour le panneau "statistiques parcs" du
#     Module 1 (déclenché par la Composante 300, cf. section 6.4 du prompt
#     de construction du dashboard).
#   - "étendue" (4 couches indépendantes — parcs, Calanques, Nerthe,
#     réserves forestières — combinées par OR) — c'est la version qui
#     alimente l'indicateur officiel Composante 300 déjà agrégé dans
#     VEG_BATIMENTS.csv / VEG_CARREAUX_200M.gpkg / MESE_IRIS_v4.gpkg (celle
#     décrite en Méthodes du mémoire, section 2.4). Elle ne donne PAS le
#     détail par parc.
#
# Les deux sont rapatriées ici : la version sfn pour le panneau parcs, la
# version étendue pour audit/vérification (elle n'a normalement pas besoin
# d'être relue séparément, l'indicateur combiné vit déjà dans VEG_BATIMENTS).
#
# ATTENTION chemins incertains : plusieurs noms de dossier de sortie
# coexistent selon la version du script qui a tourné en dernier
# (PATH_RESULTATS vs PATH_RESULTATS_ETENDU, eux-mêmes non figés dans ce que
# j'ai pu consulter du projet). Le script essaie plusieurs racines
# plausibles ; si aucune ne marche, le manifeste le signale et il faudra
# ajouter le bon chemin à la main (voir R/Outputs/*300* sur ta machine).
# ==============================================================================

titre("3bis. Desserte des parcs — panneau Composante 300")

CANDIDATS_R300_SOUSDOSSIER <- c("regle_300", "03_regle_300", "R300", "regle300")

candidats_r300 <- function(nom_fichier, sous_dossiers_fichier = c("tableaux", "csv")) {
  racines <- as.character(outer(CANDIDATS_BASE, file.path("R/Outputs", CANDIDATS_R300_SOUSDOSSIER), file.path))
  c(
    as.character(outer(racines, sous_dossiers_fichier, file.path, fsep = "/")) |>
      paste0("/", nom_fichier),
    file.path(racines, nom_fichier)   # cas sans sous-dossier de fichier
  )
}

sous_titre("3bis.1 Parcs analysés + bâtiments desservis par parc (version sfn, une couche)")
copier_csv(
  "batiments",
  candidats_r300("regle300_sfn_parcs.csv"),
  "parcs_desserte_par_parc.csv", file.path(DATA_OUT, "reference"))

sous_titre("3bis.2 Statistiques résumées de la Composante 300 (version sfn)")
copier_csv(
  "batiments",
  candidats_r300("regle300_sfn_statistiques.csv"),
  "parcs_desserte_statistiques.csv", file.path(DATA_OUT, "reference"))

sous_titre("3bis.3 Détail par bâtiment (version sfn — parc desservant, distance, nature)")
copier_csv(
  "batiments",
  candidats_r300("regle300_sfn_par_batiment.csv"),
  "parcs_desserte_par_batiment.csv", file.path(DATA_OUT, "batiments"))

sous_titre("3bis.4 Composante 300 étendue (4 couches OR — audit, normalement déjà dans VEG_BATIMENTS)")
copier_csv(
  "batiments",
  candidats_r300("regle300_etendu_par_batiment.csv"),
  "batiments_r300_etendu_audit.csv", file.path(DATA_OUT, "batiments"))

cat("\n  Si les 4 fichiers ci-dessus sont MANQUANTS, vérifier le dossier de\n")
cat("  sortie réel du script régle 300 (variable PATH_RESULTATS[_ETENDU] dans\n")
cat("  regle_300_sfn_BD_TOPO_2025_06.R / regle_300_sfn_Complet.R) et l'ajouter\n")
cat("  à CANDIDATS_R300_SOUSDOSSIER ci-dessus avant de relancer.\n\n")


# ==============================================================================
# BLOC 4 — ÉCHELLE CARREAU 200 M (N = 3 271)
# ==============================================================================

titre("4. Échelle carreau 200 m")

sous_titre("4.1 Carreaux avec indicateurs végétation (géométrie + attributs)")
copier_spatial(
  "carreaux",
  candidats_feature_store("05_analyses_finales/VEG_CARREAUX_200M.gpkg"),
  "carreaux_200m.gpkg", file.path(DATA_OUT, "carreaux"))

cat("\n  Rappel : aucun agrégat MESE (EDI/LST/pollution) n'existe à l'échelle\n")
cat("  carreau dans le pipeline actuel — seule la végétation y est disponible.\n\n")


# ==============================================================================
# BLOC 5 — ÉCHELLE IRIS (N = 393)
# ==============================================================================

titre("5. Échelle IRIS")

sous_titre("5.1 MESE complète (géométrie + 67 variables)")
iris_mese <- copier_spatial(
  "iris",
  candidats_feature_store("05_analyses_finales/MESE_IRIS_v4.gpkg"),
  "iris_mese.gpkg", file.path(DATA_OUT, "iris"))

sous_titre("5.2 ICVSE (3 méthodes + clusters HCPC)")
iris_icvse <- copier_spatial(
  "iris",
  candidats_r_outputs(CANDIDATS_ICVSE_SOUSDOSSIER, "ICVSE_IRIS_complet.gpkg"),
  "iris_icvse.gpkg", file.path(DATA_OUT, "iris"))

sous_titre("5.3 Végétation seule à l'IRIS (complément sans géométrie)")
copier_csv(
  "iris",
  candidats_feature_store("05_analyses_finales/VEG_IRIS.csv"),
  "iris_veg_seule.csv", file.path(DATA_OUT, "iris"))

sous_titre("5.4 LST MAX 2021-2025 (validé, hors coeur MESE)")
copier_csv(
  "iris",
  candidats_feature_store("05_analyses_finales/exploratoire_lst_max/LST_MAX_IRIS_2021_2025.csv"),
  "iris_lst_max_2021_2025.csv", file.path(DATA_OUT, "iris"))

cat("\n  Note : les tables statistiques (concordance, bivarié, profils, AFM)\n")
cat("  sont déjà réunies dans data/iris/stats/ — celles-ci proviennent du\n")
cat("  mémoire (accessible depuis Claude) et ne sont pas re-cherchées ici.\n")
cat("  Fichiers rejetés/ambigus déjà isolés dans data/iris/stats/_do_not_use/,\n")
cat("  voir DATA_INVENTORY.md pour le détail de chaque exclusion.\n\n")


# ==============================================================================
# BLOC 6 — ÉCHELLE ARRONDISSEMENT (calculée, pas de fichier source)
# ==============================================================================

titre("6. Échelle arrondissement (calculée par agrégation pondérée)")

if (is.null(iris_mese)) {
  cat("  x Impossible de calculer les stats arrondissement : iris_mese introuvable.\n")
  cat("    Relancer ce script une fois le Bloc 5.1 résolu.\n\n")
} else {
  # Code arrondissement = 5 premiers chiffres du CODE_IRIS (13201 à 13216).
  # Le retraitement en sprintf("%09d", ...) reprend la convention déjà en
  # place dans le script 07 (mémoire) pour garantir un CODE_IRIS à 9
  # caractères avant d'en extraire le préfixe, quel que soit le type
  # (numérique ou caractère) sous lequel gpkg le restitue.
  iris_df <- iris_mese |>
    st_drop_geometry() |>
    mutate(CODE_IRIS = sprintf("%09d", as.integer(CODE_IRIS)),
           arrond = str_sub(CODE_IRIS, 1, 5))

  # Poids d'agrégation : nombre de bâtiments par IRIS si disponible dans
  # iris_mese (colonne n_bat), sinon poids égal à 1 (moyenne simple).
  col_poids <- intersect(c("n_bat", "n_batiments"), names(iris_df))[1]
  if (is.na(col_poids)) {
    cat("  ! Pas de colonne n_bat trouvée dans MESE_IRIS_v4 : agrégation en\n")
    cat("    moyenne simple par IRIS plutôt qu'en moyenne pondérée par le\n")
    cat("    nombre de bâtiments. Vérifier si c'est le comportement voulu.\n")
    iris_df$poids_agregation <- 1
  } else {
    iris_df$poids_agregation <- iris_df[[col_poids]]
  }

  vars_num <- iris_df |>
    select(where(is.numeric), -any_of(c("poids_agregation"))) |>
    names()
  vars_num <- setdiff(vars_num, col_poids %||% "")

  moyenne_ponderee <- function(x, w) {
    ok <- !is.na(x) & !is.na(w)
    if (!any(ok)) return(NA_real_)
    weighted.mean(x[ok], w[ok])
  }

  stats_arrond <- iris_df |>
    group_by(arrond) |>
    summarise(
      n_iris = n(),
      across(all_of(vars_num), ~ moyenne_ponderee(.x, poids_agregation)),
      .groups = "drop"
    )

  # Jointure à la géométrie des arrondissements pour produire une couche
  # cartographiable directement.
  if (!is.null(communes)) {
    col_code_comm <- intersect(c("INSEE_COM", "CODE_COMM", "insee_com"), names(communes))[1]
    if (!is.na(col_code_comm)) {
      arrond_geo <- communes |>
        select(all_of(col_code_comm)) |>
        rename(arrond = all_of(col_code_comm)) |>
        mutate(arrond = as.character(arrond)) |>
        left_join(stats_arrond, by = "arrond")
      st_write(arrond_geo, file.path(DATA_OUT, "arrondissement/arrondissement_stats.gpkg"),
               delete_dsn = TRUE, quiet = TRUE)
      cat(sprintf("  OK arrondissement_stats.gpkg (%d arrondissements x %d variables agrégées)\n",
                  nrow(arrond_geo), length(vars_num)))
      log_manifeste("arrondissement", "arrondissement_stats.gpkg",
                    "calculé depuis iris_mese.gpkg", "OK", nrow(arrond_geo))
    } else {
      cat("  ! Colonne de code commune non trouvée dans arrondissements.gpkg\n")
      cat("    (attendu : INSEE_COM ou CODE_COMM). Export du tableau seul, sans géométrie.\n")
      write_csv(stats_arrond, file.path(DATA_OUT, "arrondissement/arrondissement_stats.csv"))
      log_manifeste("arrondissement", "arrondissement_stats.csv",
                    "calculé depuis iris_mese.gpkg", "OK_SANS_GEOMETRIE", nrow(stats_arrond))
    }
  } else {
    cat("  ! Communes introuvables : export du tableau seul, sans géométrie.\n")
    write_csv(stats_arrond, file.path(DATA_OUT, "arrondissement/arrondissement_stats.csv"))
    log_manifeste("arrondissement", "arrondissement_stats.csv",
                  "calculé depuis iris_mese.gpkg", "OK_SANS_GEOMETRIE", nrow(stats_arrond))
  }
}


# ==============================================================================
# BLOC 7 — TABLES COMPLÉMENTAIRES DÉJÀ PRODUITES (concordance, bivarié)
# ==============================================================================

titre("7. Tables complémentaires (R/Outputs, complément aux tables du mémoire)")

sous_titre("7.1 Assignation de cluster par IRIS (utilisée par app.R existant)")
copier_csv(
  "iris",
  candidats_r_outputs(CANDIDATS_BIVARIEE_SOUSDOSSIER, "tables/IRIS_cluster_affectation.csv"),
  "iris_stats_cluster_affectation_complement.csv", file.path(DATA_OUT, "iris/stats"))

cat("\n  Les autres tables de R/Outputs/04_concordance et\n")
cat("  R/Outputs/05_bivariee_mese recoupent trait pour trait celles déjà\n")
cat("  livrées dans data/iris/stats/ depuis le mémoire (mêmes analyses, même\n")
cat("  script source). Pas de re-collecte automatique ici pour éviter les\n")
cat("  doublons ; si une table précise manque, l'ajouter à la main dans\n")
cat("  data/iris/stats/ ou étendre ce bloc.\n\n")


# ==============================================================================
# BLOC 8 — MANIFESTE FINAL
# ==============================================================================

titre("8. Manifeste final")

write_csv(MANIFESTE, file.path(DATA_OUT, "00_MANIFEST.csv"))

n_ok       <- sum(MANIFESTE$statut %in% c("OK", "OK_SANS_GEOMETRIE"))
n_deja     <- sum(MANIFESTE$statut == "DEJA_PRESENT")
n_manquant <- sum(MANIFESTE$statut == "MANQUANT")
n_erreur   <- sum(MANIFESTE$statut == "ERREUR_LECTURE")

cat(sep("="), "\n")
cat("  RÉCAPITULATIF\n")
cat(sep("="), "\n")
cat(sprintf("  Fichiers rassemblés avec succès : %d\n", n_ok))
cat(sprintf("  Déjà présents (non re-copiés)    : %d\n", n_deja))
cat(sprintf("  MANQUANTS (chemin introuvable)   : %d\n", n_manquant))
cat(sprintf("  Erreurs de lecture               : %d\n", n_erreur))
cat("\n  Détail : data/00_MANIFEST.csv\n")

if (n_manquant > 0) {
  cat("\n  Fichiers manquants :\n")
  MANIFESTE |> filter(statut == "MANQUANT") |>
    pwalk(function(categorie, fichier_sortie, ...) cat("    -", categorie, "/", fichier_sortie, "\n"))
  cat("\n  Pour chacun, vérifier/corriger le chemin dans CANDIDATS_BASE ou dans\n")
  cat("  le bloc concerné, puis relancer (FORCER_RECALCUL = FALSE, donc les\n")
  cat("  fichiers déjà obtenus ne sont pas re-traités).\n")
}

cat("\n", sep("="), "\n", "  data/ prêt pour implémentation dashboard (R/Shiny ou Python/Django)\n",
    sep("="), "\n", sep = "")
