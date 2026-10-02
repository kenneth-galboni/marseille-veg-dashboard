# ==============================================================================
# 01_alleger_batiments_v2.R
# Deuxième passe d'allègement de data/batiments/ (app.R inchangé).
#
# Leviers :
#   1. GPKG écrit sans index spatial (SPATIAL_INDEX=NO)
#   2. ID_BAT texte "BATIMENT0000000318176135" -> numéro de ligne entier,
#      identique dans les 4 fichiers (les match() de l'app restent valides)
#   3. Simplification des polygones (SIMPLIFY_M, en mètres, calculée en Lambert-93)
#
# À lancer depuis le dossier du dashboard. Les versions actuelles des 4 fichiers
# sont copiées dans BACKUP_DIR au premier lancement ; les lancements suivants
# repartent de cette sauvegarde, donc on peut changer SIMPLIFY_M et relancer.
# La table de correspondance numéro -> ID_BAT d'origine est écrite hors du
# projet (ID_MAP_FILE) pour ne pas alourdir l'export Shinylive.
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(readr)
  library(dplyr)
})

# ---- Paramètres --------------------------------------------------------------
DIR_BAT     <- "data/batiments"
DIR_DATA    <- "data"
BACKUP_DIR  <- path.expand("~/archives_marseille/batiments_v1_light")
ID_MAP_FILE <- path.expand("~/archives_marseille/correspondance_ID_BAT.csv")
SIMPLIFY_M  <- 1.5     # mètres ; 0.5 = identique à la v1, 2 ou 3 = plus léger
RATIO_JSON  <- 1.25    # app.json / data/ observé (117 Mo / 94 Mo)
TOL_SURFACE <- 0.02    # écart toléré sur la surface totale d'emprise

F  <- list(geom = "bat_geom.gpkg", ind = "bat_ind.csv",
           arr  = "bat_arr.csv",   parcs = "bat_parcs.csv")
fp  <- function(k) file.path(DIR_BAT, F[[k]])
src <- function(k) file.path(BACKUP_DIR, F[[k]])
mo  <- function(f) round(file.info(f)$size / 1024^2, 2)
msg <- function(...) message(sprintf(...))

for (k in names(F)) if (!file.exists(fp(k)) && !file.exists(src(k)))
  stop("Fichier introuvable : ", fp(k))

# ---- Sauvegarde de l'état actuel (une seule fois) -----------------------------
dir.create(BACKUP_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(ID_MAP_FILE), recursive = TRUE, showWarnings = FALSE)
for (k in names(F)) {
  if (!file.exists(src(k))) file.copy(fp(k), src(k))
}
avant <- sapply(names(F), function(k) mo(src(k)))
data_avant <- sum(file.size(list.files(DIR_DATA, recursive = TRUE, full.names = TRUE))) / 1024^2

# ==============================================================================
# 1. LECTURE (depuis la sauvegarde)
# ==============================================================================
message("\n== Lecture ==")
ind <- read_csv(src("ind"),
                col_types = cols(.default = col_integer(), ID_BAT = col_character()),
                show_col_types = FALSE, progress = FALSE)
arr <- read_csv(src("arr"),
                col_types = cols(ID_BAT = col_character(), CODE_IRIS = col_integer()),
                show_col_types = FALSE, progress = FALSE)
parcs <- read_csv(src("parcs"), col_types = cols(.default = col_character()),
                  show_col_types = FALSE, progress = FALSE)
g <- st_read(src("geom"), quiet = TRUE)

if (!all(startsWith(ind$ID_BAT, "BATIMENT")))
  stop("Les ID de la sauvegarde ne sont pas au format BATIMENT... : sauvegarde déjà convertie ?")
if (anyDuplicated(ind$ID_BAT)) stop("ID_BAT dupliqués dans bat_ind.csv")
msg("  %d bâtiments, %d polygones, %d desservis", nrow(ind), nrow(g), nrow(parcs))

# Statistiques de référence, calculées AVANT toute transformation
cols_bin <- setdiff(names(ind), "ID_BAT")
taux_ref <- vapply(cols_bin, function(cc) mean(ind[[cc]] == 1, na.rm = TRUE), numeric(1))
parcs_ref <- list(n = nrow(parcs),
                  esp = length(unique(parcs$nom_espace_retenu)),
                  cat = as.integer(table(parcs$label_espace_retenu)))

# ==============================================================================
# 2. IDENTIFIANTS COMPACTS
# ==============================================================================
ids_orig <- ind$ID_BAT
new_id <- function(x) match(x, ids_orig)

write_csv(data.frame(id = seq_along(ids_orig), ID_BAT = ids_orig), ID_MAP_FILE)
msg("  table de correspondance : %s", ID_MAP_FILE)

ind$ID_BAT <- seq_len(nrow(ind))

arr$ID_BAT <- new_id(arr$ID_BAT)
if (anyNA(arr$ID_BAT)) { msg("  %d lignes bat_arr sans correspondance, retirées", sum(is.na(arr$ID_BAT))); arr <- arr[!is.na(arr$ID_BAT), ] }

parcs$ID <- new_id(parcs$ID)
if (anyNA(parcs$ID)) { msg("  %d lignes bat_parcs sans correspondance, retirées", sum(is.na(parcs$ID))); parcs <- parcs[!is.na(parcs$ID), ] }

g$ID <- new_id(g$ID)
if (anyNA(g$ID)) { msg("  %d polygones sans correspondance, retirés", sum(is.na(g$ID))); g <- g[!is.na(g$ID), ] }

# ==============================================================================
# 3. GÉOMÉTRIE : simplification en Lambert-93, retour en WGS84
# ==============================================================================
message("\n== Géométrie ==")
g <- st_transform(g, 2154)
surf0 <- sum(as.numeric(st_area(g)))
if (SIMPLIFY_M > 0) {
  g <- st_simplify(g, preserveTopology = TRUE, dTolerance = SIMPLIFY_M)
  g <- g[!st_is_empty(g), ]
}
surf1 <- sum(as.numeric(st_area(g)))
g <- st_transform(g, 4326)
msg("  tolérance %.2f m, surface d'emprise : %.3f %% de l'originale", SIMPLIFY_M, 100 * surf1 / surf0)

# ==============================================================================
# 4. ÉCRITURE (mêmes noms de fichiers : app.R n'est pas modifié)
# ==============================================================================
message("\n== Écriture ==")
if (file.exists(fp("geom"))) file.remove(fp("geom"))
st_write(g, fp("geom"), layer = "bat_geom",
         layer_options = "SPATIAL_INDEX=NO", quiet = TRUE)
write_csv(ind,   fp("ind"))
write_csv(arr,   fp("arr"))
write_csv(parcs, fp("parcs"))

# ==============================================================================
# 5. CONTRÔLES
# ==============================================================================
message("\n== Contrôles ==")
erreurs <- character()
ok <- function(t) msg("  [OK]    %s", t)
ko <- function(t) { erreurs <<- c(erreurs, t); msg("  [ECHEC] %s", t) }

ind2   <- read_csv(fp("ind"),   show_col_types = FALSE, progress = FALSE)
arr2   <- read_csv(fp("arr"),   show_col_types = FALSE, progress = FALSE)
parcs2 <- read_csv(fp("parcs"), show_col_types = FALSE, progress = FALSE)
g2     <- st_read(fp("geom"), quiet = TRUE)

if (nrow(ind2) == length(ids_orig)) ok(sprintf("%d lignes dans bat_ind.csv", nrow(ind2))) else ko("nombre de lignes de bat_ind.csv")

for (cc in cols_bin) {
  if (isTRUE(all.equal(taux_ref[[cc]], mean(ind2[[cc]] == 1, na.rm = TRUE)))) {
    ok(sprintf("taux %-26s = %.4f %%", cc, 100 * taux_ref[[cc]]))
  } else ko(paste("taux différent pour", cc))
}

if (identical(sort(names(ind2)), sort(names(ind)))) ok("colonnes de bat_ind.csv inchangées") else ko("colonnes de bat_ind.csv modifiées")

if (all(arr2$ID_BAT %in% ind2$ID_BAT) && all(parcs2$ID %in% ind2$ID_BAT) && all(g2$ID %in% ind2$ID_BAT)) {
  ok("tous les ID de arr, parcs et geom existent dans bat_ind")
} else ko("des ID orphelins")

p_arr <- mean(ind2$ID_BAT %in% arr2$ID_BAT); p_geo <- mean(ind2$ID_BAT %in% g2$ID)
msg("         couverture : %.3f %% des bâtiments dans bat_arr, %.3f %% avec une géométrie", 100 * p_arr, 100 * p_geo)
if (p_arr < 1) ko("des bâtiments sans CODE_IRIS")
if (p_geo < 0.999) ko("plus de 0,1 % de bâtiments sans géométrie")

if (nrow(parcs2) == parcs_ref$n &&
    length(unique(parcs2$nom_espace_retenu)) == parcs_ref$esp &&
    identical(as.integer(table(parcs2$label_espace_retenu)), parcs_ref$cat)) {
  ok("statistiques parcs identiques")
} else ko("statistiques parcs différentes")

if (abs(1 - surf1 / surf0) <= TOL_SURFACE) {
  ok(sprintf("surface d'emprise conservée à %.2f %%", 100 * surf1 / surf0))
} else ko("la simplification déforme trop les emprises, baisser SIMPLIFY_M")

bb <- st_bbox(g2)
if (isTRUE(st_crs(g2)$epsg == 4326) && max(abs(bb)) <= 180) ok("géométrie en EPSG:4326") else ko("CRS ou emprise incohérents")

# ---- Bilan -------------------------------------------------------------------
apres <- sapply(names(F), function(k) mo(fp(k)))
bilan <- data.frame(fichier = unlist(F), avant_Mo = as.numeric(avant), apres_Mo = as.numeric(apres), row.names = NULL)
print(bilan, row.names = FALSE)
data_apres <- sum(file.size(list.files(DIR_DATA, recursive = TRUE, full.names = TRUE))) / 1024^2
msg("\ndata/ : %.1f Mo -> %.1f Mo", data_avant, data_apres)
msg("app.json estimé : %.0f Mo (ratio %.2f, à confirmer à l'export ; limite GitHub 100 Mo)", RATIO_JSON * data_apres, RATIO_JSON)

if (length(erreurs)) {
  warning("Contrôles en échec : ", paste(erreurs, collapse = " | "),
          "\nPour revenir en arrière : copier le contenu de ", BACKUP_DIR, " dans ", DIR_BAT)
} else {
  message("\nTous les contrôles sont OK. Relancer shinylive::export() puis tester la carte bâtiments.")
}