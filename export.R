library(shinylive)

# Générer l'export Shinylive vers le dossier docs/
shinylive::export(appdir = ".", destdir = "docs")



# Suppression
if (dir.exists("docs")) unlink("docs", recursive = TRUE)

# Export avec prise en compte du .shinyliveignore
shinylive::export(appdir = ".", destdir = "docs")
