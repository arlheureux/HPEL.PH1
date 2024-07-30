## _________________ Infos générales
## Nom : gg_png.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 18 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction qui sauve un gpglot
#' @param GG_plot Le ggplot
#' @param filename Le nom sous lequel le sauver
#' @param width La largeur de l'image. Par défaut 30cm
#' @param height La hauteur de l'image. Par défaut 20cm
#' @param units L'unité des hauteur et largeur. Par défaut cm
#' @param res La résolution de l'image
#' @param path Le chemin de sauvegarde
#' @param verose Ecrire les sorties images ou non
#' @return Un plot sauvegardé au chemin spécifié
#
## ---------------------------

gg_png <- function(GG_plot, filename, width = 30, height = 20, units = "cm", res = 300, path=NULL,
                  verbose = TRUE, ...){
  
  
  if(!is.null(path)){
    if(!dir.exists(path))
      dir.create(path, recursive = T)
    
    filename <- file.path(path,filename)
  }

  
  png(filename = filename, width = width, height = height, units = units, res = res)
  invisible(print(GG_plot))
  dev.off()
  
  
  if (verbose) {
    if (is.null(path)) {
      cat(
        "\nImage sauvée dans", getwd(), ":)", "\n", "Largeur =", paste0(width, units),
        "Hauteur =", paste0(height, units), "Résolution =", res, "\n"
      )
    } else {
      cat(
        "\nImage sauvée dans", path, ":)", "\n", "Largeur =", paste0(width, units),
        "Hauteur =", paste0(height, units), "Résolution =", res, "\n"
      )
    }
  }

  
}
