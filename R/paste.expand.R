## _________________ Infos générales
## Nom : paste.expand.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 25 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction qui fait un paste expand
#' @param x vecteur ÃƒÂ  coller
#' @param y vecteur ÃƒÂ  coller
#' @param each valeurs 'x' ou 'y' pour définir quel vecteur sera répété = comme le 'each' dans la fonction 'rep()'
#' @param sep : séparateur comme dans la fonction 'paste()'
#' @return Un vecteur avec les trucs voulus
#
## ---------------------------

paste.expand <- function(x, y, each = "x", sep = "_") {
  
  if(each == "x") {
    xy <- data.frame(x = rep(x, each = length(y)),
                     y = unique(y))
  }
 
  if(each == "y") {
    xy <- data.frame(x = unique(x),
                     y = rep(y, each = length(x)))
  }
  
  out <- paste(xy$x, xy$y, sep = sep)
  return(out)
  
}
