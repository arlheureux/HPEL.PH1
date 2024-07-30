## _________________ Infos générales
## Nom : replicate_rows
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 20 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes 
#
#' Modification de la fonction du package threadr pour enlever le each obligatoire
#' @param df cf threadr::replicate_rows()
#' @param n cf threadr::replicate_rows()
#' @param reset cf threadr::replicate_rows()
#' @param each Logique : considérer each ou pas
#' @return Le résultats du chi2
#
## ---------------------------


replicate_rows <- function (df, n, reset = TRUE, each = FALSE) {
  if (reset) {
    row.names(df) <- NULL
  }
  
  if(isTRUE(each)) {
    df <- df[rep(seq_len(nrow(df)), each = n), ]
  }else{
    df <- df[rep(seq_len(nrow(df)), n), ]
  }
  
  if (reset) {
    row.names(df) <- NULL
  }
  
  df
}
