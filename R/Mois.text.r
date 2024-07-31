## _________________ Infos générales
## Nom : Mois.text.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 12 juillet 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________


## _________________ Notes
#
#' Fonction qui change les mois décimaux en mois en texte
#' @param x Un vecteur numérique de mois
#' @return Les données phyto ok
#' @importFrom plyr revalue
#
## ---------------------------


Mois.text <- function(x) {
revalue(x,
    replace = c(
        "1" = "Janvier",
        "2" = "Février",
        "3" = "Mars",
        "4" = "Avril",
        "5" = "Mai",
        "6" = "Juin",
        "7" = "Juillet",
        "8" = "Août",
        "9" = "Septembre",
        "10" = "Octobre",
        "11" = "Novembre",
        "12" = "Décembre"
    )
)
}
