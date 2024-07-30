## _________________ Infos générales
## Nom : extra.range.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 8 juillet 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Wrapper de range qui permet d'ajouter ou soustraire une valeur ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  range
#' @param x Vecteur de données
#' @param extra Quelle valeur ajouter ou soustraire. Défaut = 1
#' @param extra.min Quelle valeur ajouter ou soustraire au minimum. Défaut = NULL
#' @param extra.max Quelle valeur ajouter ou soustraire au maximum. Défaut = NULL
#' @return Les données avec colonnes de valeurs interpolées
#' @export
#
## ---------------------------


extra.range <- function(x, extra = 1, extra.min = NULL, extra.max = NULL) {
    y <- range(x, na.rm = TRUE)
    if (!is.null(extra) & is.null(extra.min) & is.null(extra.max)) {
        y[1] <- y[1] - extra
        y[2] <- y[2] + extra
    } else {
        y[1] <- y[1] - extra.min
        y[2] <- y[2] + extra.max
    }

    return(y)
}

