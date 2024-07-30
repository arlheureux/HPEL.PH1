## _________________ Infos générales
## Nom : Remove.duplicates.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 2 juillet 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction pour enlever les duplicats
#' @param data Les données
#' @param nb.cores Numérique : Nombre de coeurs sur lesquels paralléliser
#' @return Les données ok
#
## ---------------------------

Remove.duplicates <- function(data, nb.cores = detectCores()/2) {

    data$Serie <- paste(data$UT, data$Site, data$Date, data$Val, sep = "_")

    out <- do.call(
        "rbind",
        pbmclapply(
            mc.cores = nb.cores, 
            unique(data$Serie),
            function(s) {
                tmp <- data[data$Serie == s,]
                tmp <- tmp[1,]
                return(tmp)
            }
        )
    )

    return(out)
}
