## _________________ Infos générales
## Nom : TS.agreg.UT.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 1 juillet 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction mère de l'agrégation temporelles par moyenne normale des UT
#' @param Data_TS Data : must have columns :
#' \itemize{
#' Year : integer for year
#' Month : interger for season (could be day, week, month etc)
#' Site : Sites at which data is available
#' Param : Parameter for which data is available
#' Val : value of parameter at site at year y at month m
#' Latitude : coordinates in decimal
#' Longitude : coordinates in decimal
#' }
#' @param agg.func Texte : fonction pour aggréger
#' @param year.min Numérique : Année min
#' @param year.max Numérique : Année max
#' @param season Numérique : Numéro de saisons. Par défaut 1:12
#' @param nb.cores Numérique : Nombre de coeurs sur lesquels paralléliser
#' @return Les données agrégées
#' @examples 
#' TS.agreg.UT(Data_TS = data, agg.func = "mean", year.min = 1980, year.max = 2020, season = 1:12, nb.cores = detectCores()/2)
#
## ---------------------------


TS.agreg.UT <- function(Data_TS, agg.func, year.min, year.max, season, nb.cores = parallel::detectCores()/2) {
    cat("Aggrégation des séries en utilisant la fonction :", agg.func, "\n")

    Data_TS_month <- do.call(
        "rbind",
        pbmcapply::pbmclapply(
            mc.cores = nb.cores,
            year.min:year.max,
            function(y) {
                tmp01 <- Data_TS[Data_TS$Year == y, ]

                out01 <- do.call(
                    "rbind",
                    lapply(
                        unique(season),
                        function(m) {
                            tmp02 <- tmp01[tmp01$Month == m, ]

                            OUT02 <- data.frame()
                            for (p in unique(Data_TS$Param)) {
                                for (s in unique(Data_TS$Site)) {
                                    tmp03 <- tmp02[tmp02$Param == p & tmp02$Site == s, ]

                                    if (nrow(tmp03) >= 1) {

                                        # Je somme pour une même UT même date
                                        tmp03$Serie <- paste(tmp03$Date, tmp03$Param, sep = "_")

                                        tmp04 <- do.call(
                                            "rbind",
                                            lapply(
                                               unique(tmp03$Serie),
                                               function(se) {
                                                tmp031 <- tmp03[tmp03$Serie == se, ]
                                                out <- tmp031[1, ]
                                                out$Val <- sum(tmp031$Val)
                                                return(out)
                                               }
                                            )
                                        )

                                        out02 <- data.frame(
                                            YearTS = lubridate::decimal_date(as.Date(paste(y, m, "15", sep = "-"))),
                                            Year = y,
                                            Month = m,
                                            Site = s,
                                            Param = p,
                                            Val = mean(tmp04$Val, na.rm = TRUE)
                                        )
                                    } else {
                                        out02 <- data.frame(
                                            YearTS = lubridate::decimal_date(as.Date(paste(y, m, "15", sep = "-"))),
                                            Year = y,
                                            Month = m,
                                            Site = s,
                                            Param = p,
                                            Val = NA
                                        )
                                    }

                                    OUT02 <- rbind(OUT02, out02)
                                }
                            }


                            return(OUT02)
                        }
                    )
                )
                return(out01)
            }
        )
    )

    return(Data_TS_month)
}
