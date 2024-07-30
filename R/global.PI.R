## _________________ Infos générales
## Nom : global.PI.R
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
#' Fonction qui sort le tableau des tailles basé sur les pigments
#' @param Data_TS_ok Tableau de données
#' @param GF1 Groupe fonctionnel 1 (doit correspondre a un argument de la colonne Param)
#' @param GF2 Groupe fonctionnel 2 (doit correspondre a un argument de la colonne Param)
#' @return Le tableau de données des fractions et biomasses CHLA issues des pigments
#
## ---------------------------

global.PI <- function(Data_TS_ok, GF1, GF2) {
    
    Data_TS_GF <- Data_TS_ok[Data_TS_ok$Param %in% c(GF1, GF2), ]
    Data_TS_GF$Anomalies <- FALSE

    for (x in unique(Data_TS_GF$Param)) {
        for (s in unique(Data_TS_ok$Site)) {
            tmp <- Data_TS_GF[Data_TS_GF$Param == x & Data_TS_GF$Site == s, ]

            # je scinde en 2 les périodes éval et ref
            Anomalies.R <- tmp[tmp$Year < year.max - duree.eval + 1, ]
            Anomalies.E <- tmp[tmp$Year >= year.max - duree.eval + 1, ]

            # Je récupère les anomalies de ref et eval basées sur la moyenne de ref
            Anomalies.R$Ano <- Anomalies.R$fill - mean(Anomalies.R$fill)
            Anomalies.E$Ano <- Anomalies.E$fill - mean(Anomalies.R$fill)

            # je récupère les quantiles et les fréquences associées
            quantiles <- get.quantile.ano(x = Anomalies.R$Ano, xsup = Anomalies.E$Ano)
            ano <- get.anomalies.extreme.dates(Anomalies.R, Anomalies.E, quantiles)

            Data_TS_GF[
                Data_TS_GF$Site == s &
                    Data_TS_GF$YearTS %in% ano$YearTS &
                    Data_TS_GF$Param == x,
                "Anomalies"
            ] <- TRUE
        }
    }



    dfs <- get.df.eval.ref(Data_TS_GF, GF1, GF2)
    df_eval <- dfs$eval
    df_ref <- dfs$ref

    p <- 0.9



    # Ici je travaille sur df_ref afin de calculer mes enveloppes du PI
    for (s in unique(df_ref$Site)) {
        PI.hull <- get.PI.hull(
            xR = df_ref[df_ref$Site == s, "GF1"], yR = df_ref[df_ref$Site == s, "GF2"],
            p = p
        )

        # Ici on reprend df_eval
        PI <- PI.calc(
            xE = df_eval[df_eval$Site == s, "GF1"], yE = df_eval[df_eval$Site == s, "GF2"],
            PI.hull, p = p
        )


        if (!dir.exists(file.path(OUT, "PI", s))) {
            dir.create(file.path(OUT, "PI", s), recursive = TRUE)
        }

        PI.plot(
            dataR = df_ref[df_ref$Site == s, ],
            dataE = df_eval[df_eval$Site == s, ],
            ds = "Rephy", GF1 = GF1, GF2 = GF2,
            col = "Month",
            PI.hull = PI.hull, PI = PI, path = file.path(OUT, "PI", s, paste(GF1, GF2, sep="_"))
        )

        PI.plot(
            dataR = df_ref[df_ref$Site == s, ],
            dataE = df_eval[df_eval$Site == s, ],
            ds = "Rephy", GF1 = GF1, GF2 = GF2,
            col = "Year",
            PI.hull = PI.hull, PI = PI, path = file.path(OUT, "PI", s, paste(GF1, GF2, sep="_"))
        )

        PI.anomalies.plot(
            dataR = df_ref[df_ref$Site == s, ],
            dataE = df_eval[df_eval$Site == s, ],
            ds = "Rephy", GF1 = GF1, GF2 = GF2,
            PI.hull = PI.hull, PI = PI, path = file.path(OUT, "PI", s, paste(GF1, GF2, sep="_"))
        )
    }
}
