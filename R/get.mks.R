## _________________ Infos générales
## Nom : get.mks.R
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
#' Fonction qui récupère les résultats du mann kendall saisonnier
#' @param Data_TS_ok Le tableau de données à utiliser
#' @return Les résultats du mann kendall saisonnier
#' @export
#
## ---------------------------

get.mks <- function(Data_TS_ok) {
  res.mks <- list()
  k <- 0
  names <- c()

  for (x in unique(Data_TS_ok$Param)) {
    tmp <- Data_TS_ok[Data_TS_ok$Param == x, ]

    for (s in unique(tmp$Site)) {

      #cat("Param", x, " au site", s, "\n\n")

      k <- k + 1
      data.x <- tmp[tmp$Site == s, ]
      
      if (uniqueN(data.x$Year) >= 2) {
        mksm <- kendallSeasonalTrendTest.seas.pval(
          x = data.x$YearTS,
          y = data.x$fill,
          year = data.x$Year,
          season = as.factor(data.x$Month),
          alternative = "two.sided",
          Dates = data.x$YearTS
        )

        # Au cas ou si ça sort NA
        mksm$p.value.s[is.na(mksm$p.value.s)] <- 1
        # mksm$p.value.s
        # mksm$p.value
        # mksm$estimate

        res.mks[[k]] <- list(
          mksm = mksm,
          data.x = data.x
        )
      }else{
         res.mks[[k]] <- list(
           mksm = NA,
           data.x = data.x
         )
      }

      xs <- paste(x, s, sep = "_")
      names <- c(names, xs)
    }
  }

  res.mks <- setNames(res.mks, names)

  return(res.mks)
}
