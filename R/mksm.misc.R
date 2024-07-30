## _________________ Infos générales
## Nom : id.year.a.enlever.R
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
#' Fonction qui créer le tableau Sendf pour les plots
#' @param mksm Le tableau de contingence
#' @return dataframe Sendf
#
## ---------------------------

mksm.get.seasonal.estimate <- function(mksm){
  data.frame(a= mksm$seasonal.estimate[,"slope"], 
             b= mksm$seasonal.estimate[,"intercept"], 
             Month=factor(c("Janvier", "Février", "Mars", "Avril",
                           "Mai", "Juin", "Juillet", "AoÃƒÂ»t",
                           "Septembre", "Octobre", "Novembre", "Décembre"), 
                         levels = c("Janvier", "Février", "Mars", "Avril",
                                    "Mai", "Juin", "Juillet", "AoÃƒÂ»t",
                                    "Septembre", "Octobre", "Novembre", "Décembre"))
  )
}

## _________________ Notes
#
#' Fonction qui créer le tableau Sendf pour les plots
#' @param data.x Data issues de get.mks()
#' @param Sen_df dataframe Sendf
#' @param year.min Numérique année min
#' @param year.max Numérique année max
#' @return dataframe Sendf
#
## ---------------------------

mksm.fill.Sen.df <- function(data.x, Sen_df, year.min, year.max, Month = TRUE) {

  
  if (Month) {
    Sen_df <- rbind(Sen_df, Sen_df)
    Sen_df <- Sen_df[order(Sen_df$Month), ]
    Sen_df$x <- rep(c(year.min, year.max))
    Sen_df$y <- Sen_df$x * Sen_df$a + Sen_df$b

    for (i in unique(Sen_df$Month)) {
      tmp <- Sen_df[Sen_df$Month == i, ]

      te <- 1 + (tmp[2, "y"] - tmp[1, "y"]) / tmp[1, "y"]
      tag <- round(100 * (tmp[2, "y"] - tmp[1, "y"]) / tmp[1, "y"], 2)

      tea <- te^(1 / (1 + year.max - year.min))
      taa <- round((tea - 1) * 100, 2)


      Sen_df[which(Sen_df$Month == i), "te"] <- te
      Sen_df[which(Sen_df$Month == i), "tag"] <- tag
      Sen_df[which(Sen_df$Month == i), "tea"] <- tea
      Sen_df[which(Sen_df$Month == i), "taa"] <- taa
    }
  } else {
    Sen_df <- apply(Sen_df, 2, median)

    Sen_df <- as.data.frame(rbind(Sen_df, Sen_df))
    Sen_df$x <- rep(c(year.min, year.max))
    Sen_df$y <- Sen_df$x * Sen_df$a + Sen_df$b

    te <- 1 + (Sen_df[2, "y"] - Sen_df[1, "y"]) / Sen_df[1, "y"]
    tag <- round(100 * (Sen_df[2, "y"] - Sen_df[1, "y"]) / Sen_df[1, "y"], 2)

    tea <- te^(1 / (1 + year.max - year.min))
    taa <- round((tea - 1) * 100, 2)


    Sen_df[, "te"] <- te
    Sen_df[, "tag"] <- tag
    Sen_df[, "tea"] <- tea
    Sen_df[, "taa"] <- taa
  }  
  
  return(Sen_df)
}




