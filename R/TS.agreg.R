## _________________ Infos générales
## Nom : TS.agreg.R
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
#' Fonction mère de l'agrégation temporelles par moyenne normale
#' @param Data_TS Les données
#' @param agg.func Texte : fonction pour aggréger
#' @param year.min Numérique : Année min
#' @param year.max Numérique : Année max
#' @param season Numérique : Numéro de saisons. Par défaut 1:12
#' @return Les données agrégées
#
## ---------------------------

TS.agreg <- function(Data_TS, agg.func, year.min, year.max, season){
  
  cat("Aggrégation des séries en utilisant la fonction :", agg.func,"\n")
  
  Data_TS_month <- do.call(
    'rbind',
    pbmcapply::pbmclapply(
      year.min:year.max,
      function(y) {
        tmp01 <- Data_TS[Data_TS$Year == y,]
        
        out01 <- do.call(
          "rbind",
          lapply(
            unique(season),
            function(m) {
              tmp02 <- tmp01[tmp01$Month == m,]
              
              OUT02 <- data.frame()
              for(p in unique(Data_TS$Param)){
                for(s in unique(Data_TS$Site)) {
                  
                  tmp03 <- tmp02[tmp02$Param == p & tmp02$Site == s,]
                  
                  if(nrow(tmp03) >= 1) {
                    out02 <- data.frame(
                      YearTS = decimal_date(as.Date(paste(y, m, "15", sep ="-"))),
                      Year = y,
                      Month = m,
                      Site = s,
                      Param = p,
                      Val = mean(tmp03$Val, na.rm = TRUE)
                    )
                  }else{
                    out02 <- data.frame(
                      YearTS = decimal_date(as.Date(paste(y, m, "15", sep ="-"))),
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
