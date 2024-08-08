## _________________ Infos générales
## Nom : get.anomalies.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 24 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction qui fait les plots de mann kendall saisonniers
#' @param data Tableau de données brutes
#' @param year.max Année max de la TS
#' @param duree.eval Durée de la période d'évaluation en année
#' @param Ref_or_TS Choose if anomalies are to be calculated on whole time series (TS) or only on reference period (Ref)
#' @return Les valeurs des anomalies
#' @export
#
## ---------------------------

get.anomalies <- function(data, year.max, duree.eval, Ref_or_TS) {
  
  Anomalies.RR <- data.frame()
  Anomalies.EE <- data.frame()
  quantiles2 <- data.frame()
  Freq.ano2 <- data.frame()
  Freq.ano.an2 <- data.frame()
  chi22 <- list()
  
  for (x in unique(data$Param)) {
    for (s in unique(data[data$Param == x, 'Site'])) {
      
      tmp2 <- data[data$Param == x & data$Site == s, ]

      # je scinde en 2 les périodes éval et ref
      Anomalies.R <- tmp2[tmp2$Year < year.max - duree.eval + 1, ]
      Anomalies.E <- tmp2[tmp2$Year >= year.max - duree.eval + 1, ]
      
      # Je récupère les anomalies de ref et eval basées sur la moyenne de la TS
      if(Ref_or_TS == "TS") {
        Anomalies.R$Ano <- Anomalies.R$fill - mean(tmp2$fill)
        Anomalies.E$Ano <- Anomalies.E$fill - mean(tmp2$fill)
      }else{
        Anomalies.R$Ano <- Anomalies.R$fill - mean(Anomalies.R$fill)
        Anomalies.E$Ano <- Anomalies.E$fill - mean(Anomalies.R$fill)
      }
      
      
      
      
      # Si plus de deux ans de données dans les deux tableaux
      #if (nrow(Anomalies.E) > 24 & nrow(Anomalies.R) > 24) {
      # je récupère les quantiles et les fréquences associées
      # quantiles sur toutes la TS, pas que sur la période de ref
      if(Ref_or_TS == "TS") {
        quantiles <- get.quantile.ano(x = c(Anomalies.R$Ano, Anomalies.E$Ano), xsup = Anomalies.E$Ano)
      }else{
        quantiles <- get.quantile.ano(x = Anomalies.R$Ano, xsup = Anomalies.E$Ano)
      }
      
      
      Freq.ano <- get.anomalies.freq(Anomalies.R, Anomalies.E, quantiles)
      Freq.ano.an <- get.anomalies.freq.an(Anomalies.R, Anomalies.E, quantiles)
      
      # Test de contingence du chi2
      chi2 <- get.chi2(Freq.ano[,1:2], simulate.p.value = TRUE, B = 1000)
      chi2 <- setNames(object = chi2, nm = paste(names(chi2), x, s, sep = "_"))
      
      quantiles$Site <- s
      quantiles$Param <- x
      
      Freq.ano$Site <- s
      Freq.ano$Param <- x
      
      Freq.ano.an$Site <- s
      Freq.ano.an$Param <- x
      #}
      
      Anomalies.RR <- rbind(Anomalies.RR, Anomalies.R)
      Anomalies.EE <- rbind(Anomalies.EE, Anomalies.E)
      quantiles2 <- rbind(quantiles2, quantiles)
      Freq.ano2 <- rbind(Freq.ano2, Freq.ano)
      Freq.ano.an2 <- rbind(Freq.ano.an2, Freq.ano.an)
      chi22 <- c(chi22, chi2)
      
    }
  }
  
  quantiles2 <- quantiles2[!duplicated(quantiles2),]
  Freq.ano2 <- Freq.ano2[!duplicated(Freq.ano2),]
  
  return(
    list(
      Anomalies.R = Anomalies.RR,
      Anomalies.E = Anomalies.EE,
      quantiles = quantiles2,
      Freq.ano = Freq.ano2,
      Freq.ano.an = Freq.ano.an2,
      chi2 = chi22
    )
  )
}
