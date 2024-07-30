## _________________ Infos générales
## Nom : get.quantile.ano
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
#' Fonction qui récupÃƒÆ’Ã‚Â¨re les quantiles des anomalies
#' @param x Anomalies de la prériode de référence
#' @param xsup Anomalies de la période d'évaluation
#' @param na.rm Logique : enlever les NA
#' @return Les seuils d'anomalies
#
## ---------------------------


get.quantile.ano <- function(x, xsup, na.rm = T){
  
  
  LCS <- quantile(x, c(0.975), na.rm = na.rm)
  LCI <- quantile(x, c(0.025), na.rm = na.rm)
  LSS <- quantile(x, c(0.75), na.rm = na.rm)
  LSI <- quantile(x, c(0.25), na.rm = na.rm)
  lim_sup <- quantile(x, c(1), na.rm = na.rm)
  lim_inf <- quantile(x, c(0), na.rm = na.rm)
  
  lim_sup_ext <- quantile(xsup, c(1), na.rm = na.rm)
  lim_inf_ext <- quantile(xsup, c(0), na.rm = na.rm)
  
  res <- data.frame(Limite = c("LCS", "LCI", "LSS", "LSI", "lim_sup", "lim_inf", "lim_sup_ext", "lim_inf_ext"),
                    Val = c(LCS, LCI, LSS, LSI, lim_sup, lim_inf, lim_sup_ext, lim_inf_ext))
  
  return(res)
  
}



## _________________ Notes
#
#' Fonction qui récupÃƒÆ’Ã‚Â¨re les fréquences des anomalies
#' @param Anomalies.R Anomalies de la prériode de référence
#' @param Anomalies.E Anomalies de la période d'évaluation
#' @param quantiles quantiles issus de get.quantile.ano
#' @return Les fréquences d'anomalies
#
## ---------------------------
get.anomalies.freq <- function(Anomalies.R, Anomalies.E, quantiles) {
  
  lim_sup <- quantiles[quantiles$Limite == "lim_sup","Val"]
  LCS <- quantiles[quantiles$Limite == "LCS","Val"]
  LSS <- quantiles[quantiles$Limite == "LSS","Val"]
  LSI <- quantiles[quantiles$Limite == "LSI","Val"]
  LCI <- quantiles[quantiles$Limite == "LCI","Val"]
  lim_inf <- quantiles[quantiles$Limite == "lim_inf","Val"]
  
  lim_sup_ext <- quantiles[quantiles$Limite == "lim_sup_ext","Val"]
  lim_inf_ext <- quantiles[quantiles$Limite == "lim_inf_ext","Val"]
  
  
  Ano.faibles.E <- nrow(Anomalies.E[Anomalies.E$Ano < LSS & Anomalies.E$Ano > LSI,]) / nrow(Anomalies.E)
  Ano.fortes.E <- nrow(Anomalies.E[Anomalies.E$Ano > LCS | Anomalies.E$Ano < LCI,]) / nrow(Anomalies.E)
  Ano.inter.E <- 1 - Ano.faibles.E - Ano.fortes.E
  
  Ano.faibles.R <- nrow(Anomalies.R[Anomalies.R$Ano < LSS & Anomalies.R$Ano > LSI,]) / nrow(Anomalies.R)
  Ano.fortes.R <- nrow(Anomalies.R[Anomalies.R$Ano > LCS | Anomalies.R$Ano < LCI,]) / nrow(Anomalies.R)
  Ano.inter.R <- 1 - Ano.faibles.R - Ano.fortes.R
  
  
  res <- data.frame(Ref = c(Ano.faibles.R, Ano.inter.R, Ano.fortes.R),
                    Eval = c(Ano.faibles.E, Ano.inter.E, Ano.fortes.E))
  
  res <- 100*res
  
  return(res)
    
}

## _________________ Notes
#
#' Fonction qui récupÃƒÆ’Ã‚Â¨re les valeurs des anomalies extremes
#' @param Anomalies.R Anomalies de la prériode de référence
#' @param Anomalies.E Anomalies de la période d'évaluation
#' @param quantiles quantiles issus de get.quantile.ano
#' @return Les anomalies extremes
#
## ---------------------------
get.anomalies.extreme.dates <- function(Anomalies.R, Anomalies.E, quantiles) {
  
  lim_sup <- quantiles[quantiles$Limite == "lim_sup","Val"]
  LCS <- quantiles[quantiles$Limite == "LCS","Val"]
  LSS <- quantiles[quantiles$Limite == "LSS","Val"]
  LSI <- quantiles[quantiles$Limite == "LSI","Val"]
  LCI <- quantiles[quantiles$Limite == "LCI","Val"]
  lim_inf <- quantiles[quantiles$Limite == "lim_inf","Val"]
  
  lim_sup_ext <- quantiles[quantiles$Limite == "lim_sup_ext","Val"]
  lim_inf_ext <- quantiles[quantiles$Limite == "lim_inf_ext","Val"]
  
  
  Ano.fortes.E <- Anomalies.E[Anomalies.E$Ano > LCS | Anomalies.E$Ano < LCI,]
  Ano.fortes.R <- Anomalies.R[Anomalies.R$Ano > LCS | Anomalies.R$Ano < LCI,]

  res <- rbind(Ano.fortes.E, Ano.fortes.R)
  
  return(res)
  
}



## _________________ Notes
#
#' Fonction qui récupÃƒÆ’Ã‚Â¨re les fréquences d'anomalies annuelles
#' @param Anomalies.R Anomalies de la prériode de référence
#' @param Anomalies.E Anomalies de la période d'évaluation
#' @param quantiles quantiles issus de get.quantile.ano
#' @return Les fréquences d'anomalies annuelles
#
## ---------------------------
get.anomalies.freq.an <- function(Anomalies.R, Anomalies.E, quantiles) {
  
  lim_sup <- quantiles[quantiles$Limite == "lim_sup","Val"]
  LCS <- quantiles[quantiles$Limite == "LCS","Val"]
  LSS <- quantiles[quantiles$Limite == "LSS","Val"]
  LSI <- quantiles[quantiles$Limite == "LSI","Val"]
  LCI <- quantiles[quantiles$Limite == "LCI","Val"]
  lim_inf <- quantiles[quantiles$Limite == "lim_inf","Val"]
  
  lim_sup_ext <- quantiles[quantiles$Limite == "lim_sup_ext","Val"]
  lim_inf_ext <- quantiles[quantiles$Limite == "lim_inf_ext","Val"]
  
  

  ANO.E <- 
    do.call(
      'rbind',
      lapply(
        unique(Anomalies.E$Year),
        function(y) {
          tmp.E <- Anomalies.E[Anomalies.E$Year == y,]
          Ano.faibles.E <- nrow(tmp.E[tmp.E$Ano < LSS & tmp.E$Ano > LSI,]) / nrow(tmp.E)
          Ano.fortes.E <- nrow(tmp.E[tmp.E$Ano > LCS | tmp.E$Ano < LCI,]) / nrow(tmp.E)
          Ano.inter.E <- 1 - Ano.faibles.E - Ano.fortes.E
          
          out <- data.frame(Year = y, 
                            Param = unique(tmp.E$Param),
                            Ano.faibles = Ano.faibles.E, 
                            Ano.fortes = Ano.fortes.E, 
                            Ano.inter = Ano.inter.E)
          return(out)
        }
      )
    )
  
  
  ANO.R <- 
    do.call(
      'rbind',
      lapply(
        unique(Anomalies.R$Year),
        function(y) {
          tmp.R <- Anomalies.R[Anomalies.R$Year == y,]
          Ano.faibles.R <- nrow(tmp.R[tmp.R$Ano < LSS & tmp.R$Ano > LSI,]) / nrow(tmp.R)
          Ano.fortes.R <- nrow(tmp.R[tmp.R$Ano > LCS | tmp.R$Ano < LCI,]) / nrow(tmp.R)
          Ano.inter.R <- 1 - Ano.faibles.R - Ano.fortes.R
          
          out <- data.frame(Year = y, 
                            Param = unique(tmp.R$Param),
                            Ano.faibles = Ano.faibles.R, 
                            Ano.fortes = Ano.fortes.R, 
                            Ano.inter = Ano.inter.R)
          return(out)
        }
      )
    )
  
  
  res <- rbind(ANO.E, ANO.R)
  
  res[,3:5] <- 100*res[3:5]
  
  return(res)
  
}

