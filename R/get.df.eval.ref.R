## _________________ Infos générales
## Nom : get.df.eval.ref.R
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
#' Fonction qui crée les dataframe évaluation et référence 
#' @param Data_TS_GF Le tableau de données avec les groupes fonctionnels
#' @param GF1 Texte groupe fonctionnel 1
#' @param GF2 Texte groupe fonctionnel 2
#' @return Le dataframe avec les groupes fonctionnels
#
## ---------------------------

get.df.eval.ref <- function(Data_TS_GF, GF1, GF2) {
  
  
  # Séparation en 2 data.frames, eval et reférence
  df_eval <- Data_TS_GF[Data_TS_GF$Year >= year.max-duree.eval+1,]
  df_ref <- Data_TS_GF[Data_TS_GF$Year < year.max-duree.eval+1,]
  
  # mise en forme
  df_ref2 <- data.frame()
  for(y in unique(df_ref$Year)){
    for(m in unique(df_ref$Month)){
      for(s in unique(df_ref$Site)) {
        tmp <- df_ref[df_ref$Year == y & df_ref$Month == m & df_ref$Site == s,]
        
        if(nrow(tmp) > 1) {
          out <- data.frame(Year = y,
                            Month = m, 
                            Site = s,
                            GF1 = tmp[tmp$Param == GF1, "fill"],
                            GF2 = tmp[tmp$Param == GF2, "fill"],
                            Anomalies.GF1 = tmp[tmp$Param == GF1, "Anomalies"],
                            Anomalies.GF2 = tmp[tmp$Param == GF2, "Anomalies"])
        }else{
          out <- data.frame(Year = y,
                            Month = m, 
                            Site = s,
                            GF1 = NA,
                            GF2 = NA,
                            Anomalies.GF1 = NA,
                            Anomalies.GF2 = NA)
        }
       
        
        df_ref2 <- rbind(df_ref2, out)
      }
    }
  }
  
  df_eval2 <- data.frame()
  for(y in unique(df_eval$Year)){
    for(m in unique(df_eval$Month)){
      for(s in unique(df_eval$Site)) {
        tmp <- df_eval[df_eval$Year == y & df_eval$Month == m & df_eval$Site == s,]
       
        if(nrow(tmp) > 1) {
          out <- data.frame(Year = y,
                            Month = m, 
                            Site = s,
                            GF1 = tmp[tmp$Param == GF1, "fill"],
                            GF2 = tmp[tmp$Param == GF2, "fill"],
                            Anomalies.GF1 = tmp[tmp$Param == GF1, "Anomalies"],
                            Anomalies.GF2 = tmp[tmp$Param == GF2, "Anomalies"])
        }else{
          out <- data.frame(Year = y,
                            Month = m, 
                            Site = s,
                            GF1 = NA,
                            GF2 = NA,
                            Anomalies.GF1 = NA,
                            Anomalies.GF2 = NA)
        }
        
        df_eval2 <- rbind(df_eval2, out)
      }
    }
  }
  
  # Si jamais il y a des NA, je les enlève afin d'éviter les problèmes
  df_eval2 <- na.omit(df_eval2)
  df_ref2 <- na.omit(df_ref2)
  
  return(list(eval=df_eval2, ref=df_ref2))
}
