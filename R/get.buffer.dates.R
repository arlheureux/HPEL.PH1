## _________________ Infos générales
## Nom : get.buffer.dates.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 20 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction qui récupère les dates autours du mois ciblé pour la moyenne intégrée sur les jours juliens
#' @param tm Numérique : mois ciblé
#' @param ty Numérique : année ciblé
#' @param cl Un calendrier issu de la fonction get.calendar()
#' @return Le tableau de données des fractions et biomasses CHLA issues des pigments
#
## ---------------------------


get.buffer.dates <- function(tm, ty, cl){
  # tm = targetted month
  TM <- c(tm-1,tm,tm+1)
  TY <- ty
  
  CL <- cl[cl$Annee == TY & cl$Mois %in% TM,"Date_decimale"]
  
  
  if(tm == 12){
    TM <- c(11,12,1)
    TY <- c(ty, ty+1)
    
    CL1 <- cl[cl$Annee %in% TY[1] & cl$Mois %in% TM[1:2], "Date_decimale"]
    CL2 <- cl[cl$Annee %in% TY[2] & cl$Mois %in% TM[3], "Date_decimale"]
    CL <- c(CL1, CL2)
  }
  
  if(tm == 1){
    TM <- c(12,1,2)
    TY <- c(ty-1, ty)
    
    CL1 <- cl[cl$Annee %in% TY[1] & cl$Mois %in% TM[1], "Date_decimale"]
    CL2 <- cl[cl$Annee %in% TY[2] & cl$Mois %in% TM[2:3], "Date_decimale"]
    CL <- c(CL1, CL2)
  }
  
  out <- CL
  
  return(out)
}