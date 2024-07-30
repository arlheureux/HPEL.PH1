## _________________ Infos générales
## Nom : treat.outliers.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 21 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction qui détecte et remplace les outliers par NA
#' @param data --> série temporelle
#' @param col.param --> colonne du tableau contenant les paramÃƒÂ¨tres bio
#' @return data avec NA a la place des outliers
#
## ---------------------------


treat.outliers.NA <- function(data, col.param) {
  
  for(i in col.param){
    if(!colnames(data)[i] %in% c("Lutein", "Neo", "Prasino")){
      # J'applique un traitement log afin de limiter le nombre d'outliers
      outliers <- boxplot(log10(1+data[,i]))
      data[which(log10(1+data[,i]) %in% outliers$out),i] <- NA
    }
  }
  
  return(data)
}







